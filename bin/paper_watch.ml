type decision = Undecided | Keep | Skip

type paper = {
  id : string;
  doi : string;
  title : string;
  authors : string;
  conference : string;
  year : string;
  abstract_en : string;
  abstract_ja : string;
  mutable reviewed : bool;
  mutable decision : decision;
}

let decision_of_string = function
  | "keep" -> Keep
  | "skip" -> Skip
  | _ -> Undecided

let string_of_decision = function
  | Undecided -> "undecided"
  | Keep -> "keep"
  | Skip -> "skip"

let trim_newline s =
  let n = String.length s in
  if n > 0 && s.[n - 1] = '\r' then String.sub s 0 (n - 1) else s

let split_tsv line = String.split_on_char '\t' line

let sql_quote s = "'" ^ String.concat "''" (String.split_on_char '\'' s) ^ "'"

let check_rc context rc =
  if rc <> Sqlite3.Rc.OK && rc <> Sqlite3.Rc.DONE && rc <> Sqlite3.Rc.ROW then
    failwith (context ^ ": " ^ Sqlite3.Rc.to_string rc)

let exec db sql = check_rc "SQLite" (Sqlite3.exec db sql)

let rec mkdir_p path =
  if path = "" || path = "." || path = "/" then ()
  else if Sys.file_exists path then ()
  else (
    mkdir_p (Filename.dirname path);
    try Unix.mkdir path 0o700 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  )

let home_dir () = match Sys.getenv_opt "HOME" with Some h -> h | None -> "."

let default_db_path () =
  match Sys.getenv_opt "PAPER_WATCH_DB" with
  | Some p -> p
  | None -> Filename.concat (home_dir ()) ".local/state/paper-watch/papers.sqlite3"

let legacy_state_path () =
  match Sys.getenv_opt "PAPER_WATCH_STATE" with
  | Some p -> p
  | None -> Filename.concat (home_dir ()) ".local/state/paper-watch/reviews.tsv"

let init_schema db =
  exec db "PRAGMA foreign_keys = ON";
  exec db "PRAGMA journal_mode = WAL";
  exec db "CREATE TABLE IF NOT EXISTS papers (\
    id TEXT PRIMARY KEY, doi TEXT UNIQUE, title TEXT NOT NULL, authors TEXT NOT NULL DEFAULT '',\
    conference TEXT NOT NULL DEFAULT '', year TEXT NOT NULL DEFAULT '',\
    abstract_en TEXT NOT NULL DEFAULT '', abstract_ja TEXT NOT NULL DEFAULT '',\
    metadata_source TEXT, abstract_source TEXT, metadata_updated_at TEXT, created_at TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP\
  )";
  exec db "CREATE TABLE IF NOT EXISTS reviews (\
    paper_id TEXT PRIMARY KEY REFERENCES papers(id) ON DELETE CASCADE,\
    reviewed_at TEXT, decision TEXT NOT NULL DEFAULT 'undecided'\
      CHECK (decision IN ('undecided','keep','skip')), updated_at TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP\
  )";
  exec db "CREATE INDEX IF NOT EXISTS papers_conference_year_idx ON papers(conference, year)";
  exec db "CREATE INDEX IF NOT EXISTS reviews_decision_idx ON reviews(decision, reviewed_at)"

let scalar_int db sql =
  let stmt = Sqlite3.prepare db sql in
  let result =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW -> Sqlite3.column_int stmt 0
    | _ -> 0
  in
  ignore (Sqlite3.finalize stmt);
  result

let now_iso () =
  let tm = Unix.gmtime (Unix.gettimeofday ()) in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
    (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
    tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec

let import_tsv db path =
  let ic = open_in path in
  let imported = ref 0 in
  let rec loop () =
    match input_line ic with
    | line ->
        let line = trim_newline line in
        if line = "" || line.[0] = '#' || String.length line >= 2 && String.sub line 0 2 = "id" then loop ()
        else (match split_tsv line with
          | id :: title :: authors :: conference :: year :: abstract_en :: rest ->
              let abstract_ja = match rest with x :: _ -> x | [] -> "" in
              let doi =
                if String.length id > 4 && String.sub id 0 4 = "doi:" then String.sub id 4 (String.length id - 4)
                else ""
              in
              exec db (Printf.sprintf
                "INSERT OR IGNORE INTO papers(id,doi,title,authors,conference,year,abstract_en,abstract_ja,metadata_source,abstract_source) VALUES(%s,%s,%s,%s,%s,%s,%s,%s,'tsv','tsv')"
                (sql_quote id) (if doi = "" then "NULL" else sql_quote doi)
                (sql_quote title) (sql_quote authors) (sql_quote conference)
                (sql_quote year) (sql_quote abstract_en) (sql_quote abstract_ja));
              exec db (Printf.sprintf "INSERT OR IGNORE INTO reviews(paper_id) VALUES(%s)" (sql_quote id));
              incr imported;
              loop ()
          | _ -> prerr_endline ("Skipping malformed TSV line: " ^ line); loop ())
    | exception End_of_file -> close_in ic
  in
  loop ();
  !imported

let migrate_legacy_state db path =
  if Sys.file_exists path then (
    let ic = open_in path in
    let rec loop () = match input_line ic with
      | line -> (match split_tsv (trim_newline line) with
        | id :: reviewed :: decision :: _ ->
            let reviewed_at = if reviewed = "1" then sql_quote (now_iso ()) else "NULL" in
            exec db (Printf.sprintf
              "INSERT INTO reviews(paper_id,reviewed_at,decision) SELECT %s,%s,%s WHERE EXISTS(SELECT 1 FROM papers WHERE id=%s) ON CONFLICT(paper_id) DO NOTHING"
              (sql_quote id) reviewed_at (sql_quote (string_of_decision (decision_of_string decision))) (sql_quote id))
        | _ -> ()); loop ()
      | exception End_of_file -> close_in ic
    in loop ()
  )

let ensure_seed_data db tsv_path =
  if scalar_int db "SELECT COUNT(*) FROM papers" = 0 && Sys.file_exists tsv_path then (
    let n = import_tsv db tsv_path in
    if n > 0 then migrate_legacy_state db (legacy_state_path ());
    Printf.eprintf "Imported %d sample paper(s) into SQLite.\n%!" n
  )

let text_column stmt i =
  match Sqlite3.column stmt i with
  | Sqlite3.Data.TEXT s -> s
  | Sqlite3.Data.INT n -> Int64.to_string n
  | Sqlite3.Data.FLOAT f -> string_of_float f
  | Sqlite3.Data.NULL | Sqlite3.Data.BLOB _ | Sqlite3.Data.NONE -> ""

let load_papers db =
  let stmt = Sqlite3.prepare db
    "SELECT p.id,COALESCE(p.doi,''),p.title,p.authors,p.conference,p.year,p.abstract_en,p.abstract_ja, \
     CASE WHEN r.reviewed_at IS NULL THEN 0 ELSE 1 END,r.decision \
     FROM papers p LEFT JOIN reviews r ON r.paper_id=p.id \
     ORDER BY p.conference,p.year DESC,p.title" in
  let rec loop acc = match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW ->
        let reviewed = Sqlite3.column_int stmt 8 <> 0 in
        let p = {
          id=text_column stmt 0; doi=text_column stmt 1; title=text_column stmt 2;
          authors=text_column stmt 3; conference=text_column stmt 4; year=text_column stmt 5;
          abstract_en=text_column stmt 6; abstract_ja=text_column stmt 7;
          reviewed; decision=decision_of_string (text_column stmt 9)
        } in loop (p :: acc)
    | Sqlite3.Rc.DONE -> List.rev acc
    | rc -> check_rc "Loading papers" rc; List.rev acc
  in
  let papers = loop [] in
  ignore (Sqlite3.finalize stmt);
  papers

let save_state db p =
  let reviewed_at = if p.reviewed then sql_quote (now_iso ()) else "NULL" in
  exec db (Printf.sprintf
    "INSERT INTO reviews(paper_id,reviewed_at,decision,updated_at) VALUES(%s,%s,%s,CURRENT_TIMESTAMP) \
     ON CONFLICT(paper_id) DO UPDATE SET reviewed_at=excluded.reviewed_at,decision=excluded.decision,updated_at=CURRENT_TIMESTAMP"
    (sql_quote p.id) reviewed_at (sql_quote (string_of_decision p.decision)))

let status_label p =
  match p.reviewed, p.decision with
  | false, Undecided -> "UNREAD"
  | true, Undecided -> "READ"
  | _, Keep -> "KEEP"
  | _, Skip -> "SKIP"

let truncate width s =
  if width <= 3 || String.length s <= width then s
  else String.sub s 0 (width - 3) ^ "..."

let clear_screen () = print_string "\027[2J\027[H"

let terminal_raw_mode () =
  if Unix.isatty Unix.stdin then (
    let original = Unix.tcgetattr Unix.stdin in
    let raw = { original with Unix.c_icanon = false; Unix.c_echo = false; Unix.c_vmin = 1; Unix.c_vtime = 0 } in
    Unix.tcsetattr Unix.stdin Unix.TCSANOW raw;
    Some original
  ) else None

let restore_terminal = function
  | Some original -> Unix.tcsetattr Unix.stdin Unix.TCSANOW original
  | None -> ()

let read_key () =
  let b = Bytes.create 1 in
  let n = Unix.read Unix.stdin b 0 1 in
  if n = 0 then 'q' else Bytes.get b 0

let render papers selected show_japanese =
  clear_screen ();
  let count = List.length papers in
  let keeps = List.fold_left (fun n p -> if p.decision = Keep then n + 1 else n) 0 papers in
  let unread = List.fold_left (fun n p -> if not p.reviewed && p.decision = Undecided then n + 1 else n) 0 papers in
  Printf.printf " PAPER-WATCH  |  %d papers  |  %d unread  |  %d keep\n" count unread keeps;
  print_endline " -------------------------------------------------------------------------------";
  print_endline " j/k: move   r: read   s: keep   x: skip   u: reset   t: translation   q: quit";
  print_endline " -------------------------------------------------------------------------------";
  List.iteri (fun i p ->
    let marker = if i = selected then ">" else " " in
    let line = Printf.sprintf "%s [%5s] %-10s %s (%s)" marker (string_of_int (i + 1)) (status_label p) p.title p.conference in
    print_endline (truncate 100 line)
  ) papers;
  print_endline " -------------------------------------------------------------------------------";
  match List.nth_opt papers selected with
  | None -> print_endline "No papers loaded."
  | Some p ->
      Printf.printf "%s — %s\n" p.conference p.year;
      Printf.printf "ID: %s\n" p.id;
      if p.doi <> "" then Printf.printf "DOI: %s\n" p.doi;
      Printf.printf "Authors: %s\n\n" p.authors;
      print_endline "ABSTRACT"; print_endline "--------";
      let abstract = if show_japanese && p.abstract_ja <> "" then p.abstract_ja else p.abstract_en in
      List.iter print_endline (String.split_on_char '\n' abstract);
      if show_japanese && p.abstract_ja = "" then print_endline "\n[No Japanese translation available.]";
      print_endline ""; Printf.printf "Current state: %s\n" (status_label p); flush stdout

let set_state db p reviewed decision =
  p.reviewed <- reviewed; p.decision <- decision; save_state db p

let main () =
  let tsv_path = if Array.length Sys.argv > 1 then Sys.argv.(1) else "papers.tsv" in
  let db_path = default_db_path () in
  mkdir_p (Filename.dirname db_path);
  let db = Sqlite3.db_open db_path in
  try
    init_schema db;
    ensure_seed_data db tsv_path;
    let papers = load_papers db in
    if papers = [] then (prerr_endline "No papers in database. Provide a TSV file on first run."; ignore (Sqlite3.db_close db); exit 0);
    let old_term = terminal_raw_mode () in
    let selected = ref 0 and show_japanese = ref false and running = ref true in
    let finish () = restore_terminal old_term; clear_screen (); print_endline "paper-watch: SQLite state saved." in
    (try
      while !running do
        render papers !selected !show_japanese;
        let key = read_key () in
        let p = List.nth papers !selected in
        (match key with
         | 'j' -> selected := min (List.length papers - 1) (!selected + 1)
         | 'k' -> selected := max 0 (!selected - 1)
         | 'r' -> set_state db p true Undecided
         | 's' -> set_state db p true Keep
         | 'x' -> set_state db p true Skip
         | 'u' -> set_state db p false Undecided
         | 't' -> show_japanese := not !show_japanese
         | 'q' -> running := false
         | _ -> ())
      done;
      finish ()
    with exn -> restore_terminal old_term; raise exn);
    ignore (Sqlite3.db_close db)
  with exn -> ignore (Sqlite3.db_close db); raise exn

let () = main ()
