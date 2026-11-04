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
  abstract_source : string;
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
  exec db "CREATE INDEX IF NOT EXISTS reviews_decision_idx ON reviews(decision, reviewed_at)";
  let ensure_column name definition =
    let stmt = Sqlite3.prepare db "PRAGMA table_info(papers)" in
    let rec has_column () = match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW ->
          let found = match Sqlite3.column stmt 1 with
            | Sqlite3.Data.TEXT s -> s = name
            | _ -> false
          in found || has_column ()
      | _ -> false
    in
    let exists = has_column () in
    ignore (Sqlite3.finalize stmt);
    if not exists then exec db ("ALTER TABLE papers ADD COLUMN " ^ name ^ " " ^ definition)
  in
  ensure_column "abstract_fetched_at" "TEXT";
  ensure_column "abstract_status" "TEXT NOT NULL DEFAULT 'pending'";
  ensure_column "abstract_error" "TEXT";
  ensure_column "official_url" "TEXT";
  ensure_column "collection_source" "TEXT";
  ensure_column "collected_at" "TEXT";
  exec db "CREATE TABLE IF NOT EXISTS doi_candidates (    paper_id TEXT NOT NULL REFERENCES papers(id) ON DELETE CASCADE,    source TEXT NOT NULL, doi TEXT NOT NULL, candidate_title TEXT NOT NULL DEFAULT '',    candidate_authors TEXT NOT NULL DEFAULT '', candidate_year TEXT NOT NULL DEFAULT '',    score REAL, exact_title INTEGER NOT NULL DEFAULT 0, retrieved_at TEXT NOT NULL,    PRIMARY KEY(paper_id,source,doi))"

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
    "SELECT p.id,COALESCE(p.doi,''),p.title,p.authors,p.conference,p.year,p.abstract_en,p.abstract_ja,COALESCE(p.abstract_source,''), \
     CASE WHEN r.reviewed_at IS NULL THEN 0 ELSE 1 END,r.decision \
     FROM papers p LEFT JOIN reviews r ON r.paper_id=p.id \
     ORDER BY p.conference,p.year DESC,p.title" in
  let rec loop acc = match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW ->
        let reviewed = Sqlite3.column_int stmt 9 <> 0 in
        let p = {
          id=text_column stmt 0; doi=text_column stmt 1; title=text_column stmt 2;
          authors=text_column stmt 3; conference=text_column stmt 4; year=text_column stmt 5;
          abstract_en=text_column stmt 6; abstract_ja=text_column stmt 7;
          abstract_source=text_column stmt 8;
          reviewed; decision=decision_of_string (text_column stmt 10)
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

let shell_quote s = "'" ^ String.concat "'\\''" (String.split_on_char '\'' s) ^ "'"

let percent_encode_path s =
  let b = Buffer.create (String.length s) in
  String.iter (fun c ->
    match c with
    | 'a'..'z' | 'A'..'Z' | '0'..'9' | '-' | '_' | '.' | '~' | '/' -> Buffer.add_char b c
    | _ -> Buffer.add_string b (Printf.sprintf "%%%02X" (Char.code c))) s;
  Buffer.contents b

let http_get_with_headers headers url =
  let header_args = String.concat " " (List.map (fun h -> "-H " ^ shell_quote h) headers) in
  let command = "curl -fsSL --connect-timeout 8 --max-time 25 " ^ header_args ^ " " ^ shell_quote url ^ " 2>/dev/null" in
  let ic = Unix.open_process_in command in
  let contents =
    let b = Buffer.create 4096 in
    (try while true do Buffer.add_string b (input_line ic); Buffer.add_char b '\n' done with End_of_file -> ());
    Buffer.contents b
  in
  match Unix.close_process_in ic with
  | Unix.WEXITED 0 -> Some contents
  | _ -> None

let http_get url = http_get_with_headers ["Accept: application/json"; "User-Agent: paper-watch/0.1 (academic literature review)"] url
let http_get_html url = http_get_with_headers ["Accept: text/html,application/xhtml+xml"; "User-Agent: Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 Chrome/120 Safari/537.36"] url

let json_field key = function
  | `Assoc fields -> (try List.assoc key fields with Not_found -> `Null)
  | _ -> `Null

let json_string = function
  | `String s -> s
  | `Int n -> string_of_int n
  | _ -> ""

let json_year = function
  | `Assoc _ as obj ->
      (match json_field "date-parts" obj with
       | `List (`List (`Int y :: _) :: _) -> string_of_int y
       | _ -> "")
  | _ -> ""

let html_to_text s =
  let b = Buffer.create (String.length s) in
  let in_tag = ref false in
  String.iter (fun c ->
    if c = '<' then in_tag := true
    else if c = '>' then (in_tag := false; Buffer.add_char b ' ')
    else if not !in_tag then Buffer.add_char b c) s;
  let s = Buffer.contents b in
  let replacements = ["&amp;", "&"; "&lt;", "<"; "&gt;", ">"; "&quot;", "\""; "&#39;", "'"] in
  let replace_all input oldv newv =
    let old_len = String.length oldv in
    let b = Buffer.create (String.length input) in
    let rec loop i =
      if i >= String.length input then ()
      else if i + old_len <= String.length input && String.sub input i old_len = oldv then
        (Buffer.add_string b newv; loop (i + old_len))
      else (Buffer.add_char b input.[i]; loop (i + 1))
    in loop 0; Buffer.contents b
  in
  let s = List.fold_left (fun acc (a,b) -> replace_all acc a b) s replacements in
  String.trim s

let crossref_record doi =
  let url = "https://api.crossref.org/works/" ^ percent_encode_path doi in
  match http_get url with
  | None -> None
  | Some body ->
      (try
        let root = Yojson.Basic.from_string body in
        let msg = json_field "message" root in
        let abstract = html_to_text (json_string (json_field "abstract" msg)) in
        let title = match json_field "title" msg with `List (`String x :: _) -> x | _ -> "" in
        let authors = match json_field "author" msg with
          | `List xs -> String.concat ", " (List.filter ((<>) "") (List.map (fun a ->
              let given = json_string (json_field "given" a) in
              let family = json_string (json_field "family" a) in
              String.trim (given ^ " " ^ family)) xs))
          | _ -> "" in
        let year = let y = json_year (json_field "published-print" msg) in
          if y <> "" then y else json_year (json_field "published-online" msg) in
        Some (abstract, title, authors, year)
       with _ -> None)

let semantic_scholar_record doi =
  let paper_id = "DOI:" ^ doi in
  let url = "https://api.semanticscholar.org/graph/v1/paper/" ^
    percent_encode_path paper_id ^ "?fields=title,abstract,authors,year" in
  match http_get url with
  | None -> None
  | Some body ->
      (try
        let obj = Yojson.Basic.from_string body in
        let abstract = json_string (json_field "abstract" obj) in
        let title = json_string (json_field "title" obj) in
        let authors = match json_field "authors" obj with
          | `List xs -> String.concat ", " (List.filter ((<>) "") (List.map (fun a -> json_string (json_field "name" a)) xs))
          | _ -> "" in
        let year = json_string (json_field "year" obj) in
        Some (abstract, title, authors, year)
       with _ -> None)

let openalex_record doi =
  let url = "https://api.openalex.org/works/https://doi.org/" ^ percent_encode_path doi in
  match http_get url with
  | None -> None
  | Some body ->
      (try
        let obj = Yojson.Basic.from_string body in
        let inverted = json_field "abstract_inverted_index" obj in
        let words = match inverted with
          | `Assoc entries ->
              List.concat (List.map (fun (word, positions) -> match positions with
                | `List ps -> List.filter_map (function `Int n -> Some (n, word) | _ -> None) ps
                | _ -> []) entries)
          | _ -> [] in
        let abstract = words |> List.sort (fun (a,_) (b,_) -> compare a b)
          |> List.map snd |> String.concat " " in
        let title = json_string (json_field "title" obj) in
        let year = json_string (json_field "publication_year" obj) in
        Some (abstract, title, "", year)
       with _ -> None)

let html_attribute tag attr =
  let lower = String.lowercase_ascii tag in
  let needle = String.lowercase_ascii attr ^ "=" in
  let rec find i =
    if i + String.length needle > String.length lower then ""
    else if String.sub lower i (String.length needle) = needle then (
      let start = i + String.length needle in
      if start >= String.length tag then "" else
      let quote = tag.[start] in
      if quote = '\'' || quote = '"' then (
        try let stop = String.index_from tag (start + 1) quote in String.sub tag (start + 1) (stop - start - 1)
        with Not_found -> "")
      else
        let stop = try String.index_from tag start ' ' with Not_found -> String.length tag in
        String.sub tag start (stop - start))
    else find (i + 1)
  in find 0

let acm_record doi =
  let url = "https://dl.acm.org/doi/" ^ percent_encode_path doi in
  match http_get_html url with
  | None -> None
  | Some html ->
      let rec tags i acc =
        try
          let start = String.index_from html i '<' in
          let stop = String.index_from html start '>' in
          let tag = String.sub html start (stop - start + 1) in
          let acc = if String.length tag >= 4 && String.lowercase_ascii (String.sub tag 0 4) = "<met" then
            let name = String.lowercase_ascii (html_attribute tag "name") in
            let prop = String.lowercase_ascii (html_attribute tag "property") in
            let content = html_attribute tag "content" in
            if name = "citation_abstract" || name = "dc.description" || prop = "og:description" then content :: acc else acc
            else acc in
          tags (stop + 1) acc
        with Not_found -> List.rev acc
      in
      let abstracts = tags 0 [] in
      let abstract = match abstracts with x :: _ -> html_to_text x | [] -> "" in
      if abstract = "" then None else Some (abstract, "", "", "")

let update_paper_metadata db id source abstract title authors year =
  let stamp = sql_quote (now_iso ()) in
  exec db (Printf.sprintf
    "UPDATE papers SET title=CASE WHEN %s='' THEN title ELSE %s END,authors=CASE WHEN %s='' THEN authors ELSE %s END,year=CASE WHEN %s='' THEN year ELSE %s END,metadata_source=%s,metadata_updated_at=%s,abstract_en=CASE WHEN %s='' THEN abstract_en ELSE %s END,abstract_source=CASE WHEN %s='' THEN abstract_source ELSE %s END,abstract_fetched_at=%s,abstract_status=%s,abstract_error=NULL WHERE id=%s"
    (sql_quote title) (sql_quote title) (sql_quote authors) (sql_quote authors)
    (sql_quote year) (sql_quote year) (sql_quote source) stamp
    (sql_quote abstract) (sql_quote abstract) (sql_quote abstract) (sql_quote source)
    stamp (sql_quote (if abstract = "" then "not_found" else "success")) (sql_quote id))

let fetch_abstracts db =
  let stmt = Sqlite3.prepare db
    "SELECT id,doi FROM papers WHERE doi IS NOT NULL AND doi<>'' AND abstract_en='' AND abstract_status IN ('pending','error','not_found') ORDER BY conference,year,title" in
  let rec collect acc = match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW -> collect ((text_column stmt 0, text_column stmt 1) :: acc)
    | Sqlite3.Rc.DONE -> List.rev acc
    | rc -> check_rc "Selecting papers to fetch" rc; List.rev acc
  in
  let rows = collect [] in
  ignore (Sqlite3.finalize stmt);
  let ok = ref 0 and missing = ref 0 in
  List.iter (fun (id, doi) ->
    Printf.printf "Fetching %s ... %!" doi;
    let crossref = crossref_record doi in
    let result =
      let candidates = [
        ("crossref", (fun () -> crossref));
        ("semantic_scholar", (fun () -> semantic_scholar_record doi));
        ("openalex", (fun () -> openalex_record doi));
        ("acm", (fun () -> acm_record doi))
      ] in
      let rec choose = function
        | [] -> None
        | (source, fetch) :: rest ->
            (match fetch () with
             | Some (abstract, title, authors, year) when abstract <> "" ->
                 let fallback = match crossref with Some (_, t, a, y) -> (t,a,y) | None -> ("","","") in
                 let ft, fa, fy = fallback in
                 Some (source, abstract,
                   (if title <> "" then title else ft),
                   (if authors <> "" then authors else fa),
                   (if year <> "" then year else fy))
             | _ -> choose rest)
      in
      choose candidates
    in
    match result with
    | Some (source, abstract, title, authors, year) ->
        update_paper_metadata db id source abstract title authors year;
        incr ok; Printf.printf " %s (%d chars)\n%!" source (String.length abstract)
    | None ->
        exec db (Printf.sprintf "UPDATE papers SET abstract_status='not_found',abstract_fetched_at=%s,abstract_error='No abstract returned by Crossref, Semantic Scholar, OpenAlex, or ACM' WHERE id=%s"
          (sql_quote (now_iso ())) (sql_quote id));
        incr missing; print_endline " not found"
  ) rows;
  Printf.printf "Abstract fetch complete: %d fetched, %d unavailable, %d checked.\n%!" !ok !missing (List.length rows)

let import_dois db path =
  let ic = open_in path in
  let added = ref 0 in
  (try while true do
    let line = String.trim (input_line ic) in
    if line <> "" && line.[0] <> '#' then (
      let doi = if String.length line >= 16 && String.sub line 0 16 = "https://doi.org/" then String.sub line 16 (String.length line - 16) else line in
      let id = "doi:" ^ doi in
      exec db (Printf.sprintf
        "INSERT OR IGNORE INTO papers(id,doi,title,authors,conference,year,abstract_status) VALUES(%s,%s,%s,'','','','pending')"
        (sql_quote id) (sql_quote doi) (sql_quote ("[Metadata pending] " ^ doi)));
      let inserted = Sqlite3.changes db > 0 in
      exec db (Printf.sprintf "INSERT OR IGNORE INTO reviews(paper_id) VALUES(%s)" (sql_quote id));
      if inserted then incr added
    )
  done with End_of_file -> close_in ic);
  Printf.printf "Imported %d DOI(s) from %s.\n%!" !added path


let html_unescape s =
  let replacements = ["&amp;", "&"; "&lt;", "<"; "&gt;", ">"; "&quot;", "\""; "&#39;", "'"; "&#x27;", "'"] in
  List.fold_left (fun acc (a,b) ->
    let re = Str.regexp_string a in Str.global_replace re b acc) s replacements

let strip_html s =
  let s = Str.global_replace (Str.regexp "<[^>]*>") " " s in
  let s = html_unescape s in
  Str.global_replace (Str.regexp "[ \t\r\n]+") " " s |> String.trim

let attr_value tag name =
  let re = Str.regexp_case_fold (name ^ "[ \t]*=[ \t]*[\"']\\([^\"']+\\)[\"']") in
  try ignore (Str.search_forward re tag 0); Str.matched_group 1 tag with _ -> ""

let parse_usenix_papers html =
  (* Keep the opening position as well as the end of each heading.  The
     following section must start at the NEXT heading's opening tag, not
     after that heading; otherwise its title is appended to this abstract. *)
  let rec headings pos acc =
    try
      let heading_start = Str.search_forward (Str.regexp_case_fold "<h2[^>]*>") html pos in
      let open_end = Str.match_end () in
      let close = Str.search_forward (Str.regexp_case_fold "</h2>") html open_end in
      let heading = String.sub html open_end (close-open_end) in
      let body_start = Str.match_end () in
      let anchor_re = Str.regexp_case_fold "<a[^>]*>\\([^<]*\\)</a>" in
      let href_re = Str.regexp_case_fold "<a[^>]*>" in
      let title = try ignore (Str.search_forward anchor_re heading 0); Str.matched_group 1 heading |> strip_html with _ -> "" in
      let href = try ignore (Str.search_forward href_re heading 0); attr_value (Str.matched_string heading) "href" with _ -> "" in
      let is_presentation =
        try ignore (Str.search_forward (Str.regexp_string "/presentation/") href 0); true
        with Not_found -> false
      in
      let is_non_paper_presentation =
        let path = String.lowercase_ascii href |> fun p -> List.hd (String.split_on_char '?' p) in
        Filename.basename path = "keynote"
      in
      let acc = if title <> "" && is_presentation then (heading_start,body_start,title,href,not is_non_paper_presentation)::acc else acc in
      headings body_start acc
    with Not_found -> List.rev acc
  in
  let hs = headings 0 [] in
  let rec make = function
    | [] -> []
    | (_,body_start,title,href,is_paper)::rest ->
        let body_end = match rest with (next_heading,_,_,_,_)::_ -> next_heading | [] -> String.length html in
        let body = String.sub html body_start (max 0 (body_end-body_start)) |> strip_html in
        let available = Str.regexp_case_fold "Available Media" in
        let authors =
          try
            let i = Str.search_forward available body 0 in
            let prefix = String.sub body 0 i |> String.trim in
            Str.global_replace (Str.regexp_case_fold "Operational Systems Paper[ \\t]*$") "" prefix |> String.trim
          with Not_found -> ""
        in
        let abstract =
          try
            ignore (Str.search_forward available body 0);
            let tail = String.sub body (Str.match_end ()) (String.length body - Str.match_end ()) |> String.trim in
            let tail = Str.global_replace (Str.regexp "^Operational Systems Paper[ \\t]*") "" tail in
            String.trim tail
          with Not_found -> ""
        in
        let absolute = if String.length href > 0 && href.[0] = '/' then "https://www.usenix.org" ^ href else href in
        let current = if is_paper then [(title, authors, abstract, absolute)] else [] in
        current @ make rest
  in
  make hs

let slugify s =
  let b = Buffer.create (String.length s) in
  let dash = ref false in
  String.iter (fun c ->
    if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') then
      (if !dash && Buffer.length b > 0 then Buffer.add_char b '-';
       dash := false; Buffer.add_char b (Char.lowercase_ascii c))
    else dash := true) s;
  Buffer.contents b

let collect_osdi db year =
  let short_year = if String.length year >= 2 then String.sub year (String.length year-2) 2 else year in
  let url = "https://www.usenix.org/conference/osdi" ^ short_year ^ "/technical-sessions" in
  Printf.printf "Fetching official proceedings: %s\n%!" url;
  match http_get_html url with
  | None -> failwith ("Could not fetch official USENIX page: " ^ url)
  | Some html ->
      let papers = parse_usenix_papers html in
      let inserted = ref 0 in
      List.iter (fun (title,authors,abstract,official_url) ->
        let id = "osdi:" ^ year ^ ":" ^ slugify title in
        let stamp = sql_quote (now_iso ()) in
        let doi = "NULL" in
        exec db (Printf.sprintf
          "INSERT INTO papers(id,doi,title,authors,conference,year,abstract_en,metadata_source,abstract_source,metadata_updated_at,abstract_fetched_at,abstract_status,official_url,collection_source,collected_at) VALUES(%s,%s,%s,%s,'OSDI',%s,%s,'usenix_official','usenix_official',%s,%s,%s,%s,%s,%s) ON CONFLICT(id) DO UPDATE SET title=excluded.title,authors=excluded.authors,abstract_en=CASE WHEN excluded.abstract_en<>'' THEN excluded.abstract_en ELSE papers.abstract_en END,metadata_source='usenix_official',abstract_source=CASE WHEN excluded.abstract_en<>'' THEN 'usenix_official' ELSE papers.abstract_source END,metadata_updated_at=excluded.metadata_updated_at,abstract_fetched_at=CASE WHEN excluded.abstract_en<>'' THEN excluded.abstract_fetched_at ELSE papers.abstract_fetched_at END,abstract_status=CASE WHEN excluded.abstract_en<>'' THEN 'success' ELSE papers.abstract_status END,official_url=excluded.official_url,collection_source=excluded.collection_source,collected_at=excluded.collected_at"
          (sql_quote id) doi (sql_quote title) (sql_quote authors) (sql_quote year) (sql_quote abstract)
          stamp stamp (sql_quote (if abstract = "" then "not_found" else "success"))
          (sql_quote official_url) (sql_quote url) stamp);
        if Sqlite3.changes db > 0 then incr inserted;
        exec db (Printf.sprintf "INSERT OR IGNORE INTO reviews(paper_id) VALUES(%s)" (sql_quote id))
      ) papers;
      Printf.printf "OSDI %s collection: %d candidate paper(s) parsed; %d row(s) inserted or updated.\n" year (List.length papers) !inserted;
      if papers = [] then failwith "No paper records parsed; database was not changed."

let normalize_title s =
  let s = String.lowercase_ascii s in
  let b = Buffer.create (String.length s) in
  let pending_space = ref false in
  String.iter (fun c ->
    if (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') then (
      if !pending_space && Buffer.length b > 0 then Buffer.add_char b ' ';
      pending_space := false;
      Buffer.add_char b c
    ) else pending_space := true
  ) s;
  Buffer.contents b

let percent_encode_component s =
  let b = Buffer.create (String.length s) in
  String.iter (fun c ->
    match c with
    | 'a'..'z' | 'A'..'Z' | '0'..'9' | '-' | '_' | '.' | '~' -> Buffer.add_char b c
    | _ -> Buffer.add_string b (Printf.sprintf "%%%02X" (Char.code c))) s;
  Buffer.contents b

let crossref_title_candidates title =
  let url = "https://api.crossref.org/works?query.title=" ^ percent_encode_component title ^
    "&rows=5" in
  match http_get url with
  | None -> []
  | Some body ->
      (try
        let root = Yojson.Basic.from_string body in
        let message = json_field "message" root in
        match json_field "items" message with
        | `List items -> List.filter_map (fun item ->
            let doi = json_string (json_field "DOI" item) in
            let candidate_title = match json_field "title" item with `List (`String t::_) -> t | _ -> "" in
            if doi = "" || candidate_title = "" then None else
            let candidate_authors = match json_field "author" item with
              | `List xs -> String.concat ", " (List.filter ((<>) "") (List.map (fun a ->
                  let given = json_string (json_field "given" a) in
                  let family = json_string (json_field "family" a) in
                  String.trim (given ^ " " ^ family)) xs))
              | _ -> "" in
            let candidate_year = json_year (json_field "published" item) in
            let score = match json_field "score" item with `Float f -> Some f | `Int n -> Some (float_of_int n) | _ -> None in
            Some (doi,candidate_title,candidate_authors,candidate_year,score)
          ) items
        | _ -> []
       with _ -> [])

let resolve_dois db conference year =
  let stmt = Sqlite3.prepare db
    "SELECT id,title,authors,year FROM papers WHERE conference=?1 AND year=?2 AND (doi IS NULL OR doi='') AND official_url IS NOT NULL AND official_url<>'' ORDER BY title" in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT conference));
  ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT year));
  let rec read acc = match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW -> read ((text_column stmt 0,text_column stmt 1,text_column stmt 2,text_column stmt 3)::acc)
    | Sqlite3.Rc.DONE -> List.rev acc
    | rc -> check_rc "Selecting DOI candidates" rc; List.rev acc
  in
  let papers = read [] in
  ignore (Sqlite3.finalize stmt);
  let exact_count = ref 0 and candidate_count = ref 0 and no_match = ref 0 in
  Printf.printf "Resolving missing DOIs for %s %s: %d paper(s)\n%!" conference year (List.length papers);
  List.iter (fun (id,title,authors,paper_year) ->
    Printf.printf "\n%s\n  %s\n%!" id title;
    Unix.sleepf 0.25;
    let candidates = crossref_title_candidates title in
    let exact = List.filter (fun (_,t,_,_,_) -> normalize_title t = normalize_title title) candidates in
    List.iter (fun (doi,t,a,y,score) ->
      exec db (Printf.sprintf
        "INSERT INTO doi_candidates(paper_id,source,doi,candidate_title,candidate_authors,candidate_year,score,exact_title,retrieved_at) VALUES(%s,'crossref',%s,%s,%s,%s,%s,%d,%s) ON CONFLICT(paper_id,source,doi) DO UPDATE SET candidate_title=excluded.candidate_title,candidate_authors=excluded.candidate_authors,candidate_year=excluded.candidate_year,score=excluded.score,exact_title=excluded.exact_title,retrieved_at=excluded.retrieved_at"
        (sql_quote id) (sql_quote doi) (sql_quote t) (sql_quote a) (sql_quote y)
        (match score with Some f -> Printf.sprintf "%g" f | None -> "NULL")
        (if normalize_title t = normalize_title title then 1 else 0) (sql_quote (now_iso ())))
    ) candidates;
    candidate_count := !candidate_count + List.length candidates;
    if exact <> [] then (
      incr exact_count;
      Printf.printf "  EXACT TITLE CANDIDATE(S): %d (not assigned automatically)\n%!" (List.length exact);
      List.iter (fun (doi,t,a,y,score) ->
        Printf.printf "    DOI: %s\n    Authors: %s\n    Year: %s (paper year: %s)\n    Crossref score: %s\n%!"
          doi (if a = "" then "(not supplied)" else a)
          (if y = "" then "(not supplied)" else y) paper_year
          (match score with Some f -> Printf.sprintf "%.2f" f | None -> "(not supplied)");
        ignore t
      ) exact
    ) else (
      incr no_match;
      Printf.printf "  NO EXACT TITLE MATCH: %d candidate(s) staged for review\n%!" (List.length candidates);
      List.iter (fun (doi,t,a,y,score) ->
        Printf.printf "    %s  %s%s\n%!" doi t
          (match score with Some f -> Printf.sprintf " [score %.2f]" f | None -> "");
        ignore a; ignore y
      ) candidates
    );
    ignore authors
  ) papers;
  Printf.printf "\nDOI candidate search: %d papers with exact-title candidates, %d without exact title match, %d candidate records staged.\n%!"
    !exact_count !no_match !candidate_count;
  print_endline "No DOI values were assigned. Review candidates before applying any DOI."

let query_osdi_official_rows db year =
  let stmt = Sqlite3.prepare db
    "SELECT title, COALESCE(official_url,''), COALESCE(abstract_en,''), COALESCE(doi,'') \
     FROM papers WHERE conference='OSDI' AND year=?1 ORDER BY title" in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT year));
  let rec loop acc = match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW ->
        let title = text_column stmt 0 in
        let url = text_column stmt 1 in
        let abstract = text_column stmt 2 in
        let doi = text_column stmt 3 in
        let is_official = String.length url > 0 &&
          (try ignore (Str.search_forward (Str.regexp_string "/presentation/") url 0); true with Not_found -> false) in
        loop (if is_official then (title,url,abstract,doi)::acc else acc)
    | Sqlite3.Rc.DONE -> List.rev acc
    | rc -> check_rc "Verifying OSDI rows" rc; List.rev acc
  in
  let rows = loop [] in
  ignore (Sqlite3.finalize stmt);
  rows

let verify_osdi db year =
  let short_year = if String.length year >= 2 then String.sub year (String.length year-2) 2 else year in
  let source_url = "https://www.usenix.org/conference/osdi" ^ short_year ^ "/technical-sessions" in
  Printf.printf "Fetching official proceedings for verification: %s\n%!" source_url;
  match http_get_html source_url with
  | None -> failwith ("Could not fetch official USENIX page: " ^ source_url)
  | Some html ->
      let official = parse_usenix_papers html in
      let db_rows = query_osdi_official_rows db year in
      let official_by_title = Hashtbl.create 160 in
      let db_by_title = Hashtbl.create 160 in
      List.iter (fun (title,_,_,_) -> Hashtbl.replace official_by_title (normalize_title title) title) official;
      List.iter (fun (title,_,_,_) -> Hashtbl.replace db_by_title (normalize_title title) title) db_rows;
      let missing = Hashtbl.fold (fun key title acc ->
        if Hashtbl.mem db_by_title key then acc else title::acc) official_by_title [] |> List.sort String.compare in
      let extra = Hashtbl.fold (fun key title acc ->
        if Hashtbl.mem official_by_title key then acc else title::acc) db_by_title [] |> List.sort String.compare in
      let matched_db_rows = List.filter (fun (title,_,_,_) -> Hashtbl.mem official_by_title (normalize_title title)) db_rows in
      let missing_abstracts = List.filter (fun (_,_,abstract,_) -> String.trim abstract = "") matched_db_rows in
      let missing_dois = List.filter (fun (_,_,_,doi) -> String.trim doi = "") matched_db_rows in
      Printf.printf "\nOSDI %s verification\n" year;
      Printf.printf "Official paper candidates (keynote excluded): %d\n" (List.length official);
      Printf.printf "Database official-page records: %d\n" (List.length db_rows);
      Printf.printf "Missing from database: %d\n" (List.length missing);
      List.iter (fun t -> Printf.printf "  MISSING: %s\n" t) missing;
      Printf.printf "Not in official paper list: %d\n" (List.length extra);
      List.iter (fun t -> Printf.printf "  EXTRA: %s\n" t) extra;
      Printf.printf "Matched paper records with empty abstract: %d\n" (List.length missing_abstracts);
      List.iter (fun (t,_,_,_) -> Printf.printf "  NO ABSTRACT: %s\n" t) missing_abstracts;
      Printf.printf "Matched paper records without DOI: %d\n" (List.length missing_dois);
      if missing = [] && extra = [] then print_endline "TITLE SET: MATCH"
      else print_endline "TITLE SET: MISMATCH"

let status_label p =
  match p.reviewed, p.decision with
  | false, Undecided -> "UNREAD"
  | true, Undecided -> "READ"
  | _, Keep -> "KEEP"
  | _, Skip -> "SKIP"

let truncate width s =
  if width <= 3 || String.length s <= width then s
  else String.sub s 0 (width - 3) ^ "..."

let terminal_size () =
  try
    let ic = Unix.open_process_in "stty size 2>/dev/null" in
    let line = input_line ic in
    ignore (Unix.close_process_in ic);
    match String.split_on_char ' ' (String.trim line) |> List.filter ((<>) "") with
    | [rows; cols] -> (max 24 (int_of_string cols), max 10 (int_of_string rows))
    | _ -> (80, 24)
  with _ -> (80, 24)

let wrap_text width text =
  let width = max 10 width in
  let wrap_paragraph paragraph =
    let words = String.split_on_char ' ' (String.trim paragraph) |> List.filter ((<>) "") in
    let rec loop current_len current acc = function
      | [] -> List.rev (if current = "" then acc else current :: acc)
      | word :: rest ->
          if String.length word > width then
            let head = String.sub word 0 width in
            let tail = String.sub word width (String.length word - width) in
            if current = "" then loop width head acc (tail :: rest)
            else loop 0 "" (current :: acc) (word :: rest)
          else if current = "" then loop (String.length word) word acc rest
          else if current_len + 1 + String.length word <= width then
            loop (current_len + 1 + String.length word) (current ^ " " ^ word) acc rest
          else loop (String.length word) word (current :: acc) rest
    in
    loop 0 "" [] words
  in
  String.split_on_char '\n' text
  |> List.concat_map (fun paragraph ->
       if String.trim paragraph = "" then [""] else wrap_paragraph paragraph)

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

let render papers selected show_japanese abstract_scroll =
  clear_screen ();
  let width, height = terminal_size () in
  let count = List.length papers in
  let keeps = List.fold_left (fun n p -> if p.decision = Keep then n + 1 else n) 0 papers in
  let unread = List.fold_left (fun n p -> if not p.reviewed && p.decision = Undecided then n + 1 else n) 0 papers in
  Printf.printf " PAPER-WATCH  |  %d papers  |  %d unread  |  %d keep\n" count unread keeps;
  print_endline (String.make (min width 100) '-');
  print_endline " j/k: paper  r: read  s: keep  x: skip  u: reset  t: translation  [/]: abstract page  q: quit";
  print_endline (String.make (min width 100) '-');
  let list_rows = max 3 (min 8 (height / 3)) in
  let first = max 0 (min (max 0 (count - list_rows)) (selected - list_rows / 2)) in
  let last = min count (first + list_rows) in
  for i = first to last - 1 do
    let p = List.nth papers i in
    let marker = if i = selected then ">" else " " in
    let line = Printf.sprintf "%s [%5d] %-10s %s (%s)" marker (i + 1) (status_label p) p.title p.conference in
    print_endline (truncate width line)
  done;
  if count > list_rows then Printf.printf " Papers %d-%d of %d\n" (first + 1) last count;
  print_endline (String.make (min width 100) '-');
  match List.nth_opt papers selected with
  | None -> print_endline "No papers loaded."
  | Some p ->
      Printf.printf "%s — %s\n" p.conference p.year;
      Printf.printf "DOI: %s\n" (if p.doi = "" then "(none)" else p.doi);
      Printf.printf "Authors: %s\n" (truncate width p.authors);
      Printf.printf "Abstract source: %s\n" (if p.abstract_source = "" then "not available" else p.abstract_source);
      print_endline "ABSTRACT";
      print_endline "--------";
      let abstract = if show_japanese && p.abstract_ja <> "" then p.abstract_ja else p.abstract_en in
      let abstract = if abstract = "" then "[No abstract available.]" else abstract in
      let lines = wrap_text (width - 2) abstract in
      let reserved = 13 + (last - first) + (if count > list_rows then 1 else 0) in
      let visible = max 4 (height - reserved) in
      let max_scroll = max 0 (List.length lines - visible) in
      let scroll = min abstract_scroll max_scroll in
      List.iter print_endline (List.filteri (fun i _ -> i >= scroll && i < scroll + visible) lines);
      if max_scroll > 0 then Printf.printf "\nAbstract lines %d-%d of %d\n" (scroll + 1) (min (scroll + visible) (List.length lines)) (List.length lines);
      if show_japanese && p.abstract_ja = "" then print_endline "[No Japanese translation available.]";
      Printf.printf "\nCurrent state: %s\n" (status_label p);
      flush stdout

let set_state db p reviewed decision =
  p.reviewed <- reviewed; p.decision <- decision; save_state db p

let main () =
  let db_path = default_db_path () in
  mkdir_p (Filename.dirname db_path);
  let db = Sqlite3.db_open db_path in
  try
    init_schema db;
    let command = if Array.length Sys.argv > 1 then Sys.argv.(1) else "" in
    if command = "--import-dois" && Array.length Sys.argv > 2 then (
      import_dois db Sys.argv.(2); ignore (Sqlite3.db_close db); exit 0
    );
    if command = "--fetch-abstracts" then (
      fetch_abstracts db; ignore (Sqlite3.db_close db); exit 0
    );
    if command = "--resolve-dois" && Array.length Sys.argv > 3 then (
      resolve_dois db (String.uppercase_ascii Sys.argv.(2)) Sys.argv.(3);
      ignore (Sqlite3.db_close db); exit 0
    );
    if command = "--verify" && Array.length Sys.argv > 3 && String.uppercase_ascii Sys.argv.(2) = "OSDI" then (
      verify_osdi db Sys.argv.(3); ignore (Sqlite3.db_close db); exit 0
    );
    if command = "--collect" && Array.length Sys.argv > 3 && String.uppercase_ascii Sys.argv.(2) = "OSDI" then (
      collect_osdi db Sys.argv.(3); ignore (Sqlite3.db_close db); exit 0
    );
    let tsv_path = if command <> "" && command.[0] <> '-' then command else "papers.tsv" in
    ensure_seed_data db tsv_path;
    let papers = load_papers db in
    if papers = [] then (prerr_endline "No papers in database. Provide a TSV file on first run."; ignore (Sqlite3.db_close db); exit 0);
    let old_term = terminal_raw_mode () in
    let selected = ref 0 and show_japanese = ref false and abstract_scroll = ref 0 and running = ref true in
    let finish () = restore_terminal old_term; clear_screen (); print_endline "paper-watch: SQLite state saved." in
    (try
      while !running do
        render papers !selected !show_japanese !abstract_scroll;
        let key = read_key () in
        let p = List.nth papers !selected in
        (match key with
         | 'j' -> selected := min (List.length papers - 1) (!selected + 1); abstract_scroll := 0
         | 'k' -> selected := max 0 (!selected - 1); abstract_scroll := 0
         | 'r' -> set_state db p true Undecided
         | 's' -> set_state db p true Keep
         | 'x' -> set_state db p true Skip
         | 'u' -> set_state db p false Undecided
         | 't' -> show_japanese := not !show_japanese; abstract_scroll := 0
         | ']' | ' ' -> abstract_scroll := !abstract_scroll + max 1 (snd (terminal_size ()) / 2)
         | '[' -> abstract_scroll := max 0 (!abstract_scroll - max 1 (snd (terminal_size ()) / 2))
         | 'g' -> abstract_scroll := 0
         | 'G' -> abstract_scroll := 1000000
         | 'q' -> running := false
         | _ -> ())
      done;
      finish ()
    with exn -> restore_terminal old_term; raise exn);
    ignore (Sqlite3.db_close db)
  with exn -> ignore (Sqlite3.db_close db); raise exn

let () = main ()
