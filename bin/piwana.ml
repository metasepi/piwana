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
  match Sys.getenv_opt "PIWANA_DB" with
  | Some p -> p
  | None -> Filename.concat (home_dir ()) ".local/state/piwana/papers.sqlite3"

let legacy_state_path () =
  match Sys.getenv_opt "PIWANA_STATE" with
  | Some p -> p
  | None -> Filename.concat (home_dir ()) ".local/state/piwana/reviews.tsv"

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
  ensure_column "doi_lookup_status" "TEXT";
  ensure_column "doi_lookup_source" "TEXT";
  ensure_column "doi_lookup_at" "TEXT";
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

let starts_with s prefix =
  String.length s >= String.length prefix &&
  String.sub s 0 (String.length prefix) = prefix

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

let http_get_with_headers_status headers url =
  let header_args = String.concat " " (List.map (fun h -> "-H " ^ shell_quote h) headers) in
  let command =
    "curl -sSL --connect-timeout 8 --max-time 30 " ^ header_args ^
    " -w '%{http_code}' " ^ shell_quote url ^ " 2>/dev/null" in
  let ic = Unix.open_process_in command in
  let contents =
    let b = Buffer.create 4096 in
    (try while true do Buffer.add_string b (input_line ic); Buffer.add_char b '\n' done with End_of_file -> ());
    String.trim (Buffer.contents b)
  in
  let exited_ok = match Unix.close_process_in ic with Unix.WEXITED 0 -> true | _ -> false in
  if not exited_ok then (0, "")
  else
    let len = String.length contents in
    let status =
      if len >= 3 then
        match int_of_string_opt (String.sub contents (len - 3) 3) with
        | Some code -> code
        | None -> 0
      else 0 in
    let body = if len >= 3 then String.sub contents 0 (len - 3) else contents in
    (status, body)

let http_get_with_headers headers url =
  match http_get_with_headers_status headers url with
  | status, body when status >= 200 && status < 300 -> Some body
  | _ -> None

let http_get url = http_get_with_headers ["Accept: application/json"; "User-Agent: piwana/0.1 (academic literature review)"] url
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

let title_key s =
  let s = String.lowercase_ascii s in
  let b = Buffer.create (String.length s) in
  String.iter (fun c ->
    if (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') then
      Buffer.add_char b c) s;
  Buffer.contents b

let collapse_whitespace s =
  let b = Buffer.create (String.length s) in
  let pending_space = ref false in
  String.iter (fun c ->
    if c = ' ' || c = '\t' || c = '\n' || c = '\r' then pending_space := true
    else (
      if !pending_space && Buffer.length b > 0 then Buffer.add_char b ' ';
      pending_space := false;
      Buffer.add_char b c)) s;
  String.trim (Buffer.contents b)

let strip_abstract_label s =
  let s = String.trim s in
  let label = "abstract" in
  let n = String.length label in
  if String.length s > n &&
     String.lowercase_ascii (String.sub s 0 n) = label
  then
    let rest = String.sub s n (String.length s - n) in
    match rest.[0] with
    | ' ' | '\t' | '\r' | '\n' | ':' | '.' | '-' ->
        let rest = String.trim rest in
        let rest =
          if rest <> "" && (rest.[0] = ':' || rest.[0] = '-') then
            String.trim (String.sub rest 1 (String.length rest - 1))
          else rest in
        if rest = "" then s else rest
    | _ -> s
  else s

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

let semantic_scholar_api_key () =
  let nonempty = function
    | Some value ->
        let value = String.trim value in
        if value = "" then None else Some value
    | None -> None in
  match nonempty (Sys.getenv_opt "SEMANTIC_SCHOLAR_API_KEY") with
  | Some key -> Some key
  | None -> nonempty (Sys.getenv_opt "S2_API_KEY")

let semantic_scholar_headers () =
  ["Accept: application/json"; "User-Agent: piwana/0.1 (academic literature review)"] @
  (match semantic_scholar_api_key () with
   | Some key -> ["x-api-key: " ^ key]
   | None -> [])

let parse_semantic_scholar_record body =
  try
    let obj = Yojson.Basic.from_string body in
    let abstract = json_string (json_field "abstract" obj) in
    let title = json_string (json_field "title" obj) in
    let authors = match json_field "authors" obj with
      | `List xs -> String.concat ", " (List.filter ((<>) "") (List.map (fun a -> json_string (json_field "name" a)) xs))
      | _ -> "" in
    let year = json_string (json_field "year" obj) in
    Some (abstract, title, authors, year)
  with _ -> None

let semantic_scholar_record doi =
  let paper_id = "DOI:" ^ doi in
  let url = "https://api.semanticscholar.org/graph/v1/paper/" ^
    percent_encode_path paper_id ^ "?fields=title,abstract,authors,year" in
  let rec attempt tries =
    match http_get_with_headers_status (semantic_scholar_headers ()) url with
    | 200, body -> parse_semantic_scholar_record body
    | status, _ when (status = 429 || status >= 500) && tries > 0 ->
        let delay = match tries with 4 -> 3.0 | 3 -> 5.0 | 2 -> 8.0 | _ -> 12.0 in
        Unix.sleepf delay;
        attempt (tries - 1)
    | _ -> None
  in
  attempt 4

let openalex_abstract_of_item item =
  match json_field "abstract_inverted_index" item with
  | `Assoc entries ->
      let words = List.concat (List.map (fun (word, positions) ->
        match positions with
        | `List ps -> List.filter_map (function `Int n -> Some (n, word) | _ -> None) ps
        | _ -> []) entries) in
      words |> List.sort (fun (a,_) (b,_) -> compare a b)
      |> List.map snd |> String.concat " "
  | _ -> ""

let openalex_record doi =
  let url = "https://api.openalex.org/works/https://doi.org/" ^ percent_encode_path doi in
  match http_get url with
  | None -> None
  | Some body ->
      (try
        let obj = Yojson.Basic.from_string body in
        let abstract = openalex_abstract_of_item obj in
        let title = json_string (json_field "title" obj) in
        let year = json_string (json_field "publication_year" obj) in
        Some (abstract, title, "", year)
       with _ -> None)

let openalex_title_record title =
  let url = "https://api.openalex.org/works?search=" ^ percent_encode_path title ^
    "&per-page=50&select=title,abstract_inverted_index,doi,publication_year" in
  match http_get url with
  | None -> None
  | Some body ->
      (try
        let root = Yojson.Basic.from_string body in
        let items = match json_field "results" root with `List xs -> xs | _ -> [] in
        let wanted = title_key title in
        let candidates = List.filter_map (fun item ->
          let item_title = json_string (json_field "title" item) in
          let abstract = openalex_abstract_of_item item in
          if abstract = "" || title_key item_title <> wanted then None
          else
            let doi = json_string (json_field "doi" item) in
            let preprint = starts_with doi "https://doi.org/10.48550/arxiv." in
            let score = (if preprint then 2 else 0) +
              (if String.length abstract >= 500 then 1 else 0) in
            Some (score, String.length abstract, abstract, item_title,
                  json_string (json_field "publication_year" item))) items in
        match List.sort (fun (s1,l1,_,_,_) (s2,l2,_,_,_) ->
          compare (s2,l2) (s1,l1)) candidates with
        | (_, _, abstract, t, y) :: _ -> Some (abstract, t, "", y)
        | [] -> None
       with _ -> None)

let parse_arxiv_entries xml =
  let start_re = Str.regexp "<entry>" in
  let end_re = Str.regexp "</entry>" in
  let rec loop pos acc =
    try
      ignore (Str.search_forward start_re xml pos);
      let start = Str.match_end () in
      ignore (Str.search_forward end_re xml start);
      let stop = Str.match_beginning () in
      let block = String.sub xml start (stop - start) in
      loop (Str.match_end ()) (block :: acc)
    with Not_found -> List.rev acc
  in
  loop 0 []

let xml_tag tag block =
  let re = Str.regexp ("<" ^ tag ^ ">\\([^<]*\\)</" ^ tag ^ ">") in
  try
    ignore (Str.search_forward re block 0);
    html_to_text (Str.matched_group 1 block)
  with Not_found -> ""

let arxiv_record title =
  let query = "ti:\"" ^ title ^ "\"" in
  let url = "https://export.arxiv.org/api/query?search_query=" ^
    percent_encode_path query ^ "&max_results=10" in
  match http_get_with_headers
    ["Accept: application/atom+xml";
     "User-Agent: piwana/0.1 (academic literature review)"] url with
  | None -> None
  | Some xml ->
      let wanted = title_key title in
      let candidates = parse_arxiv_entries xml |> List.filter_map (fun block ->
        let item_title = xml_tag "title" block in
        let abstract = xml_tag "summary" block |> collapse_whitespace in
        if abstract = "" || title_key item_title <> wanted then None
        else Some (abstract, item_title)) in
      (match candidates with
       | (abstract, t) :: _ -> Some (abstract, t, "", "")
       | [] -> None)

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
    "UPDATE papers SET title=CASE WHEN %s='' THEN title ELSE %s END,authors=CASE WHEN %s='' THEN authors ELSE %s END,year=CASE WHEN year='' THEN %s ELSE year END,metadata_source=%s,metadata_updated_at=%s,abstract_en=CASE WHEN %s='' THEN abstract_en ELSE %s END,abstract_source=CASE WHEN %s='' THEN abstract_source ELSE %s END,abstract_fetched_at=%s,abstract_status=%s,abstract_error=NULL WHERE id=%s"
    (sql_quote title) (sql_quote title) (sql_quote authors) (sql_quote authors)
    (sql_quote year) (sql_quote source) stamp
    (sql_quote abstract) (sql_quote abstract) (sql_quote abstract) (sql_quote source)
    stamp (sql_quote (if abstract = "" then "not_found" else "success")) (sql_quote id))

let fetch_abstracts ?(conference="") ?(year="") db =
  let filters = [
    "doi IS NOT NULL"; "doi<>''"; "abstract_en=''";
    "abstract_status IN ('pending','error','not_found')" ] in
  let filters =
    if conference = "" then filters else filters @ ["conference=" ^ sql_quote conference] in
  let filters = if year = "" then filters else filters @ ["year=" ^ sql_quote year] in
  let where = String.concat " AND " filters in
  let stmt = Sqlite3.prepare db
    ("SELECT id,doi,title FROM papers WHERE " ^ where ^ " ORDER BY conference,year,title") in
  let rec collect acc = match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW -> collect ((text_column stmt 0, text_column stmt 1, text_column stmt 2) :: acc)
    | Sqlite3.Rc.DONE -> List.rev acc
    | rc -> check_rc "Selecting papers to fetch" rc; List.rev acc
  in
  let rows = collect [] in
  ignore (Sqlite3.finalize stmt);
  let ok = ref 0 and missing = ref 0 in
  List.iter (fun (id, doi, paper_title) ->
    Printf.printf "Fetching %s ... %!" doi;
    let crossref = crossref_record doi in
    let result =
      let candidates = [
        ("crossref", (fun () -> crossref));
        ("openalex", (fun () -> openalex_record doi));
        ("openalex_title", (fun () -> openalex_title_record paper_title));
        ("arxiv", (fun () -> arxiv_record paper_title));
        ("semantic_scholar", (fun () -> semantic_scholar_record doi));
        ("acm", (fun () -> acm_record doi))
      ] in
      let rec choose = function
        | [] -> None
        | (source, fetch) :: rest ->
            (match fetch () with
             | Some (abstract, title, authors, year) when abstract <> "" ->
                 let fallback = match crossref with Some (_, t, a, y) -> (t,a,y) | None -> ("","","") in
                 let ft, fa, fy = fallback in
                 Some (source, strip_abstract_label abstract,
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
        exec db (Printf.sprintf "UPDATE papers SET abstract_status='not_found',abstract_fetched_at=%s,abstract_error='No abstract returned by Crossref, OpenAlex, arXiv, Semantic Scholar, or ACM' WHERE id=%s"
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
          "INSERT INTO papers(id,doi,title,authors,conference,year,abstract_en,metadata_source,abstract_source,metadata_updated_at,abstract_fetched_at,abstract_status,official_url,collection_source,collected_at,doi_lookup_status,doi_lookup_source,doi_lookup_at) VALUES(%s,%s,%s,%s,'OSDI',%s,%s,'usenix_official','usenix_official',%s,%s,%s,%s,%s,%s,'unsupported','usenix_official',%s) ON CONFLICT(id) DO UPDATE SET title=excluded.title,authors=excluded.authors,abstract_en=CASE WHEN excluded.abstract_en<>'' THEN excluded.abstract_en ELSE papers.abstract_en END,metadata_source='usenix_official',abstract_source=CASE WHEN excluded.abstract_en<>'' THEN 'usenix_official' ELSE papers.abstract_source END,metadata_updated_at=excluded.metadata_updated_at,abstract_fetched_at=CASE WHEN excluded.abstract_en<>'' THEN excluded.abstract_fetched_at ELSE papers.abstract_fetched_at END,abstract_status=CASE WHEN excluded.abstract_en<>'' THEN 'success' ELSE papers.abstract_status END,official_url=excluded.official_url,collection_source=excluded.collection_source,collected_at=excluded.collected_at,doi_lookup_status=CASE WHEN papers.doi IS NULL OR papers.doi='' THEN 'unsupported' ELSE papers.doi_lookup_status END,doi_lookup_source=CASE WHEN papers.doi IS NULL OR papers.doi='' THEN 'usenix_official' ELSE papers.doi_lookup_source END,doi_lookup_at=CASE WHEN papers.doi IS NULL OR papers.doi='' THEN excluded.doi_lookup_at ELSE papers.doi_lookup_at END"
          (sql_quote id) doi (sql_quote title) (sql_quote authors) (sql_quote year) (sql_quote abstract)
          stamp stamp (sql_quote (if abstract = "" then "not_found" else "success"))
          (sql_quote official_url) (sql_quote url) stamp stamp);
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

(* ------------------------------------------------------------------ *)
(* DOI policy and proceedings collectors                               *)
(* ------------------------------------------------------------------ *)

let unique_strings xs =
  let seen = Hashtbl.create 16 in
  List.filter (fun x ->
    if x = "" || Hashtbl.mem seen x then false
    else (Hashtbl.add seen x (); true)) xs

type proceedings_record = {
  pr_doi : string;
  pr_title : string;
  pr_authors : string;
  pr_source : string;
}

let dedup_records records =
  let seen = Hashtbl.create 256 in
  List.filter (fun r ->
    let key = String.lowercase_ascii (String.trim r.pr_doi) in
    if key = "" || Hashtbl.mem seen key then false
    else (Hashtbl.add seen key (); true)) records

let crossref_item_title item =
  match json_field "title" item with
  | `List (`String t :: _) -> strip_html t
  | _ -> ""

let crossref_item_authors item =
  match json_field "author" item with
  | `List xs ->
      String.concat ", " (List.filter ((<>) "") (List.map (fun a ->
        let given = json_string (json_field "given" a) in
        let family = json_string (json_field "family" a) in
        String.trim (given ^ " " ^ family)) xs))
  | _ -> ""

let crossref_has_container item container_title =
  let wanted = normalize_title container_title in
  match json_field "container-title" item with
  | `List xs ->
      List.exists (function
        | `String s -> normalize_title (strip_html s) = wanted
        | _ -> false) xs
  | _ -> false

let http_get_json_retry url =
  let rec attempt tries =
    match http_get url with
    | Some body ->
        (try Some (Yojson.Basic.from_string body)
         with _ ->
           if tries > 0 then (Unix.sleepf 1.0; attempt (tries - 1)) else None)
    | None ->
        if tries > 0 then (Unix.sleepf 1.0; attempt (tries - 1)) else None
  in
  attempt 2

let crossref_query_articles ~container_title ~issue ~year ~parent_doi ~article_type =
  let year_int = match int_of_string_opt year with Some y -> y | None -> 2026 in
  let filter = Printf.sprintf
    "type:%s,prefix:10.1145,from-pub-date:%04d-06-01,until-pub-date:%04d-06-30"
    article_type (year_int - 1) (year_int + 1) in
  let url = Printf.sprintf
    "https://api.crossref.org/works?query.container-title=%s&filter=%s&rows=1000&select=DOI,title,container-title,issue,published,author"
    (percent_encode_component container_title)
    (percent_encode_component filter) in
  match http_get_json_retry url with
  | None -> []
  | Some root ->
      let message = json_field "message" root in
      (match json_field "items" message with
       | `List items ->
           let records = List.filter_map (fun item ->
             let doi = String.trim (json_string (json_field "DOI" item)) in
             let title = crossref_item_title item in
             let item_issue = json_string (json_field "issue" item) in
             let published_year = json_year (json_field "published" item) in
             let matches_container = crossref_has_container item container_title in
             let matches_issue = match issue with
               | None -> true
               | Some wanted ->
                   String.uppercase_ascii (String.trim item_issue) =
                   String.uppercase_ascii (String.trim wanted) in
             let matches_prefix = parent_doi = "" || starts_with doi (parent_doi ^ ".") in
             let matches_year = parent_doi <> "" || published_year = year in
             if doi <> "" && title <> "" && matches_container && matches_issue &&
                matches_prefix && matches_year then
               Some { pr_doi = doi; pr_title = title;
                      pr_authors = crossref_item_authors item;
                      pr_source = "crossref_proceedings" }
             else None) items in
           if List.length records >= 990 then
             Printf.eprintf
               "Warning: Crossref returned close to its 1000-row limit for %s; the article list may be incomplete.\n%!"
               container_title;
           records
       | _ -> [])

let parse_acm_toc html =
  let anchor_re = Str.regexp_case_fold "<a[^>]*href=\"\\([^\"]+\\)\"[^>]*>" in
  let close_re = Str.regexp_case_fold "</a>" in
  let li_re = Str.regexp_case_fold "<li[^>]*>\\([^<]*\\)</li>" in
  let doi_re = Str.regexp "/doi/\\(10\\.[^\"?#]+\\)" in
  let labels = ["pdf"; "erider"; "abstract"; "full text"; "view"; "download"] in
  let extract_doi href =
    try
      ignore (Str.search_forward doi_re href 0);
      Some (Str.matched_group 1 href)
    with Not_found -> None in
  let authors_in block =
    let rec collect pos acc =
      try
        ignore (Str.search_forward li_re block pos);
        let author = strip_html (Str.matched_group 1 block) in
        collect (Str.match_end ()) (if author = "" then acc else author :: acc)
      with Not_found -> List.rev acc in
    collect 0 [] in
  let rec loop pos acc =
    try
      ignore (Str.search_forward anchor_re html pos);
      let href = Str.matched_group 1 html in
      let open_end = Str.match_end () in
      (match extract_doi href with
       | None -> loop open_end acc
       | Some doi ->
           ignore (Str.search_forward close_re html open_end);
           let close_start = Str.match_beginning () in
           let close_end = Str.match_end () in
           let title = strip_html (String.sub html open_end (close_start - open_end)) in
           let lower_title = String.lowercase_ascii title in
           if title = "" || List.mem lower_title labels then loop close_end acc
           else (
             let block_end =
               try
                 ignore (Str.search_forward anchor_re html close_end);
                 Str.match_beginning ()
               with Not_found -> String.length html in
             let block = String.sub html close_end (max 0 (block_end - close_end)) in
             let authors = String.concat ", " (authors_in block) in
             loop close_end
               ({ pr_doi = doi; pr_title = title; pr_authors = authors;
                  pr_source = "acm_dl" } :: acc)))
    with Not_found -> List.rev acc
  in
  dedup_records (loop 0 [])

type acm_edition = {
  ae_conference : string;
  ae_year : string;
  ae_volumes : (string * string) list;
  ae_issue : string option;
  ae_type : string;
}

let acm_editions = [
  { ae_conference = "SOSP"; ae_year = "2025";
    ae_volumes = [("10.1145/3731569",
      "Proceedings of the ACM SIGOPS 31st Symposium on Operating Systems Principles")];
    ae_issue = None; ae_type = "proceedings-article" };
  { ae_conference = "SOSP"; ae_year = "2026";
    ae_volumes = [("10.1145/3830418",
      "Proceedings of the ACM SIGOPS 32nd Symposium on Operating Systems Principles")];
    ae_issue = None; ae_type = "proceedings-article" };
  { ae_conference = "ASPLOS"; ae_year = "2025";
    ae_volumes = [
      ("10.1145/3669940",
       "Proceedings of the 30th ACM International Conference on Architectural Support for Programming Languages and Operating Systems, Volume 1");
      ("10.1145/3676641",
       "Proceedings of the 30th ACM International Conference on Architectural Support for Programming Languages and Operating Systems, Volume 2");
      ("10.1145/3676642",
       "Proceedings of the 30th ACM International Conference on Architectural Support for Programming Languages and Operating Systems, Volume 3")];
    ae_issue = None; ae_type = "proceedings-article" };
  { ae_conference = "ASPLOS"; ae_year = "2026";
    ae_volumes = [
      ("10.1145/3760250",
       "Proceedings of the 31st ACM International Conference on Architectural Support for Programming Languages and Operating Systems, Volume 1");
      ("10.1145/3779212",
       "Proceedings of the 31st ACM International Conference on Architectural Support for Programming Languages and Operating Systems, Volume 2")];
    ae_issue = None; ae_type = "proceedings-article" };
  { ae_conference = "CPP"; ae_year = "2025";
    ae_volumes = [("10.1145/3703595",
      "Proceedings of the 14th ACM SIGPLAN International Conference on Certified Programs and Proofs")];
    ae_issue = None; ae_type = "proceedings-article" };
  { ae_conference = "CPP"; ae_year = "2026";
    ae_volumes = [("10.1145/3779031",
      "Proceedings of the 15th ACM SIGPLAN International Conference on Certified Programs and Proofs")];
    ae_issue = None; ae_type = "proceedings-article" };
  { ae_conference = "POPL"; ae_year = "2025";
    ae_volumes = [("", "Proceedings of the ACM on Programming Languages")];
    ae_issue = Some "POPL"; ae_type = "journal-article" };
  { ae_conference = "POPL"; ae_year = "2026";
    ae_volumes = [("", "Proceedings of the ACM on Programming Languages")];
    ae_issue = Some "POPL"; ae_type = "journal-article" };
  { ae_conference = "PLDI"; ae_year = "2025";
    ae_volumes = [("", "Proceedings of the ACM on Programming Languages")];
    ae_issue = Some "PLDI"; ae_type = "journal-article" };
  { ae_conference = "PLDI"; ae_year = "2026";
    ae_volumes = [("", "Proceedings of the ACM on Programming Languages")];
    ae_issue = Some "PLDI"; ae_type = "journal-article" };
]

let find_acm_edition conference year =
  List.find_opt (fun e -> e.ae_conference = conference && e.ae_year = year) acm_editions

let acm_edition_key edition =
  match edition.ae_volumes with
  | (parent, _) :: _ when parent <> "" -> "acm:" ^ parent
  | _ ->
      let issue = match edition.ae_issue with Some i -> i | None -> "" in
      "acm:pacmpl:" ^ issue ^ ":" ^ edition.ae_year

let crossref_records_for_edition edition =
  let records =
    List.concat_map (fun (parent, container) ->
      let rs = crossref_query_articles ~container_title:container
        ~issue:edition.ae_issue ~year:edition.ae_year ~parent_doi:parent
        ~article_type:edition.ae_type in
      Printf.printf "  %s: %d article(s)\n%!"
        (if parent = "" then container else parent) (List.length rs);
      rs) edition.ae_volumes in
  dedup_records records

let fetch_acm_edition edition =
  let official_records =
    List.concat_map (fun (parent, _) ->
      if parent = "" then []
      else
        let url = "https://dl.acm.org/doi/proceedings/" ^ percent_encode_path parent in
        match http_get_html url with
        | None -> []
        | Some html ->
            parse_acm_toc html
            |> List.filter (fun r -> starts_with r.pr_doi (parent ^ ".")))
      edition.ae_volumes
    |> dedup_records in
  let crossref_records = crossref_records_for_edition edition in
  if official_records <> [] &&
     (crossref_records = [] || List.length official_records >= List.length crossref_records)
  then (official_records, "acm_dl")
  else (
    if official_records = [] then
      Printf.printf
        "ACM Digital Library table of contents unavailable; using Crossref publisher metadata (exact container and DOI prefix).\n%!"
    else
      Printf.printf
        "ACM Digital Library table of contents was incomplete; using the complete Crossref edition metadata instead.\n%!";
    (crossref_records, "crossref_proceedings"))

let parse_springer_year_book html year =
  let marker = "CAV " ^ year in
  try
    ignore (Str.search_forward (Str.regexp_string marker) html 0);
    let tail = String.sub html (Str.match_end ()) (String.length html - Str.match_end ()) in
    let re = Str.regexp "/book/\\(10\\.1007/[0-9.\\-]+\\)" in
    ignore (Str.search_forward re tail 0);
    Some (Str.matched_group 1 tail)
  with Not_found -> None

let parse_springer_book_links html =
  let re = Str.regexp "/book/\\(10\\.1007/[0-9.\\-]+\\)" in
  let rec loop pos acc =
    try
      ignore (Str.search_forward re html pos);
      loop (Str.match_end ()) (Str.matched_group 1 html :: acc)
    with Not_found -> List.rev acc
  in
  unique_strings (loop 0 [])

let parse_springer_chapters html =
  let start_re = Str.regexp_case_fold "<li[^>]*data-test=\"chapter\"[^>]*>" in
  let doi_re = Str.regexp "/chapter/\\(10\\.1007/[0-9.\\-]+_[0-9]+\\)" in
  let author_re = Str.regexp_case_fold
    "<li[^>]*class=\"app-author-list__item\"[^>]*>\\([^<]*\\)</li>" in
  let title_re = Str.regexp_case_fold
    "data-test=\"chapter-title[^\"]*\"[^>]*>[ \t\r\n]*<a[^>]*>\\([^<]*\\)</a>" in
  let rec blocks pos acc =
    try
      ignore (Str.search_forward start_re html pos);
      let start = Str.match_beginning () in
      let next =
        try
          ignore (Str.search_forward start_re html (Str.match_end ()));
          Str.match_beginning ()
        with Not_found -> String.length html in
      let block = String.sub html start (max 0 (next - start)) in
      let rec authors p a =
        try
          ignore (Str.search_forward author_re block p);
          let author = strip_html (Str.matched_group 1 block) in
          authors (Str.match_end ()) (if author = "" then a else author :: a)
        with Not_found -> List.rev a in
      let authors = String.concat ", " (authors 0 []) in
      let record =
        try
          ignore (Str.search_forward doi_re block 0);
          let doi = Str.matched_group 1 block in
          let title =
            try
              ignore (Str.search_forward title_re block 0);
              strip_html (Str.matched_group 1 block)
            with Not_found -> "" in
          let lower = String.lowercase_ascii title in
          if title = "" || authors = "" ||
             List.mem lower ["front matter"; "back matter"; "introduction"; "erratum"]
          then None
          else Some { pr_doi = doi; pr_title = title; pr_authors = authors;
                      pr_source = "springer_link" }
        with Not_found -> None in
      blocks next (match record with Some r -> r :: acc | None -> acc)
    with Not_found -> List.rev acc
  in
  dedup_records (blocks 0 [])

let crossref_cav_records year =
  let year_int = match int_of_string_opt year with Some y -> y | None -> 2026 in
  let filter = Printf.sprintf
    "type:book-chapter,from-pub-date:%04d-01-01,until-pub-date:%04d-12-31"
    year_int year_int in
  let url = Printf.sprintf
    "https://api.crossref.org/works?query.container-title=%s&filter=%s&rows=1000&select=DOI,title,container-title,published,author"
    (percent_encode_component "Computer Aided Verification")
    (percent_encode_component filter) in
  match http_get_json_retry url with
  | None -> []
  | Some root ->
      let message = json_field "message" root in
      (match json_field "items" message with
       | `List items ->
           List.filter_map (fun item ->
             let doi = String.trim (json_string (json_field "DOI" item)) in
             let title = crossref_item_title item in
             let lower = String.lowercase_ascii title in
             if doi <> "" && title <> "" &&
                crossref_has_container item "Computer Aided Verification" &&
                starts_with doi "10.1007/" &&
                not (List.mem lower ["front matter"; "back matter"; "introduction"; "erratum"])
             then Some { pr_doi = doi; pr_title = title;
                         pr_authors = crossref_item_authors item;
                         pr_source = "crossref_proceedings" }
             else None) items
       | _ -> [])

let parse_springer_page_numbers html =
  let re = Str.regexp "/book/\\([^\"?]+\\)\\?page=\\([0-9]+\\)" in
  let rec loop pos acc =
    try
      ignore (Str.search_forward re html pos);
      let page = int_of_string (Str.matched_group 2 html) in
      loop (Str.match_end ()) (page :: acc)
    with Not_found -> List.sort_uniq compare acc
  in
  loop 0 []

let fetch_springer_book_chapters book =
  let base = "https://link.springer.com/book/" ^ percent_encode_path book in
  match http_get_html base with
  | None -> []
  | Some html ->
      let first = parse_springer_chapters html in
      let pages = parse_springer_page_numbers html |> List.filter (fun page -> page > 1) in
      let rest = List.concat_map (fun page ->
        let url = base ^ "?page=" ^ string_of_int page ^ "#toc" in
        match http_get_html url with
        | None -> []
        | Some h -> parse_springer_chapters h) pages in
      first @ rest

let fetch_cav_edition year =
  let conference_url = "https://link.springer.com/conference/cav" in
  let official_records =
    match http_get_html conference_url with
    | None -> []
    | Some html ->
        (match parse_springer_year_book html year with
         | None -> []
         | Some first_book ->
             let book_url = "https://link.springer.com/book/" ^ percent_encode_path first_book in
             (match http_get_html book_url with
              | None -> []
              | Some book_html ->
                  let books = unique_strings (first_book :: parse_springer_book_links book_html) in
                  Printf.printf "Springer CAV %s: %d volume(s)\n%!" year (List.length books);
                  List.concat_map fetch_springer_book_chapters books))
    |> dedup_records in
  let crossref_records = dedup_records (crossref_cav_records year) in
  if official_records <> [] &&
     (crossref_records = [] || List.length official_records >= List.length crossref_records)
  then (official_records, "springer_link")
  else (
    if official_records = [] then
      Printf.printf
        "Springer book table of contents unavailable; using Crossref book-chapter metadata.\n%!"
    else
      Printf.printf
        "Springer book table of contents was incomplete; using the complete Crossref edition metadata instead.\n%!";
    (crossref_records, "crossref_proceedings"))

let find_paper_id_by_doi db doi =
  let stmt = Sqlite3.prepare db "SELECT id FROM papers WHERE doi=?1 LIMIT 1" in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT doi));
  let result =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW -> Some (text_column stmt 0)
    | _ -> None in
  ignore (Sqlite3.finalize stmt);
  result

let upsert_proceedings_records db conference year records =
  let stamp = sql_quote (now_iso ()) in
  let touched = ref 0 in
  List.iter (fun r ->
    let id =
      match find_paper_id_by_doi db r.pr_doi with
      | Some existing -> existing
      | None -> "doi:" ^ r.pr_doi in
    let official_url = "https://doi.org/" ^ r.pr_doi in
    let collection_source = "proceedings:" ^ r.pr_source in
    exec db (Printf.sprintf
      "INSERT INTO papers(id,title,authors,conference,year,abstract_status,metadata_source,metadata_updated_at,official_url,collection_source,collected_at) VALUES(%s,%s,%s,%s,%s,'pending',%s,%s,%s,%s,%s) \
       ON CONFLICT(id) DO UPDATE SET title=excluded.title,authors=CASE WHEN excluded.authors<>'' THEN excluded.authors ELSE papers.authors END,conference=excluded.conference,year=excluded.year,metadata_source=excluded.metadata_source,metadata_updated_at=excluded.metadata_updated_at,official_url=excluded.official_url,collection_source=excluded.collection_source,collected_at=excluded.collected_at"
      (sql_quote id) (sql_quote r.pr_title) (sql_quote r.pr_authors)
      (sql_quote conference) (sql_quote year) (sql_quote r.pr_source)
      stamp (sql_quote official_url) (sql_quote collection_source) stamp);
    if Sqlite3.changes db > 0 then incr touched;
    exec db (Printf.sprintf "INSERT OR IGNORE INTO reviews(paper_id) VALUES(%s)" (sql_quote id))
  ) records;
  !touched

type doi_stage_stats = {
  ds_checked : int;
  ds_staged : int;
  ds_exact : int;
  ds_missing : int;
  ds_ambiguous : int;
  ds_skipped : int;
}

let doi_status_is_terminal status =
  match String.lowercase_ascii status with
  | "success" | "candidate_found" | "not_found" | "ambiguous" | "unsupported" -> true
  | _ -> false

let stage_proceedings_candidates db conference year edition_key records ~refresh =
  let stmt = Sqlite3.prepare db
    "SELECT id,title,COALESCE(doi_lookup_status,'') FROM papers \
     WHERE conference=?1 AND year=?2 AND (doi IS NULL OR doi='') ORDER BY title" in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT conference));
  ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT year));
  let rec read acc = match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW ->
        read ((text_column stmt 0, text_column stmt 1, text_column stmt 2) :: acc)
    | Sqlite3.Rc.DONE -> List.rev acc
    | rc -> check_rc "Selecting papers for DOI staging" rc; List.rev acc
  in
  let all = read [] in
  ignore (Sqlite3.finalize stmt);
  let selected =
    if refresh then all
    else List.filter (fun (_, _, status) -> not (doi_status_is_terminal status)) all in
  let skipped = List.length all - List.length selected in
  let by_title = Hashtbl.create 256 in
  List.iter (fun r ->
    let key = normalize_title r.pr_title in
    let previous = try Hashtbl.find by_title key with Not_found -> [] in
    Hashtbl.replace by_title key (r :: previous)) records;
  let source = "proceedings:" ^ edition_key in
  let stamp = sql_quote (now_iso ()) in
  let staged = ref 0 and exact = ref 0 and missing = ref 0 and ambiguous = ref 0 in
  List.iter (fun (id, title, _) ->
    exec db (Printf.sprintf
      "DELETE FROM doi_candidates WHERE paper_id=%s AND source LIKE 'proceedings:%%'"
      (sql_quote id));
    let matches =
      try Hashtbl.find by_title (normalize_title title) with Not_found -> [] in
    let matches = dedup_records matches in
    List.iter
      (fun r ->
         let sql = Printf.sprintf
           "INSERT INTO doi_candidates(paper_id,source,doi,candidate_title,candidate_authors,candidate_year,score,exact_title,retrieved_at) VALUES(%s,%s,%s,%s,%s,%s,NULL,1,%s) \
            ON CONFLICT(paper_id,source,doi) DO UPDATE SET candidate_title=excluded.candidate_title,candidate_authors=excluded.candidate_authors,candidate_year=excluded.candidate_year,score=NULL,exact_title=1,retrieved_at=excluded.retrieved_at"
           (sql_quote id) (sql_quote source) (sql_quote r.pr_doi)
           (sql_quote r.pr_title) (sql_quote r.pr_authors) (sql_quote year)
           (sql_quote (now_iso ())) in
         exec db sql)
      matches;
    staged := !staged + List.length matches;
    let status =
      if List.length matches = 1 then (incr exact; "candidate_found")
      else if List.length matches > 1 then (incr ambiguous; "ambiguous")
      else (incr missing; "not_found") in
    exec db (Printf.sprintf
      "UPDATE papers SET doi_lookup_status=%s,doi_lookup_source=%s,doi_lookup_at=%s WHERE id=%s"
      (sql_quote status) (sql_quote source) stamp (sql_quote id))
  ) selected;
  { ds_checked = List.length selected; ds_staged = !staged; ds_exact = !exact;
    ds_missing = !missing; ds_ambiguous = !ambiguous; ds_skipped = skipped }

let print_stage_summary conference year stats =
  Printf.printf
    "DOI lookup for %s %s: %d paper(s) checked, %d exact-title DOI candidate(s) staged, %d without a match, %d ambiguous, %d previously searched.\n%!"
    conference year stats.ds_checked stats.ds_staged stats.ds_missing
    stats.ds_ambiguous stats.ds_skipped;
  if stats.ds_staged > 0 then
    Printf.printf
      "No DOI values were assigned automatically. Run --apply-dois %s %s to apply the exact proceedings matches.\n%!"
      conference year

let mark_osdi_doi_unsupported db year =
  let missing = scalar_int db (Printf.sprintf
    "SELECT COUNT(*) FROM papers WHERE conference='OSDI' AND year=%s AND (doi IS NULL OR doi='')"
    (sql_quote year)) in
  exec db (Printf.sprintf
    "UPDATE papers SET doi_lookup_status='unsupported',doi_lookup_source='usenix_official',doi_lookup_at=%s \
     WHERE conference='OSDI' AND year=%s AND (doi IS NULL OR doi='')"
    (sql_quote (now_iso ())) (sql_quote year));
  Printf.printf "DOI lookup for OSDI %s: skipped (USENIX proceedings do not expose DOI identifiers).\n" year;
  print_endline "No network requests were made; papers remain fully reviewable without DOI.";
  Printf.printf "Recorded 'unsupported' for %d paper(s) to avoid repeated lookups.\n%!" missing

let count_papers db conference year =
  scalar_int db (Printf.sprintf
    "SELECT COUNT(*) FROM papers WHERE conference=%s AND year=%s"
    (sql_quote conference) (sql_quote year))

let count_pending_doi_lookups db conference year =
  scalar_int db (Printf.sprintf
    "SELECT COUNT(*) FROM papers WHERE conference=%s AND year=%s AND (doi IS NULL OR doi='') \
     AND COALESCE(doi_lookup_status,'') NOT IN ('success','candidate_found','not_found','ambiguous','unsupported')"
    (sql_quote conference) (sql_quote year))

let collect_acm db conference year =
  match find_acm_edition conference year with
  | None ->
      failwith (Printf.sprintf
        "No ACM proceedings configuration for %s %s. Supported ACM editions: SOSP/ASPLOS/POPL/PLDI/CPP 2025-2026."
        conference year)
  | Some edition ->
      let records, source = fetch_acm_edition edition in
      if records = [] then
        failwith (Printf.sprintf "No articles found for %s %s." conference year);
      let touched = upsert_proceedings_records db conference year records in
      let stats = stage_proceedings_candidates db conference year
        (acm_edition_key edition) records ~refresh:true in
      Printf.printf
        "%s %s collection: %d article(s) from %s; %d row(s) inserted or updated; %d exact DOI candidate(s) staged.\n%!"
        conference year (List.length records) source touched stats.ds_staged

let collect_cav db year =
  let records, source = fetch_cav_edition year in
  if records = [] then failwith (Printf.sprintf "No articles found for CAV %s." year);
  let touched = upsert_proceedings_records db "CAV" year records in
  let stats = stage_proceedings_candidates db "CAV" year
    ("springer:cav:" ^ year) records ~refresh:true in
  Printf.printf
    "CAV %s collection: %d article(s) from %s; %d row(s) inserted or updated; %d exact DOI candidate(s) staged.\n%!"
    year (List.length records) source touched stats.ds_staged

let resolve_dois db conference year ~refresh =
  match conference with
  | "OSDI" -> mark_osdi_doi_unsupported db year
  | "SOSP" | "ASPLOS" | "PLDI" | "POPL" | "CPP" ->
      let total = count_papers db conference year in
      if total = 0 then
        Printf.printf
          "No %s %s papers in the database yet. Run --collect %s %s first.\n%!"
          conference year conference year
      else if not refresh && count_pending_doi_lookups db conference year = 0 then
        Printf.printf
          "DOI lookup for %s %s: all %d paper(s) already have a lookup status; use --refresh to search again.\n%!"
          conference year total
      else (
        match find_acm_edition conference year with
        | None ->
            failwith (Printf.sprintf "No ACM proceedings configuration for %s %s." conference year)
        | Some edition ->
            let records, source = fetch_acm_edition edition in
            if records = [] then
              failwith (Printf.sprintf "No articles found for %s %s." conference year);
            Printf.printf "Using %s for %s %s.\n%!" source conference year;
            let stats = stage_proceedings_candidates db conference year
              (acm_edition_key edition) records ~refresh in
            print_stage_summary conference year stats)
  | "CAV" ->
      let total = count_papers db "CAV" year in
      if total = 0 then
        Printf.printf
          "No CAV %s papers in the database yet. Run --collect CAV %s first.\n%!" year year
      else if not refresh && count_pending_doi_lookups db "CAV" year = 0 then
        Printf.printf
          "DOI lookup for CAV %s: all %d paper(s) already have a lookup status; use --refresh to search again.\n%!"
          year total
      else (
        let records, source = fetch_cav_edition year in
        if records = [] then failwith (Printf.sprintf "No articles found for CAV %s." year);
        Printf.printf "Using %s for CAV %s.\n%!" source year;
        let stats = stage_proceedings_candidates db "CAV" year
          ("springer:cav:" ^ year) records ~refresh in
        print_stage_summary "CAV" year stats)
  | _ ->
      failwith (Printf.sprintf
        "DOI lookup is not implemented for %s. Supported conferences: OSDI, SOSP, ASPLOS, PLDI, POPL, CPP, CAV."
        conference)

let doi_taken_by_other db doi id =
  let stmt = Sqlite3.prepare db "SELECT id FROM papers WHERE doi=?1 AND id<>?2 LIMIT 1" in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT doi));
  ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT id));
  let found = match Sqlite3.step stmt with Sqlite3.Rc.ROW -> true | _ -> false in
  ignore (Sqlite3.finalize stmt);
  found

let apply_exact_dois db conference year =
  let stmt = Sqlite3.prepare db
    "SELECT p.id,p.title,c.doi FROM papers p JOIN doi_candidates c ON c.paper_id=p.id \
     WHERE p.conference=?1 AND p.year=?2 AND (p.doi IS NULL OR p.doi='') \
       AND c.exact_title=1 AND c.source LIKE 'proceedings:%' ORDER BY p.id,c.doi" in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT conference));
  ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT year));
  let rec read acc = match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW ->
        read ((text_column stmt 0, text_column stmt 1, text_column stmt 2) :: acc)
    | Sqlite3.Rc.DONE -> List.rev acc
    | rc -> check_rc "Selecting exact DOI candidates" rc; List.rev acc
  in
  let rows = read [] in
  ignore (Sqlite3.finalize stmt);
  let table = Hashtbl.create 256 in
  List.iter (fun (id, title, doi) ->
    let previous = try Hashtbl.find table id with Not_found -> (title, []) in
    let _, dois = previous in
    Hashtbl.replace table id (title, unique_strings (doi :: dois))) rows;
  let applied = ref 0 and conflicts = ref 0 in
  Hashtbl.iter (fun id (title, dois) ->
    match dois with
    | [doi] ->
        if doi_taken_by_other db doi id then (
          incr conflicts;
          Printf.printf "  CONFLICT: %s -> %s is already assigned to another record; not applied.\n%!" title doi)
        else (
          exec db (Printf.sprintf
            "UPDATE papers SET doi=%s,doi_lookup_status='success',doi_lookup_source='proceedings',doi_lookup_at=%s,metadata_updated_at=%s WHERE id=%s"
            (sql_quote doi) (sql_quote (now_iso ())) (sql_quote (now_iso ())) (sql_quote id));
          incr applied;
          Printf.printf "  DOI %s -> %s\n%!" doi title)
    | _ ->
        incr conflicts;
        exec db (Printf.sprintf
          "UPDATE papers SET doi_lookup_status='ambiguous',doi_lookup_at=%s WHERE id=%s"
          (sql_quote (now_iso ())) (sql_quote id));
        Printf.printf "  CONFLICT: %s has %d distinct exact DOI candidates; not applied.\n%!"
          title (List.length dois)
  ) table;
  Printf.printf "Applied %d exact DOI(s) to %s %s; %d conflict(s).\n%!"
    !applied conference year !conflicts;
  if !applied = 0 then
    print_endline "No exact DOI candidates were applied. Review the collection and DOI staging output."
  else
    print_endline "Review decisions were not changed. Run --fetch-abstracts to retrieve abstracts for the new DOIs."

let supported_conferences = ["OSDI"; "SOSP"; "ASPLOS"; "POPL"; "PLDI"; "CPP"; "CAV"]

let collect_by_conference db conference year =
  match conference with
  | "OSDI" -> collect_osdi db year
  | "SOSP" | "ASPLOS" | "PLDI" | "POPL" | "CPP" -> collect_acm db conference year
  | "CAV" -> collect_cav db year
  | _ ->
      failwith (Printf.sprintf
        "Collection is not implemented for %s. Supported conferences: OSDI, SOSP, ASPLOS, PLDI, POPL, CPP, CAV."
        conference)

let sync_conference db conference year =
  collect_by_conference db conference year;
  (match conference with
   | "OSDI" -> mark_osdi_doi_unsupported db year
   | _ -> apply_exact_dois db conference year);
  fetch_abstracts ~conference:conference ~year:year db

let sync_all_conferences db year =
  let failures = ref [] in
  let record_failure conference message =
    Printf.eprintf "Skipping %s %s: %s\n%!" conference year message;
    failures := conference :: !failures in
  List.iter (fun conference ->
    Printf.printf "\n=== %s %s ===\n%!" conference year;
    try sync_conference db conference year
    with
    | Failure msg -> record_failure conference msg
    | exn -> record_failure conference (Printexc.to_string exn)) supported_conferences;
  match List.rev !failures with
  | [] -> ()
  | failed ->
      failwith (Printf.sprintf
        "Sync completed with errors for: %s" (String.concat ", " failed))

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
  Printf.printf " PIWANA  |  %d papers  |  %d unread  |  %d keep\n" count unread keeps;
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
      let conference = if Array.length Sys.argv > 2 then String.uppercase_ascii Sys.argv.(2) else "" in
      let year = if Array.length Sys.argv > 3 then Sys.argv.(3) else "" in
      fetch_abstracts ~conference ~year db; ignore (Sqlite3.db_close db); exit 0
    );
    if command = "--resolve-dois" && Array.length Sys.argv > 3 then (
      let refresh = Array.length Sys.argv > 4 && Sys.argv.(4) = "--refresh" in
      resolve_dois db (String.uppercase_ascii Sys.argv.(2)) Sys.argv.(3) ~refresh;
      ignore (Sqlite3.db_close db); exit 0
    );
    if command = "--apply-dois" && Array.length Sys.argv > 3 then (
      apply_exact_dois db (String.uppercase_ascii Sys.argv.(2)) Sys.argv.(3);
      ignore (Sqlite3.db_close db); exit 0
    );
    if command = "--sync" && Array.length Sys.argv > 3 then (
      let target = String.uppercase_ascii Sys.argv.(2) in
      if target = "ALL" then sync_all_conferences db Sys.argv.(3)
      else sync_conference db target Sys.argv.(3);
      ignore (Sqlite3.db_close db); exit 0
    );
    if command = "--verify" && Array.length Sys.argv > 3 && String.uppercase_ascii Sys.argv.(2) = "OSDI" then (
      verify_osdi db Sys.argv.(3); ignore (Sqlite3.db_close db); exit 0
    );
    if command = "--collect" && Array.length Sys.argv > 3 then (
      collect_by_conference db (String.uppercase_ascii Sys.argv.(2)) Sys.argv.(3);
      ignore (Sqlite3.db_close db); exit 0
    );
    let tsv_path = if command <> "" && command.[0] <> '-' then Some command else None in
    (match tsv_path with Some path -> ensure_seed_data db path | None -> ());
    let papers = load_papers db in
    if papers = [] then (
      prerr_endline "No papers in database. Run --sync all YEAR (for example: --sync all 2026) to collect papers.";
      ignore (Sqlite3.db_close db); exit 0);
    let old_term = terminal_raw_mode () in
    let selected = ref 0 and show_japanese = ref false and abstract_scroll = ref 0 and running = ref true in
    let finish () = restore_terminal old_term; clear_screen (); print_endline "piwana: SQLite state saved." in
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

let () =
  try main () with
  | Failure msg -> prerr_endline ("piwana: " ^ msg); exit 1
  | Sys_error msg -> prerr_endline ("piwana: " ^ msg); exit 1
