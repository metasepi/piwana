# paper-watch TUI (SQLite + DOI Abstract Fetch)

A small OCaml terminal application for reviewing papers. Review state is stored in SQLite.

## Dependencies

On Debian/Ubuntu:

```sh
sudo apt install libsqlite3-dev curl
opam install dune sqlite3 yojson
```

## Build and run

```sh
dune build
dune exec ./bin/paper_watch.exe
```

The database defaults to `~/.local/state/paper-watch/papers.sqlite3`.
Set `PAPER_WATCH_DB` to use another database path.

On first run, `papers.tsv` is imported if the database is empty. The bundled TSV contains **fictional UI sample records** with placeholder DOIs (`10.0000/...`); they are not real papers and cannot be used to test metadata retrieval.

## Import real DOIs

Create a text file with one DOI per line. A `https://doi.org/` URL is also accepted. Blank lines and lines beginning with `#` are ignored.

Example `dois.txt`:

```text
10.xxxx/example-one
10.xxxx/example-two
https://doi.org/10.xxxx/example-three
```

Import them into the current SQLite database:

```sh
dune exec ./bin/paper_watch.exe -- --import-dois dois.txt
```

Then retrieve abstracts:

```sh
dune exec ./bin/paper_watch.exe -- --fetch-abstracts
```

The fetcher tries Crossref first and Semantic Scholar if Crossref has no abstract. When metadata is returned, title, authors, and year are also populated where available. It does not download PDFs.

The `papers` table records `abstract_source`, `abstract_fetched_at`, and `abstract_status` (`pending`, `success`, or `not_found`). Existing databases are migrated automatically when the application starts. Records without an abstract remain visible and are not silently treated as successful.

The fetch command only retries records with an empty abstract and status `pending` or `error`. A `not_found` result is retained to avoid repeatedly querying providers; edit its status to `pending` in SQLite if you want to retry later.

## TUI keys

- `j` / `k`: move down / up
- `r`: mark read (Undecided)
- `s`: Keep
- `x`: Skip
- `u`: reset to unread
- `t`: toggle English / Japanese abstract
- `]` or `Space`: scroll the abstract down by half a screen
- `[`: scroll the abstract up by half a screen
- `g` / `G`: jump to the beginning / end of the abstract
- `q`: quit

The selected paper detail pane wraps the abstract to the terminal width, shows its retrieval source, and pages long abstracts vertically. The paper list is limited to a viewport around the selected row so it does not push the abstract off-screen.

## Scope and limitations

This is an initial DOI-to-abstract retrieval path, not the full proceedings collector. Coverage depends on whether the metadata providers have an abstract for a DOI. The next step is to test with several real DOI records and compare the results against the publishers' official proceedings. The current implementation uses the `curl` executable for HTTPS requests.


### Abstract retrieval sources

The `--fetch-abstracts` command tries sources in this order: Crossref, Semantic Scholar, OpenAlex, and (for ACM DOI pages) the ACM Digital Library HTML metadata. OpenAlex abstracts are reconstructed from `abstract_inverted_index`. Retrieval stops at the first non-empty abstract. Failed attempts are retried on the next run. Publisher pages may block automated requests or omit machine-readable abstract metadata; in that case the paper remains unavailable rather than being treated as proof that no abstract exists.


## Official proceedings collection (experimental)

Collect OSDI papers from the official USENIX Technical Sessions page:

```sh
dune exec ./bin/paper_watch.exe -- --collect OSDI 2026
```

The collector stores a stable edition/title-derived ID, title, authors, abstract, official presentation URL, source page URL, and collection timestamp. It does not overwrite review decisions. OSDI records currently have no DOI unless one is resolved separately. This first parser is experimental: compare the reported number of parsed records against the official table of contents before treating a run as complete.

The collector requires `curl` and network access. It adds `official_url`, `collection_source`, and `collected_at` columns automatically to existing databases.
