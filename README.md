# piwana TUI (SQLite + proceedings collection + DOI abstracts)

A small OCaml terminal application for reviewing conference papers. Review
state, paper metadata, DOI candidates, and abstracts are stored in SQLite.

## Dependencies

On Debian/Ubuntu:

```sh
sudo apt install libsqlite3-dev curl
opam install dune sqlite3 yojson str
```

## Build and run

```sh
dune build
dune exec ./bin/piwana.exe
```

The database defaults to `~/.local/state/piwana/papers.sqlite3`.
Set `PIWANA_DB` to use another database path.

On first run, `papers.tsv` is imported if the database is empty. The bundled
TSV contains **fictional UI sample records** with placeholder DOIs
(`10.0000/...`); they are not real papers and cannot be used to test metadata
retrieval.

## Import individual DOIs

Create a text file with one DOI per line. A `https://doi.org/` URL is also
accepted. Blank lines and lines beginning with `#` are ignored.

```sh
dune exec ./bin/piwana.exe -- --import-dois dois.txt
dune exec ./bin/piwana.exe -- --fetch-abstracts
```

## Supported conferences

The collectors currently cover the following editions. The same commands work
for 2025 and 2026 unless noted.

| Conference | Publisher / index | How the edition is identified | DOI |
| --- | --- | --- | --- |
| OSDI | USENIX | official Technical Sessions page | none (USENIX does not expose DOI identifiers) |
| SOSP | ACM | ACM DL proceedings / Crossref | yes |
| ASPLOS | ACM | 2–3 proceedings volumes | yes |
| POPL | ACM PACMPL | PACMPL issue `POPL` | yes |
| PLDI | ACM PACMPL | PACMPL issue `PLDI` | yes |
| CPP | ACM | ACM DL proceedings / Crossref | yes |
| CAV | Springer LNCS | Springer book volumes | yes |

Verified article counts at the time of writing:

| Edition | Articles |
| --- | ---: |
| OSDI 2026 | 136 |
| SOSP 2026 | 62 |
| ASPLOS 2026 | 155 |
| POPL 2026 | 92 |
| PLDI 2026 | 108 |
| CPP 2026 | 27 |
| CAV 2026 | 82 |

Always compare these numbers with the official table of contents before
treating a collection as complete.

## One-command workflow

To collect all supported conferences for a year, apply exact publisher DOIs,
and fetch abstracts in one run:

```sh
dune exec ./bin/piwana.exe -- --sync all 2026
```

`all` is case-insensitive and runs OSDI, SOSP, ASPLOS, POPL, PLDI, CPP, and CAV
in that order. If one conference fails, the remaining conferences still run and
the failed ones are reported at the end.

To process a single conference instead:

```sh
dune exec ./bin/piwana.exe -- --sync SOSP 2026
dune exec ./bin/piwana.exe -- --sync OSDI 2026
dune exec ./bin/piwana.exe -- --sync ASPLOS 2026
dune exec ./bin/piwana.exe -- --sync POPL 2026
dune exec ./bin/piwana.exe -- --sync PLDI 2026
dune exec ./bin/piwana.exe -- --sync CPP 2026
dune exec ./bin/piwana.exe -- --sync CAV 2026
```

Then start the TUI:

```sh
dune exec ./bin/piwana.exe
```

`--sync` is an explicit user action. It applies only DOI values taken from the
official/structured proceedings index for that exact edition. It never falls
back to a fuzzy per-paper title search. Review decisions are not changed.

## Separate collection steps

Collection, DOI application, and abstract retrieval can also be run
separately:

```sh
# 1. Collect papers and stage exact DOI candidates.
dune exec ./bin/piwana.exe -- --collect SOSP 2026

# 2. Apply exact proceedings DOI candidates for this edition.
dune exec ./bin/piwana.exe -- --apply-dois SOSP 2026

# 3. Fetch abstracts for this edition.
dune exec ./bin/piwana.exe -- --fetch-abstracts SOSP 2026
```

`--resolve-dois` is the lower-level lookup command. It stages exact-title
matches without assigning them:

```sh
dune exec ./bin/piwana.exe -- --resolve-dois SOSP 2026
dune exec ./bin/piwana.exe -- --resolve-dois SOSP 2026 --refresh
```

By default, papers whose `doi_lookup_status` is already terminal
(`success`, `candidate_found`, `not_found`, `ambiguous`, or `unsupported`) are
not searched again. Use `--refresh` to repeat the lookup.

For OSDI, DOI lookup deliberately does no network work:

```text
DOI lookup for OSDI 2026: skipped (USENIX proceedings do not expose DOI identifiers).
No network requests were made; papers remain fully reviewable without DOI.
```

The status is recorded as `unsupported` so cron-like runs do not repeat it.

## DOI policy

The application no longer performs a Crossref title query for every paper.
DOI discovery now follows the publisher edition:

- The collector obtains the proceedings identity from the publisher metadata
  (ACM proceedings DOI and exact container title, PACMPL issue, or Springer
  book volume).
- Articles are accepted only when the container title matches exactly and,
  where applicable, the DOI belongs to the proceedings DOI prefix or the
  requested PACMPL issue.
- Exact-title matches are written to `doi_candidates` with
  `source LIKE 'proceedings:%'`.
- `papers.doi` is only changed by an explicit `--apply-dois` or `--sync`.
  If a paper has multiple distinct exact candidates, it is marked
  `ambiguous` and is not applied.
- Non-exact titles are not saved. Old Crossref title-search candidates remain
  in the database for audit but are ignored by `--apply-dois`.

`doi_lookup_status` values:

| Status | Meaning |
| --- | --- |
| `pending` / empty | Not searched yet |
| `candidate_found` | Exactly one exact DOI candidate staged |
| `ambiguous` | Multiple distinct exact candidates; needs review |
| `not_found` | No exact title match in the current edition |
| `success` | DOI applied |
| `unsupported` | Publisher does not expose DOIs (OSDI) |

## Abstract retrieval

```sh
dune exec ./bin/piwana.exe -- --fetch-abstracts
dune exec ./bin/piwana.exe -- --fetch-abstracts SOSP 2026
```

The fetcher tries these sources in order until it finds a non-empty abstract:

1. Crossref DOI metadata
2. OpenAlex DOI metadata
3. OpenAlex exact-title search (preprint/arXiv record)
4. arXiv exact-title search
5. Semantic Scholar DOI metadata
6. ACM Digital Library HTML metadata

OpenAlex abstracts are reconstructed from `abstract_inverted_index`. The
OpenAlex and arXiv title fallbacks accept only an exact normalized title match;
they do not use fuzzy matching. The successful source is recorded in
`abstract_source` (for example `arxiv`, `openalex_title`, or
`semantic_scholar`). Semantic Scholar requests are retried when the API returns
HTTP 429 or a server error. If you have a Semantic Scholar API key, export it to
reduce rate limiting:

```sh
export SEMANTIC_SCHOLAR_API_KEY=your-key
# or: export S2_API_KEY=your-key
```

Retrieval stops at the first non-empty abstract and does not download PDFs.

Accessible author pages or publisher pages found by a general web search may
still have an abstract that these sources lack. Such manual retrieval is not
automatic; add the text to `abstract_en` in SQLite if you need it. The bundled
collectors intentionally avoid scraping arbitrary search-engine results.

The `papers` table records `abstract_source`, `abstract_fetched_at`, and
`abstract_status` (`pending`, `success`, or `not_found`). Records without an
abstract remain visible and are not treated as successful. Papers marked
`not_found` are retried by the next explicit `--fetch-abstracts` run.

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

The selected paper detail pane wraps the abstract to the terminal width, shows
its retrieval source, and pages long abstracts vertically. The paper list is
limited to a viewport around the selected row so it does not push the abstract
off-screen.

## Official proceedings and fallbacks

ACM Digital Library pages are protected by a browser challenge in many
automated environments. The collector first tries the ACM DL proceedings table
of contents. If that is unavailable or incomplete, it uses Crossref metadata
deposited by ACM, scoped to the exact proceedings container title and DOI
prefix (or the PACMPL issue). This is a structured edition lookup, not a fuzzy
per-paper search.

Springer Nature Link may also present a bot challenge. The CAV collector first
tries the official Springer conference/book pages, including the book volume
list and paginated tables of contents. If those pages are unavailable or
incomplete, it uses Crossref book-chapter metadata for the exact
`Computer Aided Verification` container and publication year.

Because the fallback is edition-scoped, a failure is reported as an empty or
incomplete collection rather than being silently replaced with unrelated
papers. Check the printed article counts against the official proceedings.

## Scope and limitations

This is a proceedings-and-abstract review tool, not a full bibliographic
database. Coverage depends on whether the metadata providers have an abstract
for a DOI. Publisher pages may block automated requests or omit
machine-readable abstract metadata; in that case the paper remains available
for review without an abstract.

The database schema is migrated automatically when the application starts.
Existing review state is preserved across collection, DOI application, and
abstract retrieval.
