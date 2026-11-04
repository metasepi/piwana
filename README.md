# paper-watch TUI (SQLite edition)

A small terminal UI for reviewing papers. This version keeps the original keyboard-driven UI and stores bibliographic data and review decisions in SQLite.

## Requirements

- OCaml >= 4.13
- Dune >= 3.0
- SQLite development library and the OCaml `sqlite3` package

Install the OCaml dependency with:

```sh
opam install dune sqlite3
```

## Build and run

```sh
dune build
dune exec ./bin/paper_watch.exe
```

On the first run, `papers.tsv` in the current directory is imported if the database is empty. The TSV format remains:

```text
id<TAB>title<TAB>authors<TAB>conference<TAB>year<TAB>abstract_en<TAB>abstract_ja
```

The database is created at:

```text
~/.local/state/paper-watch/papers.sqlite3
```

Override the database location with `PAPER_WATCH_DB=/path/to/papers.sqlite3`. If the old `reviews.tsv` state file exists, its states are migrated on the first TSV import. The old `PAPER_WATCH_STATE` environment variable controls that legacy file's path.

## Keys

- `j` / `k`: move down / up
- `r`: mark as read (decision remains undecided)
- `s`: Keep
- `x`: Skip
- `u`: reset to unread / undecided
- `t`: toggle English / Japanese abstract
- `q`: quit

## Database model

- `papers`: bibliographic metadata, DOI, English/Japanese abstract, and metadata provenance fields
- `reviews`: review timestamp and decision, keyed by `paper_id`

Review state is deliberately separate from bibliographic metadata. Future metadata refreshes can update titles and abstracts without overwriting Keep/Skip decisions. DOI has a UNIQUE constraint to support deduplication when real conference records are imported.

Useful inspection commands:

```sh
sqlite3 ~/.local/state/paper-watch/papers.sqlite3 '.schema'
sqlite3 ~/.local/state/paper-watch/papers.sqlite3 'select conference, year, count(*) from papers group by conference, year;'
sqlite3 ~/.local/state/paper-watch/papers.sqlite3 'select p.title, r.decision from papers p join reviews r on r.paper_id=p.id;'
```

The five current sample records are illustrative placeholders, not real papers. Live metadata collection is the next step.
