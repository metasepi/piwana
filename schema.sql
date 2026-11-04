PRAGMA foreign_keys = ON;

CREATE TABLE IF NOT EXISTS papers (
  id TEXT PRIMARY KEY,
  doi TEXT UNIQUE,
  title TEXT NOT NULL,
  authors TEXT NOT NULL DEFAULT '',
  conference TEXT NOT NULL DEFAULT '',
  year TEXT NOT NULL DEFAULT '',
  abstract_en TEXT NOT NULL DEFAULT '',
  abstract_ja TEXT NOT NULL DEFAULT '',
  metadata_source TEXT,
  abstract_source TEXT,
  metadata_updated_at TEXT,
  abstract_fetched_at TEXT,
  abstract_status TEXT NOT NULL DEFAULT 'pending',
  abstract_error TEXT,
  official_url TEXT,
  collection_source TEXT,
  collected_at TEXT,
  doi_lookup_status TEXT,
  doi_lookup_source TEXT,
  doi_lookup_at TEXT,
  created_at TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE IF NOT EXISTS reviews (
  paper_id TEXT PRIMARY KEY REFERENCES papers(id) ON DELETE CASCADE,
  reviewed_at TEXT,
  decision TEXT NOT NULL DEFAULT 'undecided'
    CHECK (decision IN ('undecided', 'keep', 'skip')),
  updated_at TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX IF NOT EXISTS papers_conference_year_idx
  ON papers(conference, year);
CREATE INDEX IF NOT EXISTS reviews_decision_idx
  ON reviews(decision, reviewed_at);

-- Exact DOI matches from publisher proceedings are staged here first.
-- Rows with source LIKE 'proceedings:%' may be applied with --apply-dois;
-- older Crossref title-search candidates are kept for audit only.
CREATE TABLE IF NOT EXISTS doi_candidates (
  paper_id TEXT NOT NULL REFERENCES papers(id) ON DELETE CASCADE,
  source TEXT NOT NULL,
  doi TEXT NOT NULL,
  candidate_title TEXT NOT NULL DEFAULT '',
  candidate_authors TEXT NOT NULL DEFAULT '',
  candidate_year TEXT NOT NULL DEFAULT '',
  score REAL,
  exact_title INTEGER NOT NULL DEFAULT 0,
  retrieved_at TEXT NOT NULL,
  PRIMARY KEY (paper_id, source, doi)
);
