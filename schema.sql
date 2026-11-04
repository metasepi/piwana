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
