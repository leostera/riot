CREATE TABLE IF NOT EXISTS registry_events (
  sequence_id INTEGER PRIMARY KEY AUTOINCREMENT,
  event_id TEXT NOT NULL UNIQUE,
  event_type TEXT NOT NULL,
  package_name TEXT,
  package_version TEXT,
  package_locator TEXT,
  payload_json TEXT NOT NULL,
  created_at TEXT NOT NULL
);

CREATE INDEX IF NOT EXISTS idx_registry_events_sequence_id ON registry_events(sequence_id);
CREATE INDEX IF NOT EXISTS idx_registry_events_created_at ON registry_events(created_at);
CREATE INDEX IF NOT EXISTS idx_registry_events_package ON registry_events(package_name, package_version, created_at);

DROP TABLE IF EXISTS web_views;
