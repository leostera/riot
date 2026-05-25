CREATE TABLE IF NOT EXISTS package_releases_to_process (
  release_id TEXT PRIMARY KEY,
  package_name TEXT NOT NULL,
  package_version TEXT NOT NULL,
  artifact_sha256 TEXT NOT NULL,
  source_archive_key TEXT NOT NULL,
  status TEXT NOT NULL,
  attempt_count INTEGER NOT NULL DEFAULT 0,
  next_attempt_at TEXT NOT NULL,
  created_at TEXT NOT NULL,
  updated_at TEXT NOT NULL,
  last_attempted_at TEXT,
  lease_expires_at TEXT,
  finished_at TEXT,
  status_message TEXT,
  payload_json TEXT NOT NULL
);

CREATE UNIQUE INDEX IF NOT EXISTS package_releases_to_process_identity_unique
  ON package_releases_to_process(package_name, package_version, artifact_sha256);

CREATE INDEX IF NOT EXISTS idx_package_releases_to_process_status
  ON package_releases_to_process(status, next_attempt_at, updated_at);

CREATE INDEX IF NOT EXISTS idx_package_releases_to_process_package
  ON package_releases_to_process(package_name, package_version, updated_at);
