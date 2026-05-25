CREATE TABLE IF NOT EXISTS package_pipeline_runs (
  run_id TEXT PRIMARY KEY,
  run_kind TEXT NOT NULL,
  package_name TEXT NOT NULL,
  package_version TEXT NOT NULL,
  artifact_sha256 TEXT NOT NULL,
  source_archive_key TEXT NOT NULL,
  runner_kind TEXT NOT NULL,
  status TEXT NOT NULL,
  output_prefix TEXT NOT NULL,
  request_key TEXT NOT NULL,
  created_at TEXT NOT NULL,
  updated_at TEXT NOT NULL,
  started_at TEXT,
  finished_at TEXT,
  status_message TEXT,
  metadata_json TEXT NOT NULL
);

CREATE UNIQUE INDEX IF NOT EXISTS package_pipeline_runs_identity_unique
  ON package_pipeline_runs(package_name, package_version, artifact_sha256, run_kind);

CREATE INDEX IF NOT EXISTS idx_package_pipeline_runs_package
  ON package_pipeline_runs(package_name, package_version, run_kind, created_at);

CREATE INDEX IF NOT EXISTS idx_package_pipeline_runs_status
  ON package_pipeline_runs(status, updated_at);
