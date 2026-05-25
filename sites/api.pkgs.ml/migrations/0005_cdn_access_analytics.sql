CREATE TABLE IF NOT EXISTS index_reads (
  read_id TEXT PRIMARY KEY,
  document_key TEXT NOT NULL,
  package_name TEXT,
  read_at TEXT NOT NULL
);

CREATE INDEX IF NOT EXISTS idx_index_reads_document ON index_reads(document_key, read_at);
CREATE INDEX IF NOT EXISTS idx_index_reads_package ON index_reads(package_name, read_at);
CREATE INDEX IF NOT EXISTS idx_index_reads_read_at ON index_reads(read_at);

CREATE TABLE IF NOT EXISTS package_downloads (
  download_id TEXT PRIMARY KEY,
  package_name TEXT NOT NULL,
  package_version TEXT NOT NULL,
  artifact_sha256 TEXT NOT NULL,
  source_archive_key TEXT NOT NULL,
  downloaded_at TEXT NOT NULL
);

CREATE INDEX IF NOT EXISTS idx_package_downloads_package
  ON package_downloads(package_name, package_version, downloaded_at);
CREATE INDEX IF NOT EXISTS idx_package_downloads_artifact ON package_downloads(artifact_sha256);
CREATE INDEX IF NOT EXISTS idx_package_downloads_downloaded_at ON package_downloads(downloaded_at);
