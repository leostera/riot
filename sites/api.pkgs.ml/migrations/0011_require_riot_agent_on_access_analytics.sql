CREATE TABLE index_reads_v2 (
  read_id TEXT PRIMARY KEY,
  document_key TEXT NOT NULL,
  package_name TEXT,
  riot_agent TEXT NOT NULL,
  read_at TEXT NOT NULL
);

INSERT INTO index_reads_v2 (read_id, document_key, package_name, riot_agent, read_at)
SELECT read_id, document_key, package_name, riot_agent, read_at
FROM index_reads
WHERE riot_agent IS NOT NULL;

DROP TABLE index_reads;
ALTER TABLE index_reads_v2 RENAME TO index_reads;

CREATE INDEX idx_index_reads_document ON index_reads(document_key, read_at);
CREATE INDEX idx_index_reads_package ON index_reads(package_name, read_at);
CREATE INDEX idx_index_reads_read_at ON index_reads(read_at);

CREATE TABLE package_downloads_v2 (
  download_id TEXT PRIMARY KEY,
  package_name TEXT NOT NULL,
  package_version TEXT NOT NULL,
  artifact_sha256 TEXT NOT NULL,
  source_archive_key TEXT NOT NULL,
  riot_agent TEXT NOT NULL,
  downloaded_at TEXT NOT NULL
);

INSERT INTO package_downloads_v2 (
  download_id,
  package_name,
  package_version,
  artifact_sha256,
  source_archive_key,
  riot_agent,
  downloaded_at
)
SELECT
  download_id,
  package_name,
  package_version,
  artifact_sha256,
  source_archive_key,
  riot_agent,
  downloaded_at
FROM package_downloads
WHERE riot_agent IS NOT NULL;

DROP TABLE package_downloads;
ALTER TABLE package_downloads_v2 RENAME TO package_downloads;

CREATE INDEX idx_package_downloads_package
  ON package_downloads(package_name, package_version, downloaded_at);
CREATE INDEX idx_package_downloads_artifact ON package_downloads(artifact_sha256);
CREATE INDEX idx_package_downloads_downloaded_at ON package_downloads(downloaded_at);

CREATE TABLE binary_downloads_v2 (
  download_id TEXT PRIMARY KEY,
  binary_name TEXT NOT NULL,
  object_key TEXT NOT NULL,
  riot_agent TEXT NOT NULL,
  downloaded_at TEXT NOT NULL
);

INSERT INTO binary_downloads_v2 (
  download_id,
  binary_name,
  object_key,
  riot_agent,
  downloaded_at
)
SELECT
  download_id,
  binary_name,
  object_key,
  riot_agent,
  downloaded_at
FROM binary_downloads
WHERE riot_agent IS NOT NULL;

DROP TABLE binary_downloads;
ALTER TABLE binary_downloads_v2 RENAME TO binary_downloads;

CREATE INDEX idx_binary_downloads_binary
  ON binary_downloads(binary_name, downloaded_at);
CREATE INDEX idx_binary_downloads_object
  ON binary_downloads(object_key);
CREATE INDEX idx_binary_downloads_downloaded_at
  ON binary_downloads(downloaded_at);
