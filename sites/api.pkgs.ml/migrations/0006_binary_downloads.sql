CREATE TABLE IF NOT EXISTS binary_downloads (
  download_id TEXT PRIMARY KEY,
  binary_name TEXT NOT NULL,
  object_key TEXT NOT NULL,
  downloaded_at TEXT NOT NULL
);

CREATE INDEX IF NOT EXISTS idx_binary_downloads_binary
  ON binary_downloads(binary_name, downloaded_at);

CREATE INDEX IF NOT EXISTS idx_binary_downloads_object
  ON binary_downloads(object_key);

CREATE INDEX IF NOT EXISTS idx_binary_downloads_downloaded_at
  ON binary_downloads(downloaded_at);
