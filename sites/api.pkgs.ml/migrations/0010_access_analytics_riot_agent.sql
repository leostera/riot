ALTER TABLE index_reads
  ADD COLUMN riot_agent TEXT;

ALTER TABLE package_downloads
  ADD COLUMN riot_agent TEXT;

ALTER TABLE binary_downloads
  ADD COLUMN riot_agent TEXT;
