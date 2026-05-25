ALTER TABLE published_releases
ADD COLUMN yanked_at TEXT;

ALTER TABLE published_releases
ADD COLUMN yanked_by_github_login TEXT;
