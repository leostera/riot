CREATE TABLE IF NOT EXISTS users (
  user_id TEXT PRIMARY KEY,
  github_id INTEGER NOT NULL UNIQUE,
  github_login TEXT NOT NULL UNIQUE,
  github_login_lower TEXT NOT NULL UNIQUE,
  github_name TEXT,
  github_avatar_url TEXT,
  github_email TEXT,
  github_email_verified INTEGER NOT NULL DEFAULT 0,
  created_at TEXT NOT NULL,
  updated_at TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS user_logins (
  github_login_lower TEXT PRIMARY KEY,
  github_login TEXT NOT NULL,
  user_id TEXT NOT NULL,
  updated_at TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS oauth_states (
  state_id TEXT PRIMARY KEY,
  return_to TEXT NOT NULL,
  created_at TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS sessions (
  session_id TEXT PRIMARY KEY,
  user_id TEXT NOT NULL,
  github_login TEXT NOT NULL,
  created_at TEXT NOT NULL,
  expires_at TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS api_tokens (
  token_id TEXT PRIMARY KEY,
  user_id TEXT NOT NULL,
  github_login TEXT NOT NULL,
  name TEXT NOT NULL,
  secret_hash TEXT NOT NULL UNIQUE,
  capabilities_json TEXT NOT NULL,
  created_at TEXT NOT NULL,
  last_used_at TEXT,
  revoked_at TEXT
);

CREATE TABLE IF NOT EXISTS api_token_lookups (
  secret_hash TEXT PRIMARY KEY,
  token_id TEXT NOT NULL,
  user_id TEXT NOT NULL,
  github_login TEXT NOT NULL,
  capabilities_json TEXT NOT NULL,
  revoked_at TEXT
);

CREATE TABLE IF NOT EXISTS package_claims (
  package_name TEXT PRIMARY KEY,
  package_locator TEXT NOT NULL,
  source_url TEXT NOT NULL,
  package_subdir TEXT NOT NULL,
  owner_user_id TEXT,
  owner_github_login TEXT,
  claimed_at TEXT NOT NULL,
  updated_at TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS published_releases (
  package_name TEXT NOT NULL,
  package_version TEXT NOT NULL,
  package_locator TEXT NOT NULL,
  source_url TEXT NOT NULL,
  package_subdir TEXT NOT NULL,
  selector TEXT NOT NULL,
  resolved_sha TEXT NOT NULL,
  package_description TEXT,
  package_license TEXT,
  package_homepage TEXT,
  package_repository TEXT,
  package_root_module TEXT,
  package_categories_json TEXT NOT NULL,
  package_keywords_json TEXT NOT NULL,
  dependencies_json TEXT NOT NULL,
  source_archive_key TEXT NOT NULL,
  manifest_key TEXT NOT NULL,
  published_at TEXT NOT NULL,
  PRIMARY KEY (package_name, package_version)
);

CREATE TABLE IF NOT EXISTS selector_resolutions (
  package_locator TEXT NOT NULL,
  selector TEXT NOT NULL,
  resolved_sha TEXT NOT NULL,
  frozen INTEGER NOT NULL,
  recorded_at TEXT NOT NULL,
  PRIMARY KEY (package_locator, selector)
);

CREATE TABLE IF NOT EXISTS packages (
  package_name TEXT PRIMARY KEY,
  normalized_name TEXT NOT NULL,
  latest_version TEXT NOT NULL,
  description TEXT,
  license TEXT,
  homepage TEXT,
  repository TEXT,
  root_module TEXT,
  canonical_locator TEXT NOT NULL,
  repo_url TEXT NOT NULL,
  repo_owner TEXT NOT NULL,
  repo_name TEXT NOT NULL,
  subdir TEXT NOT NULL,
  release_count INTEGER NOT NULL,
  updated_at TEXT NOT NULL
);

CREATE VIRTUAL TABLE IF NOT EXISTS package_search USING fts5 (
  package_name,
  description,
  repo_owner,
  repo_name,
  subdir,
  repository,
  tokenize = 'unicode61 remove_diacritics 2'
);

CREATE INDEX IF NOT EXISTS idx_sessions_user_id ON sessions(user_id);
CREATE INDEX IF NOT EXISTS idx_api_tokens_user_id ON api_tokens(user_id);
CREATE INDEX IF NOT EXISTS idx_api_tokens_secret_hash ON api_tokens(secret_hash);
CREATE INDEX IF NOT EXISTS idx_claims_owner_login ON package_claims(owner_github_login);
CREATE INDEX IF NOT EXISTS idx_releases_package_name ON published_releases(package_name);
CREATE INDEX IF NOT EXISTS idx_selector_resolutions_locator ON selector_resolutions(package_locator);
