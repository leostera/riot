CREATE TABLE IF NOT EXISTS session_handoffs (
  handoff_id TEXT PRIMARY KEY,
  session_id TEXT NOT NULL,
  return_to TEXT NOT NULL,
  created_at TEXT NOT NULL,
  expires_at TEXT NOT NULL
);

CREATE INDEX IF NOT EXISTS idx_session_handoffs_session_id
  ON session_handoffs(session_id);

CREATE INDEX IF NOT EXISTS idx_session_handoffs_expires_at
  ON session_handoffs(expires_at);
