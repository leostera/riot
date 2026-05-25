# riot-package-registry

Cloudflare Worker scaffold for Riot's package publication service.

Production endpoint: `https://api.pkgs.ml`

## Local development

1. Install dependencies with `bun install`.
2. Create a `.env` file next to `wrangler.toml`.
3. Run `bun run dev`.

Apply Wrangler D1 migrations before first run:

```bash
bun run migrate
```

For production databases:

```bash
bun run migrate:remote
```

## Backups and rollback

The control-plane database is Cloudflare D1 (`riot-registry`).

## Database-first rollback
- D1 Time Travel is enabled and should be the primary point-in-time recovery path:

  ```bash
  wrangler d1 time-travel info riot-registry
  wrangler d1 time-travel restore riot-registry --timestamp 2026-03-30T10:20:00Z
  ```

## Exported backups (Cloudflare Workflows)
`services/api.pkgs.ml` includes a `D1BackupWorkflow` that exports D1 to R2 using
Cloudflare’s D1 REST export API and stores `*.sql` snapshots in
`ML_PKGS_BACKUPS` (Cloudflare R2 bucket `ml-pkgs-backups`) at:
`{prefix}/{accountId}/{databaseId}/{YYYY-MM-DD}/{timestamp}-*.sql`.

- The workflow is scheduled by cron in `wrangler.toml` (default daily at midnight UTC).
- Enable it with `D1_BACKUP_ENABLED=true`.
- Configure:

```dotenv
D1_BACKUP_ACCOUNT_ID=<cloudflare-account-id>
D1_BACKUP_DATABASE_ID=3a482dd8-a28f-4f87-a143-46153d492819
D1_BACKUP_BUCKET_PREFIX=registry-database-backups
D1_REST_API_TOKEN=<cloudflare-api-token>
```

The token only needs `Cloudflare D1:Read` / export-capable permissions for the
target account/database.

Wrangler reads `.env` during local development, and the Worker expects at least:

```dotenv
CDN_BASE_URL=https://cdn.pkgs.ml
INDEX_BASE_URL=https://cdn.pkgs.ml
INDEX_BASE_PATH=index/v1
ROOT_AUTH_TOKEN=
GITHUB_OAUTH_CLIENT_ID=
GITHUB_OAUTH_CLIENT_SECRET=
AUTH_COOKIE_DOMAIN=pkgs.ml
PKGS_WEB_BASE_URL=https://pkgs.ml
```

## Current surface

- `GET /` returns service metadata.
- `POST /v1/publish` accepts a `tar.gz` package artifact body and publishes it.
- `GET /v1/auth/github/start?return_to=<url>` starts GitHub OAuth login.
- `GET /v1/auth/github/callback?code=<code>&state=<state>` completes GitHub OAuth, creates a user session, and redirects back to `pkgs.ml`.
- `POST /v1/auth/logout` clears the session cookie.
- `GET /v1/me` returns the authenticated user session, if one exists.
- `GET /v1/search?q=<query>` returns one search result per indexed package, backed by D1 + FTS5.
- `GET /v1/me/tokens` lists publish tokens for the authenticated user.
- `POST /v1/me/tokens` creates a new publish token and returns the plaintext token once.
- `DELETE /v1/me/tokens/<token-id>` revokes a publish token.

Package publish is artifact-native:

- the client uploads the package-root `tar.gz`
- the registry derives package metadata from `riot.toml` at archive root
- the registry stores immutable manifests and source artifacts in R2
- the registry synchronously updates the sparse index and search rows
- the sparse index and artifact downloads are served through `cdn.pkgs.ml`

Legacy compatibility aliases under `registry.pkgs.ml` and `/api/v1`/`/package/.../-/...` remain available during the transition.

The Worker stores registry control-plane metadata in D1:
auth, sessions, API tokens, package claims, published releases, registry
events, search, and derived web views. Runtime D1 access is implemented with
`drizzle-orm`, while schema changes are managed only through Wrangler D1 SQL
migrations in `services/api.pkgs.ml/migrations/`.

## Live smoke tests

Set a live registry base URL in `.env` to run end-to-end smoke tests against a
deployed Worker:

```dotenv
REGISTRY_E2E_BASE_URL=https://api.pkgs.ml
REGISTRY_E2E_ROOT_AUTH_TOKEN=
REGISTRY_ARTIFACT_E2E_BASE_URL=https://cdn.pkgs.ml
REGISTRY_E2E_SESSION_COOKIE=pkgs_session=...
REGISTRY_E2E_GITHUB_LOGIN=leostera
REGISTRY_INDEX_E2E_BASE_PATH=index/v1
```

Then run:

```bash
bun run test:e2e
```

The live tests are skipped when `REGISTRY_E2E_BASE_URL` is not set.
The live publish smoke test is skipped unless `REGISTRY_E2E_ROOT_AUTH_TOKEN` is
set. The authenticated token-management smoke tests are skipped unless both
`REGISTRY_E2E_SESSION_COOKIE` and `REGISTRY_E2E_GITHUB_LOGIN` are set.

`bun run test` only runs the local unit suite. The live registry smoke tests are
kept behind `bun run test:e2e`.
