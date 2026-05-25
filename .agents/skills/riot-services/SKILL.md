---
name: riot-services
description: Use when changing, reviewing, debugging, testing, deploying, or documenting Riot repo services under `sites/`, including Cloudflare Workers, Wrangler configs, Astro/Starlight sites, pkgs.ml registry/CDN/docs/play flows, Cloudflare D1/R2/Queues/Durable Objects/Workflows bindings, service Bun lockfiles, and service-local TypeScript tests.
---

# Riot Services

## Service Loop

1. Use `riot-contributor` first for repo workflow, dirty-worktree handling, and validation expectations.
2. Work from the specific site/service directory; every service owns its own `package.json`, `bun.lock`, `tsconfig.json`, and usually `wrangler.toml`.
3. Use `bun run <script>` instead of invoking tools directly when a script exists.
4. Preserve per-service lockfiles. Do not add a repo-root JS workspace unless explicitly asked.
5. Treat deploys, remote migrations, live e2e tests, token changes, DNS changes, and production data changes as explicit-user-approval operations.
6. Prefer local checks and fakes first; live tests are opt-in through service-specific env vars.

## Service Map

- `sites/api.pkgs.ml`: package registry control-plane Worker. Owns publish/auth/search/views/events, D1 migrations, Drizzle schema, R2 storage keys, queues, Durable Object publication coordination, and D1 backup workflow.
- `sites/cdn.pkgs.ml`: CDN Worker over package artifacts, sparse index documents, Riot/OCaml binary metadata, and access analytics. Reuses registry types/storage helpers from `../api.pkgs.ml/src`.
- `sites/docs.pkgs.ml`: generated package-docs service. Serves docs from R2, consumes package-published/processing queues, records pipeline runs/events in registry D1, and uses Cloudflare Sandbox containers for docs/build verification.
- `sites/pkgs.ml`: public package registry web app, Astro + Cloudflare adapter + React islands. Reads registry view APIs and sparse index documents.
- `sites/riot.ml`: main static Riot website, Astro + Tailwind, Pages-style deploy.
- `sites/get.riot.ml`: install-script redirect/proxy Worker.

## Cloudflare Tooling

- Use Wrangler for Worker/Astro service lifecycle: `wrangler dev`, `wrangler deploy`, `wrangler d1 migrations apply`, `wrangler pages deploy`, `wrangler types`, and config-driven local development.
- Keep the service-local `wrangler` dependency current and pinned by lockfile rather than relying on a global install.
- Use the unified `cf` CLI only for API/resource inspection or operations it currently exposes, especially account, zone, DNS, D1 API, R2 API, queues API, workers API metadata, and schema discovery.
- Before using `cf` for real Cloudflare mutations, run `npx cf auth whoami` and inspect command shape with `npx cf agent-context <product>` or `npx cf schema <command...>`.
- Do not replace Wrangler deploy/dev/migration scripts with `cf` unless the service-specific command has been verified locally and supports the same config-driven behavior.

## Config And Bindings

- Keep `wrangler.toml`, exported `Env` TypeScript interfaces, and test fake environments in sync.
- Prefer explicit `Env` interfaces in Worker entrypoints and shared modules.
- Put non-secret defaults in `[vars]`; keep secrets in `.env`, Wrangler secrets, or Cloudflare-managed secrets.
- For D1 schema changes in `api.pkgs.ml`, add SQL migrations under `sites/api.pkgs.ml/migrations/` and update `src/schema.ts`/DB helpers together.
- Runtime D1 access goes through Drizzle helpers in `api.pkgs.ml/src/db.ts`, `metadata-db.ts`, `pipeline-db.ts`, `access-db.ts`, or a narrowly named sibling.
- R2 object key contracts live in `api.pkgs.ml/src/storage.ts`; update CDN/API/web consumers and tests together when changing keys.
- Registry event payload shape is user-facing service history. Add new event types to `api.pkgs.ml/src/types.ts`, DB serialization, views, and tests.

## Testing

- Run the changed service's `bun run check` before finishing.
- Run `bun run test` when the service defines local tests.
- Do not run `bun run deploy`, `bun run migrate:remote`, or live `test:e2e` unless explicitly requested.
- For `api.pkgs.ml`, local tests use `FakeR2Bucket`, `FakeD1Database`, `FakeQueue`, and `FakeExecutionContext` from `tests/helpers.ts`.
- For `cdn.pkgs.ml`, reuse registry test fakes instead of inventing Cloudflare SDK mocks.
- For `docs.pkgs.ml`, prefer fake pipeline executors and queue drain helpers; assert durable ordering by database sequence when testing event streams.
- For Astro services, run `bun run check`; expect Astro to regenerate `.astro` type files as part of checking.

## Web Work

- Match each service's existing UI stack. `pkgs.ml` uses Astro server output, React islands, Tailwind, and Cloudflare adapter; `docs*.ml` use Starlight where applicable; `riot.ml` is a static Astro site.
- Keep first screens as actual product/service surfaces, not marketing placeholders.
- Use existing components, layouts, CSS tokens, fonts, and route conventions before adding abstractions.
- For registry web changes, prefer registry view APIs in `sites/pkgs.ml/src/lib/web-views.ts` and sparse index helpers in `package-index.ts`.

## Operational Safety

- Treat production endpoints as real: `api.pkgs.ml`, `registry.pkgs.ml`, `cdn.pkgs.ml`, `pkgs.ml`, `docs.pkgs.ml`, `docs.riot.ml`, `play.riot.ml`, `riot.ml`, and `get.riot.ml`.
- Never run remote D1 restore/export/import, DNS writes, route/domain writes, deploys, or token operations without explicit user direction.
- If a live smoke test is requested, read that service README/env gates first and state which env vars make the test destructive or authenticated.
