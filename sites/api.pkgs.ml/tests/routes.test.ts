import { describe, expect, test } from "bun:test";

import { writeBinaryDownloadRecord, writePackageDownloadRecord } from "../src/access-db.ts";
import { buildSessionCookie } from "../src/auth.ts";
import { readArchiveFileFromTarGz } from "../src/archive.ts";
import { handleRequest } from "../src/routes.ts";
import {
  applyMetadataMigrations,
  listPackageRegistryEvents,
  readPackageClaim,
  readPublishedRelease,
  writePackageClaim,
  writeSessionRecord,
  writeUserRecord,
} from "../src/metadata-db.ts";
import { FakeExecutionContext, makeEnv, makeTarGz } from "./helpers.ts";

describe("riot package registry routes", () => {
  test("root route returns service metadata", async () => {
    const { env } = makeEnv();
    const ctx = new FakeExecutionContext();

    const response = await handleRequest(new Request("https://registry.test/"), env, ctx);
    await ctx.drain();

    expect(response.status).toBe(200);
    expect(await readJson(response)).toEqual({
      service: "riot-package-registry",
      routes: {
        publish_artifact: "/v1/publish",
        views_package_overview: "/v1/views/packages/<package-name>/overview",
        views_package_readme: "/v1/views/packages/<package-name>/readme?version=<version>",
        views_package_examples: "/v1/views/packages/<package-name>/examples?version=<version>",
        views_package_downloads: "/v1/views/packages/<package-name>/downloads",
        views_package_relations: "/v1/views/packages/<package-name>/relations",
        views_recent_packages: "/v1/views/recent/packages",
        views_popular_packages: "/v1/views/popular/packages",
        views_categories: "/v1/views/categories",
        views_owner_packages: "/v1/views/owners/<github-login>/packages",
        views_stats_summary: "/v1/views/stats/summary",
        views_stats_dashboard: "/v1/views/stats/dashboard",
        auth_github_start: "/v1/auth/github/start?return_to=<url>",
        auth_github_callback: "/v1/auth/github/callback?code=<code>&state=<state>",
        auth_logout: "/v1/auth/logout",
        me: "/v1/me",
        tokens: "/v1/me/tokens",
        yank_release: "/v1/me/packages/<package-name>/versions/<version>/yank",
        search: "/v1/search?q=<query>",
        events: "/v1/events?limit=<count>&after=<event-id>",
        package_events: "/v1/packages/<package-name>/events?version=<version>&limit=<count>",
      },
      cdn_routes: {
        index_config: "/index/v1/config.json",
        index_package: "/index/v1/<sharded-package-document>.json",
        artifact_download: "/<artifact-key>",
        riot_latest_metadata: "/riot/latest.json",
        riot_release_metadata: "/riot/riot-<version>.json",
      },
      legacy_routes: {
        publish_artifact: "/api/v1/publish",
        index_config: "/api/v1/index/config.json",
        index_package: "/api/v1/index/<sharded-package-document>.json",
        artifact_download: "/api/v1/artifacts/<artifact-key>",
        riot_latest_metadata: "/api/v1/riot/latest.json",
        riot_release_metadata: "/api/v1/riot/riot-<version>.json",
        views_package_overview: "/api/v1/views/packages/<package-name>/overview",
        views_package_readme: "/api/v1/views/packages/<package-name>/readme?version=<version>",
        views_package_examples: "/api/v1/views/packages/<package-name>/examples?version=<version>",
        views_package_downloads: "/api/v1/views/packages/<package-name>/downloads",
        views_package_relations: "/api/v1/views/packages/<package-name>/relations",
        views_recent_packages: "/api/v1/views/recent/packages",
        views_popular_packages: "/api/v1/views/popular/packages",
        views_categories: "/api/v1/views/categories",
        views_owner_packages: "/api/v1/views/owners/<github-login>/packages",
        views_stats_summary: "/api/v1/views/stats/summary",
        views_stats_dashboard: "/api/v1/views/stats/dashboard",
        auth_github_start: "/auth/github/start?return_to=<url>",
        auth_github_callback: "/auth/github/callback?code=<code>&state=<state>",
        auth_logout: "/auth/logout",
        me: "/api/v1/me",
        tokens: "/api/v1/me/tokens",
        yank_release: "/api/v1/me/packages/<package-name>/versions/<version>/yank",
        search: "/api/v1/search?q=<query>",
        events: "/api/v1/events?limit=<count>&after=<event-id>",
        package_events: "/api/v1/packages/<package-name>/events?version=<version>&limit=<count>",
      },
      cdn_base_url: "https://cdn.pkgs.ml",
      index_base_url: "https://cdn.pkgs.ml/index/v1",
    });
  });

  test("removed source-resolution routes now return 404", async () => {
    const { env } = makeEnv();

    const responses = await Promise.all([
      handleRequest(
        new Request("https://registry.test/v1/packages/github.com/leostera/minttea/resolve?ref=main"),
        env,
        new FakeExecutionContext(),
      ),
      handleRequest(
        new Request("https://registry.test/package/leostera/minttea/-/publish?ref=main", {
          method: "POST",
          headers: { authorization: "Bearer root-secret" },
        }),
        env,
        new FakeExecutionContext(),
      ),
    ]);

    expect(responses[0].status).toBe(404);
    expect(responses[1].status).toBe(404);
  });

  test("index config route serves the sparse config through the api worker", async () => {
    const { env } = makeEnv();
    const ctx = new FakeExecutionContext();

    const response = await handleRequest(
      new Request("https://registry.test/v1/index/config.json"),
      env,
      ctx,
    );
    await ctx.drain();

    expect(response.status).toBe(200);
    expect(response.headers.get("cache-control")).toBe("no-store");
    expect(await readJson(response)).toEqual({
      schema_version: 1,
      kind: "sparse",
      package_path_strategy: "cargo-lowercase-v1",
      index_base_url: "https://cdn.pkgs.ml/index/v1",
      artifact_base_url: "https://cdn.pkgs.ml",
    });
  });

  test("artifact route proxies stored source archives through the api worker", async () => {
    const { env } = makeEnv();
    const publishResponse = await publishArtifact(
      env,
      await makePackageArtifact({
        packageName: "kernel",
        packageVersion: "0.0.1",
        description: "Kernel package",
        license: "Apache-2.0",
        files: {
          "src/kernel.ml": "let hello = \"world\"\n",
        },
      }),
    );
    expect(publishResponse.status).toBe(200);
    const publishPayload = await readJson(publishResponse) as PublishResponse;

    const response = await handleRequest(
      new Request(`https://registry.test/v1/artifacts/${publishPayload.source_archive.key}`),
      env,
      new FakeExecutionContext(),
    );

    expect(response.status).toBe(200);
    expect(response.headers.get("cache-control")).toBe("no-store");
    expect(response.headers.get("content-type")).toContain("application/gzip");
    const archiveBytes = new Uint8Array(await response.arrayBuffer());
    expect(await readArchiveFileFromTarGz(archiveBytes, "riot.toml")).toContain('name = "kernel"');
    expect(await readArchiveFileFromTarGz(archiveBytes, "src/kernel.ml")).toBe('let hello = "world"\n');
  });

  test("riot release metadata routes proxy stored latest and versioned release metadata", async () => {
    const { env, bucket } = makeEnv();

    const latestMetadata = {
      release_id: "v9.9.9",
      build_sha: "deadbeefcafe",
      notes_url: "https://example.test/notes",
      compare_url: "https://example.test/compare",
      issues_url: "https://example.test/issues",
    };
    const versionedMetadata = {
      release_id: "v9.9.8",
      build_sha: "feedface1234",
      notes_url: null,
      compare_url: null,
      issues_url: null,
    };

    await bucket.put("riot/latest.json", JSON.stringify(latestMetadata, null, 2), {
      httpMetadata: {
        contentType: "application/json; charset=utf-8",
      },
    });
    await bucket.put("riot/riot-v9.9.8.json", JSON.stringify(versionedMetadata, null, 2), {
      httpMetadata: {
        contentType: "application/json; charset=utf-8",
      },
    });

    const latestResponse = await handleRequest(
      new Request("https://registry.test/v1/riot/latest.json"),
      env,
      new FakeExecutionContext(),
    );
    expect(latestResponse.status).toBe(200);
    expect(latestResponse.headers.get("cache-control")).toBe("no-store");
    expect(await readJson(latestResponse)).toEqual(latestMetadata);

    const versionedResponse = await handleRequest(
      new Request("https://registry.test/v1/riot/riot-v9.9.8.json"),
      env,
      new FakeExecutionContext(),
    );
    expect(versionedResponse.status).toBe(200);
    expect(versionedResponse.headers.get("cache-control")).toBe("no-store");
    expect(await readJson(versionedResponse)).toEqual(versionedMetadata);
  });

  test("index package route serves the latest package document with etag revalidation", async () => {
    const { env, db } = makeEnv();
    const releaseResponse = await publishArtifact(
      env,
      await makePackageArtifact({
        packageName: "kernel",
        packageVersion: "0.0.1",
        description: "Kernel package",
        license: "Apache-2.0",
      }),
    );
    expect(releaseResponse.status).toBe(200);
    await applyMetadataMigrations(db as unknown as D1Database);

    const firstResponse = await handleRequest(
      new Request("https://registry.test/v1/index/ke/rn/kernel.json"),
      env,
      new FakeExecutionContext(),
    );

    expect(firstResponse.status).toBe(200);
    expect(firstResponse.headers.get("content-type")).toContain("application/json");
    expect(firstResponse.headers.get("cache-control")).toBe("no-store");

    const etag = firstResponse.headers.get("etag");
    expect(etag).not.toBeNull();
    expect(await readJson(firstResponse)).toMatchObject({
      name: "kernel",
      latest: "0.0.1",
    });

    const secondResponse = await handleRequest(
      new Request("https://registry.test/v1/index/ke/rn/kernel.json", {
        headers: {
          "if-none-match": etag ?? "",
        },
      }),
      env,
      new FakeExecutionContext(),
    );

    expect(secondResponse.status).toBe(304);
    expect(secondResponse.headers.get("etag")).toBe(etag);
  });

  test("index package route drops legacy source-snapshot releases from served documents", async () => {
    const { env, bucket } = makeEnv();

    await bucket.put(
      "index/v1/ke/rn/kernel.json",
      JSON.stringify({
        schema_version: 1,
        name: "kernel",
        latest: "0.2.0",
        updated_at: "2026-04-02T10:00:00.000Z",
        releases: [
          {
            version: "0.2.0",
            published_at: "2026-04-02T10:00:00.000Z",
            canonical_locator: "",
            repo_url: "",
            subdir: ".",
            artifact_sha256: "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
            manifest_key:
              "packages/github.com/leostera/riot-new/packages/kernel/bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb.manifest.json",
            source_key:
              "sources/github.com/leostera/riot-new/bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb.tar.gz",
            dependencies: [],
          },
          {
            version: "0.1.0",
            published_at: "2026-04-01T10:00:00.000Z",
            canonical_locator: "",
            repo_url: "",
            subdir: ".",
            artifact_sha256: "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
            manifest_key:
              "packages/kernel/0.1.0/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa.manifest.json",
            source_key:
              "sources/kernel/0.1.0/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa.tar.gz",
            dependencies: [],
          },
        ],
      }),
      {
        httpMetadata: {
          contentType: "application/json; charset=utf-8",
        },
      },
    );

    const response = await handleRequest(
      new Request("https://registry.test/v1/index/ke/rn/kernel.json"),
      env,
      new FakeExecutionContext(),
    );

    expect(response.status).toBe(200);
    expect(await readJson(response)).toEqual({
      schema_version: 1,
      name: "kernel",
      latest: "0.1.0",
      updated_at: "2026-04-01T10:00:00.000Z",
      releases: [
        {
          version: "0.1.0",
          published_at: "2026-04-01T10:00:00.000Z",
          canonical_locator: "",
          repo_url: "",
          subdir: ".",
          artifact_sha256: "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
          manifest_key:
            "packages/kernel/0.1.0/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa.manifest.json",
          source_key:
            "sources/kernel/0.1.0/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa.tar.gz",
          dependencies: [],
        },
      ],
    });
  });

  test("events route returns the latest registry events in reverse chronological order", async () => {
    const { env, db } = makeEnv();
    await applyMetadataMigrations(db as unknown as D1Database);
    await db.exec(`
      INSERT INTO registry_events (
        event_id,
        event_type,
        package_name,
        package_version,
        package_locator,
        payload_json,
        created_at
      ) VALUES
        (
          'evt-1',
          'package.submitted',
          'kernel',
          '0.0.1',
          '',
          '{"artifact_sha256":"sha-1"}',
          '2026-03-30T12:00:00.000Z'
        ),
        (
          'evt-2',
          'package.indexed',
          'kernel',
          '0.0.1',
          '',
          '{"artifact_sha256":"sha-1","latest":"0.0.1"}',
          '2026-03-30T12:01:00.000Z'
        )
    `);

    const response = await handleRequest(
      new Request("https://registry.test/v1/events?limit=1"),
      env,
      new FakeExecutionContext(),
    );

    expect(response.status).toBe(200);
    expect(await readJson(response)).toEqual({
      limit: 1,
      events: [
        {
          event_id: "evt-2",
          event_type: "package.indexed",
          package_name: "kernel",
          package_version: "0.0.1",
          package_locator: "",
          payload: {
            artifact_sha256: "sha-1",
            latest: "0.0.1",
          },
          created_at: "2026-03-30T12:01:00.000Z",
        },
      ],
    });
  });

  test("events route supports incremental polling via after event id", async () => {
    const { env, db } = makeEnv();
    await applyMetadataMigrations(db as unknown as D1Database);
    await db.exec(`
      INSERT INTO registry_events (
        event_id,
        event_type,
        package_name,
        package_version,
        package_locator,
        payload_json,
        created_at
      ) VALUES
        ('evt-1', 'package.submitted', 'kernel', '0.0.1', '', '{}', '2026-03-30T12:00:00.000Z'),
        ('evt-2', 'package.verified', 'kernel', '0.0.1', '', '{}', '2026-03-30T12:01:00.000Z'),
        ('evt-3', 'package.indexed', 'kernel', '0.0.1', '', '{}', '2026-03-30T12:02:00.000Z')
    `);

    const response = await handleRequest(
      new Request("https://registry.test/v1/events?after=evt-1&limit=10"),
      env,
      new FakeExecutionContext(),
    );

    expect(response.status).toBe(200);
    expect(await readJson(response)).toEqual({
      limit: 10,
      after: "evt-1",
      events: [
        expect.objectContaining({ event_id: "evt-2" }),
        expect.objectContaining({ event_id: "evt-3" }),
      ],
    });
  });

  test("package events route returns package timeline and supports version filtering", async () => {
    const { env, db } = makeEnv();
    await applyMetadataMigrations(db as unknown as D1Database);
    await db.exec(`
      INSERT INTO registry_events (
        event_id,
        event_type,
        package_name,
        package_version,
        package_locator,
        payload_json,
        created_at
      ) VALUES
        ('evt-1', 'package.submitted', 'kernel', '0.0.1', '', '{}', '2026-03-30T12:00:00.000Z'),
        ('evt-2', 'package.published', 'kernel', '0.0.1', '', '{}', '2026-03-30T12:01:00.000Z'),
        ('evt-3', 'package.published', 'kernel', '0.1.0', '', '{}', '2026-03-30T12:02:00.000Z')
    `);

    const response = await handleRequest(
      new Request("https://registry.test/v1/packages/kernel/events?version=0.0.1&limit=10"),
      env,
      new FakeExecutionContext(),
    );

    expect(response.status).toBe(200);
    expect(await readJson(response)).toEqual({
      package_name: "kernel",
      package_version: "0.0.1",
      limit: 10,
      events: [
        expect.objectContaining({ event_id: "evt-2", package_version: "0.0.1" }),
        expect.objectContaining({ event_id: "evt-1", package_version: "0.0.1" }),
      ],
    });
  });

  test("artifact publish stores a package-root tarball, claim, release, and events", async () => {
    const { env, bucket, db, queue, indexedQueue } = makeEnv();
    const response = await publishArtifact(
      env,
      await makePackageArtifact({
        packageName: "std",
        packageVersion: "0.1.0",
        description: "The Riot standard library",
        license: "Apache-2.0",
        files: {
          "src/std.ml": "let hello = \"riot\"\n",
          "README.md": "# std\n",
        },
      }),
    );

    expect(response.status).toBe(200);
    const payload = await readJson(response) as PublishResponse;
    expect(payload.package_name).toBe("std");
    expect(payload.package_version).toBe("0.1.0");
    expect(payload.artifact_sha256).toMatch(/^[0-9a-f]{64}$/);

    const sourceObject = await bucket.get(payload.source_archive.key);
    expect(sourceObject).not.toBeNull();
    const sourceBytes = new Uint8Array(await sourceObject!.arrayBuffer());
    expect(await readArchiveFileFromTarGz(sourceBytes, "riot.toml")).toContain('name = "std"');
    expect(await readArchiveFileFromTarGz(sourceBytes, "src/std.ml")).toBe('let hello = "riot"\n');
    expect(await readArchiveFileFromTarGz(sourceBytes, "repo-root/riot.toml")).toBeNull();

    await applyMetadataMigrations(db as unknown as D1Database);
    expect(await readPackageClaim(db as unknown as D1Database, "std")).toMatchObject({
      package_name: "std",
      package_locator: "",
    });
    expect(await readPublishedRelease(db as unknown as D1Database, "std", "0.1.0")).toMatchObject({
      package_name: "std",
      artifact_sha256: payload.artifact_sha256,
      source_archive_key: payload.source_archive.key,
      manifest_key: payload.manifest.key,
    });

    const events = await listPackageRegistryEvents(db as unknown as D1Database, "std", "0.1.0", 10);
    expect(events.map((event) => event.event_type)).toEqual([
      "package.indexed",
      "package.searchable",
      "package.published",
      "package.verified",
      "package.submitted",
    ]);
    expect(events[2]?.payload).toMatchObject({
      artifact_sha256: payload.artifact_sha256,
      release_created: true,
    });

    expect(queue.messages).toHaveLength(1);
    expect(indexedQueue.messages).toHaveLength(1);
  });

  test("package readme view exposes the versioned README markdown from the stored artifact", async () => {
    const { env } = makeEnv();
    const publishResponse = await publishArtifact(
      env,
      await makePackageArtifact({
        packageName: "std",
        packageVersion: "0.1.0",
        description: "The Riot standard library",
        license: "Apache-2.0",
        files: {
          "README.md": "# std\n\nThe Riot standard library.\n",
          "src/std.ml": "let hello = \"riot\"\n",
        },
      }),
    );
    expect(publishResponse.status).toBe(200);

    const response = await handleRequest(
      new Request("https://registry.test/v1/views/packages/std/readme?version=0.1.0"),
      env,
      new FakeExecutionContext(),
    );

    expect(response.status).toBe(200);
    expect(await readJson(response)).toMatchObject({
      package_name: "std",
      package_version: "0.1.0",
      readme_path: "README.md",
      readme_markdown: "# std\n\nThe Riot standard library.\n",
    });
  });

  test("package examples view exposes example sources from the stored artifact", async () => {
    const { env } = makeEnv();
    const publishResponse = await publishArtifact(
      env,
      await makePackageArtifact({
        packageName: "actors",
        packageVersion: "0.0.4",
        description: "The multicore actor runtime that powers Riot",
        license: "Apache-2.0",
        files: {
          "examples/ping_pong.ml": "let () = print_endline \"pong\"\n",
          "examples/support/format.ml": "let format value = value\n",
          "src/actors.ml": "let hello = \"actors\"\n",
        },
      }),
    );
    expect(publishResponse.status).toBe(200);

    const response = await handleRequest(
      new Request("https://registry.test/v1/views/packages/actors/examples?version=0.0.4"),
      env,
      new FakeExecutionContext(),
    );

    expect(response.status).toBe(200);
    expect(await readJson(response)).toEqual({
      schema_version: 1,
      package_name: "actors",
      package_version: "0.0.4",
      source_key: expect.stringContaining("sources/actors/0.0.4/"),
      examples: [
        {
          name: "ping_pong",
          path: "examples/ping_pong.ml",
          source_code: "let () = print_endline \"pong\"\n",
        },
        {
          name: "format",
          path: "examples/support/format.ml",
          source_code: "let format value = value\n",
        },
      ],
    });
  });

  test("artifact publish short-circuits duplicate package versions for the same artifact", async () => {
    const { env, db, queue, indexedQueue } = makeEnv();
    const archive = await makePackageArtifact({
      packageName: "std",
      packageVersion: "0.1.0",
      description: "The Riot standard library",
      license: "Apache-2.0",
    });

    const first = await publishArtifact(env, archive);
    const firstPayload = await readJson(first) as PublishResponse;
    const second = await publishArtifact(env, archive);

    expect(first.status).toBe(200);
    expect(second.status).toBe(200);
    expect(await readJson(second)).toMatchObject({
      package_name: "std",
      package_version: "0.1.0",
      artifact_sha256: firstPayload.artifact_sha256,
      claim: { created: false },
      release: { created: false },
    });

    await applyMetadataMigrations(db as unknown as D1Database);
    const events = await listPackageRegistryEvents(db as unknown as D1Database, "std", "0.1.0", 10);
    expect(events).toHaveLength(5);
    expect(queue.messages).toHaveLength(1);
    expect(indexedQueue.messages).toHaveLength(1);
  });

  test("artifact publish rejects republishing the same version with a different artifact", async () => {
    const { env } = makeEnv();
    const first = await publishArtifact(env, await makePackageArtifact({
      packageName: "std",
      packageVersion: "0.1.0",
      description: "The Riot standard library",
      license: "Apache-2.0",
      files: {
        "src/std.ml": "let hello = \"riot\"\n",
      },
    }));
    expect(first.status).toBe(200);

    const second = await publishArtifact(env, await makePackageArtifact({
      packageName: "std",
      packageVersion: "0.1.0",
      description: "The Riot standard library, revised",
      license: "Apache-2.0",
      files: {
        "src/std.ml": "let hello = \"riot again\"\n",
      },
    }));

    expect(second.status).toBe(409);
    expect(await readJson(second)).toMatchObject({
      error: "package_version_already_published",
    });
  });

  test("artifact publish rejects requests without root auth", async () => {
    const { env } = makeEnv();
    const response = await handleRequest(
      new Request("https://registry.test/v1/publish", {
        method: "POST",
        headers: {
          "content-type": "application/gzip",
        },
        body: await makePackageArtifact({
          packageName: "std",
          packageVersion: "0.1.0",
          description: "The Riot standard library",
          license: "Apache-2.0",
        }),
      }),
      env,
      new FakeExecutionContext(),
    );

    expect(response.status).toBe(401);
    expect(await readJson(response)).toMatchObject({
      error: "unauthorized",
    });
  });

  test("artifact publish rejects unsupported media types", async () => {
    const { env } = makeEnv();
    const response = await handleRequest(
      new Request("https://registry.test/v1/publish", {
        method: "POST",
        headers: {
          authorization: "Bearer root-secret",
          "content-type": "application/json",
        },
        body: JSON.stringify({ nope: true }),
      }),
      env,
      new FakeExecutionContext(),
    );

    expect(response.status).toBe(415);
    expect(await readJson(response)).toMatchObject({
      error: "unsupported_media_type",
    });
  });

  test("artifact publish rejects non-public packages", async () => {
    const { env, db, queue, indexedQueue } = makeEnv();
    const response = await publishArtifact(env, await makePackageArtifact({
      packageName: "std",
      packageVersion: "0.1.0",
      description: "The Riot standard library",
      license: "Apache-2.0",
      publicPackage: false,
    }));

    expect(response.status).toBe(422);
    expect(await readJson(response)).toMatchObject({
      error: "package_not_public",
    });

    await applyMetadataMigrations(db as unknown as D1Database);
    expect(await readPackageClaim(db as unknown as D1Database, "std")).toBeNull();
    expect(await readPublishedRelease(db as unknown as D1Database, "std", "0.1.0")).toBeNull();
    expect(queue.messages).toHaveLength(0);
    expect(indexedQueue.messages).toHaveLength(0);
  });

  test("artifact publish accepts a new version of an existing package name", async () => {
    const { env, db, queue, indexedQueue } = makeEnv();
    expect((await publishArtifact(env, await makePackageArtifact({
      packageName: "std",
      packageVersion: "0.1.0",
      description: "The Riot standard library",
      license: "Apache-2.0",
    }))).status).toBe(200);

    const second = await publishArtifact(env, await makePackageArtifact({
      packageName: "std",
      packageVersion: "0.2.0",
      description: "The Riot standard library",
      license: "Apache-2.0",
    }));

    expect(second.status).toBe(200);
    await applyMetadataMigrations(db as unknown as D1Database);
    expect(await readPublishedRelease(db as unknown as D1Database, "std", "0.2.0")).not.toBeNull();
    expect(queue.messages).toHaveLength(2);
    expect(indexedQueue.messages).toHaveLength(2);
  });

  test("owners can yank a release and latest moves to the highest non-yanked version", async () => {
    const { env, db } = makeEnv();
    const sessionCreatedAt = new Date().toISOString();
    const sessionExpiresAt = new Date(Date.now() + 7 * 24 * 60 * 60 * 1000).toISOString();
    expect((await publishArtifact(env, await makePackageArtifact({
      packageName: "std",
      packageVersion: "0.1.0",
      description: "The Riot standard library",
      license: "Apache-2.0",
    }))).status).toBe(200);
    expect((await publishArtifact(env, await makePackageArtifact({
      packageName: "std",
      packageVersion: "0.2.0",
      description: "The Riot standard library",
      license: "Apache-2.0",
    }))).status).toBe(200);

    await writeUserRecord(db as unknown as D1Database, {
      user_id: "user-1",
      github_id: 42,
      github_login: "leostera",
      github_name: "Leo Stera",
      created_at: sessionCreatedAt,
      updated_at: sessionCreatedAt,
    });
    await writeSessionRecord(db as unknown as D1Database, {
      session_id: "session-1",
      user_id: "user-1",
      github_login: "leostera",
      created_at: sessionCreatedAt,
      expires_at: sessionExpiresAt,
    });
    await writePackageClaim(db as unknown as D1Database, {
      package_name: "std",
      package_locator: "",
      source_url: "",
      package_subdir: ".",
      owner_user_id: "user-1",
      owner_github_login: "leostera",
      claimed_at: "2026-04-06T10:00:00.000Z",
      updated_at: "2026-04-06T10:00:00.000Z",
    });

    const cookie = buildSessionCookie(env, {
      session_id: "session-1",
      user_id: "user-1",
      github_login: "leostera",
      created_at: sessionCreatedAt,
      expires_at: sessionExpiresAt,
    });

    const yankResponse = await handleRequest(
      new Request("https://registry.test/v1/me/packages/std/versions/0.2.0/yank", {
        method: "POST",
        headers: {
          cookie,
          accept: "application/json",
        },
      }),
      env,
      new FakeExecutionContext(),
    );

    expect(yankResponse.status).toBe(200);
    expect(await readJson(yankResponse)).toMatchObject({
      package_name: "std",
      package_version: "0.2.0",
      yanked: true,
      yanked_by_github_login: "leostera",
    });

    const yankedRelease = await readPublishedRelease(db as unknown as D1Database, "std", "0.2.0");
    expect(yankedRelease?.yanked_at).toBeDefined();
    expect(yankedRelease?.yanked_by_github_login).toBe("leostera");

    const indexResponse = await handleRequest(
      new Request("https://registry.test/v1/index/3/s/std.json"),
      env,
      new FakeExecutionContext(),
    );
    expect(indexResponse.status).toBe(200);
    expect(await readJson(indexResponse)).toMatchObject({
      latest: "0.1.0",
      releases: [
        expect.objectContaining({
          version: "0.2.0",
          yanked: true,
          yanked_by_github_login: "leostera",
        }),
        expect.objectContaining({
          version: "0.1.0",
        }),
      ],
    });

    const ownerResponse = await handleRequest(
      new Request("https://registry.test/v1/views/owners/leostera/packages"),
      env,
      new FakeExecutionContext(),
    );
    expect(ownerResponse.status).toBe(200);
    const ownerPayload = await readJson(ownerResponse);
    expect(ownerPayload).toMatchObject({
      owner_github_login: "leostera",
      packages: [
        expect.objectContaining({
          package_name: "std",
          latest_version: "0.1.0",
          yanked_release_count: 1,
          releases: [
            expect.objectContaining({
              version: "0.2.0",
              yanked: true,
              yanked_by_github_login: "leostera",
              yanked_at: expect.any(String),
            }),
            expect.objectContaining({
              version: "0.1.0",
              yanked: false,
            }),
          ],
        }),
      ],
    });
  });

  test("artifact publish rejects packages without a description", async () => {
    const { env } = makeEnv();
    const response = await publishArtifact(env, await makePackageArtifact({
      packageName: "std",
      packageVersion: "0.1.0",
      license: "Apache-2.0",
      omitDescription: true,
    }));

    expect(response.status).toBe(422);
    expect(await readJson(response)).toMatchObject({
      error: "missing_package_description",
    });
  });

  test("artifact publish rejects packages with non-SPDX licenses", async () => {
    const { env } = makeEnv();
    const response = await publishArtifact(env, await makePackageArtifact({
      packageName: "std",
      packageVersion: "0.1.0",
      description: "The Riot standard library",
      license: "lolnope",
    }));

    expect(response.status).toBe(422);
    expect(await readJson(response)).toMatchObject({
      error: "invalid_package_license",
    });
  });

  test("artifact publish rejects packages whose semver dependencies have not been published yet", async () => {
    const { env } = makeEnv();
    const response = await publishArtifact(env, await makePackageArtifact({
      packageName: "std",
      packageVersion: "0.1.0",
      description: "The Riot standard library",
      license: "Apache-2.0",
      dependencyLines: [
        "[dependencies]",
        'kernel = "0.0.1"',
      ],
    }));

    expect(response.status).toBe(422);
    expect(await readJson(response)).toMatchObject({
      error: "missing_dependency",
    });
  });

  test("artifact publish accepts dependencies on already-published packages", async () => {
    const { env } = makeEnv();
    expect((await publishArtifact(env, await makePackageArtifact({
      packageName: "kernel",
      packageVersion: "0.0.1",
      description: "Kernel primitives",
      license: "Apache-2.0",
    }))).status).toBe(200);

    const response = await publishArtifact(env, await makePackageArtifact({
      packageName: "std",
      packageVersion: "0.1.0",
      description: "The Riot standard library",
      license: "Apache-2.0",
      dependencyLines: [
        "[dependencies]",
        'kernel = "0.0.1"',
      ],
    }));

    expect(response.status).toBe(200);
  });

  test("package overview and stats summary expose download counts", async () => {
    const { env, db } = makeEnv();

    await publishArtifact(env, await makePackageArtifact({
      packageName: "kernel",
      packageVersion: "0.0.1",
      description: "Kernel package",
      license: "Apache-2.0",
    }));

    await writePackageDownloadRecord(db as unknown as D1Database, {
      download_id: crypto.randomUUID(),
      package_name: "kernel",
      package_version: "0.0.1",
      artifact_sha256: "deadbeef",
      source_archive_key: "sources/kernel/0.0.1/deadbeef.tar.gz",
      riot_agent: "riot-docs-pipeline@1.0",
      downloaded_at: "2026-04-04T11:59:00.000Z",
    });
    await writePackageDownloadRecord(db as unknown as D1Database, {
      download_id: crypto.randomUUID(),
      package_name: "kernel",
      package_version: "0.0.1",
      artifact_sha256: "deadbeef",
      source_archive_key: "sources/kernel/0.0.1/deadbeef.tar.gz",
      riot_agent: "riot-cli@0.0.5",
      downloaded_at: "2026-04-04T12:00:00.000Z",
    });
    await writePackageDownloadRecord(db as unknown as D1Database, {
      download_id: crypto.randomUUID(),
      package_name: "kernel",
      package_version: "0.0.1",
      artifact_sha256: "deadbeef",
      source_archive_key: "sources/kernel/0.0.1/deadbeef.tar.gz",
      riot_agent: "riot-cli@0.0.5",
      downloaded_at: "2026-04-04T12:01:00.000Z",
    });
    await writeBinaryDownloadRecord(db as unknown as D1Database, {
      download_id: crypto.randomUUID(),
      binary_name: "riot",
      object_key: "riot/riot-latest-aarch64-apple-darwin.tar.gz",
      riot_agent: "riot-docs-pipeline@1.0",
      downloaded_at: "2026-04-04T12:01:30.000Z",
    });
    await writeBinaryDownloadRecord(db as unknown as D1Database, {
      download_id: crypto.randomUUID(),
      binary_name: "riot",
      object_key: "riot/riot-latest-aarch64-apple-darwin.tar.gz",
      riot_agent: "riot-cli@0.0.5",
      downloaded_at: "2026-04-04T12:02:00.000Z",
    });
    await writeBinaryDownloadRecord(db as unknown as D1Database, {
      download_id: crypto.randomUUID(),
      binary_name: "ocaml",
      object_key: "ocaml/ocaml-5.3.0-aarch64-apple-darwin.tar.gz",
      riot_agent: "riot-cli@0.0.5",
      downloaded_at: "2026-04-04T12:03:00.000Z",
    });

    const overview = await handleRequest(
      new Request("https://registry.test/v1/views/packages/kernel/overview"),
      env,
      new FakeExecutionContext(),
    );
    const stats = await handleRequest(
      new Request("https://registry.test/v1/views/stats/summary"),
      env,
      new FakeExecutionContext(),
    );

    expect(overview.status).toBe(200);
    expect(await readJson(overview)).toMatchObject({
      package_name: "kernel",
      download_count: 2,
    });

    expect(stats.status).toBe(200);
    expect(await readJson(stats)).toMatchObject({
      total_package_downloads: 2,
      total_riot_downloads: 1,
      total_ocaml_downloads: 1,
      total_packages: 1,
      total_versions: 1,
      total_users: 0,
    });
  });

  test("package downloads view exposes daily totals and per-version download counts", async () => {
    const { env, db } = makeEnv();

    await publishArtifact(env, await makePackageArtifact({
      packageName: "kernel",
      packageVersion: "0.0.1",
      description: "Kernel package",
      license: "Apache-2.0",
    }));
    await publishArtifact(env, await makePackageArtifact({
      packageName: "kernel",
      packageVersion: "0.1.0",
      description: "Kernel package",
      license: "Apache-2.0",
    }));

    await writePackageDownloadRecord(db as unknown as D1Database, {
      download_id: crypto.randomUUID(),
      package_name: "kernel",
      package_version: "0.0.1",
      artifact_sha256: "oldsha",
      source_archive_key: "sources/kernel/0.0.1/oldsha.tar.gz",
      riot_agent: "riot-docs-pipeline@1.0",
      downloaded_at: "2026-04-04T08:30:00.000Z",
    });
    await writePackageDownloadRecord(db as unknown as D1Database, {
      download_id: crypto.randomUUID(),
      package_name: "kernel",
      package_version: "0.0.1",
      artifact_sha256: "oldsha",
      source_archive_key: "sources/kernel/0.0.1/oldsha.tar.gz",
      riot_agent: "riot-cli@0.0.5",
      downloaded_at: "2026-04-04T09:00:00.000Z",
    });
    await writePackageDownloadRecord(db as unknown as D1Database, {
      download_id: crypto.randomUUID(),
      package_name: "kernel",
      package_version: "0.1.0",
      artifact_sha256: "newsha",
      source_archive_key: "sources/kernel/0.1.0/newsha.tar.gz",
      riot_agent: "riot-cli@0.0.5",
      downloaded_at: "2026-04-04T09:10:00.000Z",
    });
    await writePackageDownloadRecord(db as unknown as D1Database, {
      download_id: crypto.randomUUID(),
      package_name: "kernel",
      package_version: "0.1.0",
      artifact_sha256: "newsha",
      source_archive_key: "sources/kernel/0.1.0/newsha.tar.gz",
      riot_agent: "riot-cli@0.0.5",
      downloaded_at: "2026-04-04T10:00:00.000Z",
    });

    const response = await handleRequest(
      new Request("https://registry.test/v1/views/packages/kernel/downloads"),
      env,
      new FakeExecutionContext(),
    );

    expect(response.status).toBe(200);
    expect(await readJson(response)).toMatchObject({
      package_name: "kernel",
      latest_version: "0.1.0",
      total_downloads: 3,
      stacked_downloads: [
        {
          key: "0.1.0",
          label: "0.1.0",
          total_downloads: 2,
          is_latest: true,
          is_other: false,
        },
        {
          key: "0.0.1",
          label: "0.0.1",
          total_downloads: 1,
          is_latest: false,
          is_other: false,
        },
      ],
      version_downloads: [
        {
          version: "0.1.0",
          download_count: 2,
          is_latest: true,
        },
        {
          version: "0.0.1",
          download_count: 1,
          is_latest: false,
        },
      ],
    });
  });

  test("stats dashboard exposes timeseries, top packages, and recent releases", async () => {
    const { env, db } = makeEnv();

    await publishArtifact(env, await makePackageArtifact({
      packageName: "kernel",
      packageVersion: "0.0.1",
      description: "Kernel package",
      license: "Apache-2.0",
    }));
    await publishArtifact(env, await makePackageArtifact({
      packageName: "std",
      packageVersion: "0.1.0",
      description: "The Riot standard library",
      license: "Apache-2.0",
      dependencyLines: [
        "[dependencies]",
        'kernel = "0.0.1"',
      ],
    }));

    await writePackageDownloadRecord(db as unknown as D1Database, {
      download_id: crypto.randomUUID(),
      package_name: "kernel",
      package_version: "0.0.1",
      artifact_sha256: "kernelsha",
      source_archive_key: "sources/kernel/0.0.1/kernelsha.tar.gz",
      riot_agent: "riot-docs-pipeline@1.0",
      downloaded_at: "2026-04-04T08:45:00.000Z",
    });
    await writePackageDownloadRecord(db as unknown as D1Database, {
      download_id: crypto.randomUUID(),
      package_name: "kernel",
      package_version: "0.0.1",
      artifact_sha256: "kernelsha",
      source_archive_key: "sources/kernel/0.0.1/kernelsha.tar.gz",
      riot_agent: "riot-cli@0.0.5",
      downloaded_at: "2026-04-04T09:00:00.000Z",
    });
    await writePackageDownloadRecord(db as unknown as D1Database, {
      download_id: crypto.randomUUID(),
      package_name: "kernel",
      package_version: "0.0.1",
      artifact_sha256: "kernelsha",
      source_archive_key: "sources/kernel/0.0.1/kernelsha.tar.gz",
      riot_agent: "riot-cli@0.0.5",
      downloaded_at: "2026-04-04T09:15:00.000Z",
    });
    await writePackageDownloadRecord(db as unknown as D1Database, {
      download_id: crypto.randomUUID(),
      package_name: "std",
      package_version: "0.1.0",
      artifact_sha256: "stdsha",
      source_archive_key: "sources/std/0.1.0/stdsha.tar.gz",
      riot_agent: "riot-cli@0.0.5",
      downloaded_at: "2026-04-04T10:00:00.000Z",
    });
    await writeBinaryDownloadRecord(db as unknown as D1Database, {
      download_id: crypto.randomUUID(),
      binary_name: "riot",
      object_key: "riot/riot-v0.1.0-aarch64-apple-darwin.tar.gz",
      riot_agent: "riot-docs-pipeline@1.0",
      downloaded_at: "2026-04-04T10:30:00.000Z",
    });
    await writeBinaryDownloadRecord(db as unknown as D1Database, {
      download_id: crypto.randomUUID(),
      binary_name: "riot",
      object_key: "riot/riot-v0.1.0-aarch64-apple-darwin.tar.gz",
      riot_agent: "riot-cli@0.0.5",
      downloaded_at: "2026-04-04T11:00:00.000Z",
    });

    const response = await handleRequest(
      new Request("https://registry.test/v1/views/stats/dashboard"),
      env,
      new FakeExecutionContext(),
    );

    expect(response.status).toBe(200);
    const payload = await readJson(response) as {
      window: string;
      window_label: string;
      window_days: number;
      available_windows: Array<{ key: string; label: string }>;
      summary: {
        total_package_downloads: number;
        total_riot_downloads: number;
        total_ocaml_downloads: number;
        total_packages: number;
        total_versions: number;
        total_users: number;
        total_index_reads: number;
        mean_package_downloads_per_package: number;
      };
      metrics: Array<{ key: string; total: number }>;
      top_packages: unknown[];
      latest_releases: unknown[];
    };

    expect(payload.window).toBe("30d");
    expect(payload.window_label).toBe("Last 30 days");
    expect(payload.window_days).toBe(30);
    expect(payload.available_windows).toEqual([
      { key: "all", label: "All time" },
      { key: "year", label: "This year" },
      { key: "30d", label: "Last 30 days" },
      { key: "7d", label: "This week" },
    ]);
    expect(payload.summary).toMatchObject({
      total_package_downloads: 3,
      total_riot_downloads: 1,
      total_ocaml_downloads: 0,
      total_packages: 2,
      total_versions: 2,
      total_users: 0,
      total_index_reads: 0,
      mean_package_downloads_per_package: 1.5,
    });
    expect(payload.metrics.map((metric: { key: string; total: number }) => [metric.key, metric.total])).toEqual([
      ["package_downloads", 3],
      ["riot_downloads", 1],
      ["ocaml_downloads", 0],
      ["index_reads", 0],
      ["releases_published", 2],
    ]);
    expect(payload.top_packages).toMatchObject([
      {
        package_name: "kernel",
        latest_version: "0.0.1",
        download_count: 2,
        package_path: "/p/kernel",
      },
      {
        package_name: "std",
        latest_version: "0.1.0",
        download_count: 1,
        package_path: "/p/std",
      },
    ]);
    expect(payload.latest_releases).toMatchObject([
      {
        package_name: "std",
        package_version: "0.1.0",
        package_path: "/p/std/0.1.0",
      },
      {
        package_name: "kernel",
        package_version: "0.0.1",
        package_path: "/p/kernel/0.0.1",
      },
    ]);
  });

  test("artifact publish accepts built-in OCaml dependencies without registry lookup", async () => {
    const { env } = makeEnv();
    const response = await publishArtifact(env, await makePackageArtifact({
      packageName: "std",
      packageVersion: "0.1.0",
      description: "The Riot standard library",
      license: "Apache-2.0",
      dependencyLines: [
        "[dependencies]",
        'stdlib = "latest"',
        'unix = "latest"',
      ],
    }));

    expect(response.status).toBe(200);
  });

  test("artifact publish accepts git-based dependencies without registry lookup", async () => {
    const { env } = makeEnv();
    const response = await publishArtifact(env, await makePackageArtifact({
      packageName: "std",
      packageVersion: "0.1.0",
      description: "The Riot standard library",
      license: "Apache-2.0",
      dependencyLines: [
        "[dependencies]",
        'minttea = { github = "leostera/minttea" }',
      ],
    }));

    expect(response.status).toBe(200);
  });

  test("artifact publish rejects invalid dependency references", async () => {
    const { env } = makeEnv();
    const response = await publishArtifact(env, await makePackageArtifact({
      packageName: "std",
      packageVersion: "0.1.0",
      description: "The Riot standard library",
      license: "Apache-2.0",
      dependencyLines: [
        "[dependencies]",
        'minttea = { github = "" }',
      ],
    }));

    expect(response.status).toBe(422);
    expect(await readJson(response)).toMatchObject({
      error: "invalid_dependency_reference",
    });
  });
});

interface MakePackageArtifactOptions {
  packageName: string;
  packageVersion: string;
  description?: string;
  license?: string;
  publicPackage?: boolean;
  omitDescription?: boolean;
  dependencyLines?: string[];
  files?: Record<string, string>;
}

interface PublishResponse {
  package_name: string;
  package_version: string;
  artifact_sha256: string;
  manifest: {
    key: string;
    url?: string;
    cdn_url?: string;
  };
  source_archive: {
    key: string;
    url?: string;
    cdn_url?: string;
  };
  claim: {
    created: boolean;
  };
  release: {
    created: boolean;
  };
}

async function publishArtifact(env: ReturnType<typeof makeEnv>["env"], archive: Uint8Array<ArrayBuffer>) {
  const ctx = new FakeExecutionContext();
  const response = await handleRequest(
    new Request("https://registry.test/v1/publish", {
      method: "POST",
      headers: {
        authorization: "Bearer root-secret",
        "content-type": "application/gzip",
      },
      body: archive,
    }),
    env,
    ctx,
  );
  await ctx.drain();
  return response;
}

async function makePackageArtifact(options: MakePackageArtifactOptions): Promise<Uint8Array<ArrayBuffer>> {
  const packageLines = [
    "[package]",
    `name = "${options.packageName}"`,
    `version = "${options.packageVersion}"`,
    `public = ${options.publicPackage === false ? "false" : "true"}`,
  ];

  if (!options.omitDescription) {
    packageLines.push(`description = "${options.description ?? "Package description"}"`);
  }

  packageLines.push(`license = "${options.license ?? "Apache-2.0"}"`);

  const riotToml = [
    ...packageLines,
    ...(options.dependencyLines ?? []),
  ].join("\n");

  return await makeTarGz({
    "riot.toml": riotToml,
    ...(options.files ?? {}),
  }, "");
}

async function readJson(response: Response): Promise<any> {
  return JSON.parse(await response.text());
}
