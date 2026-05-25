import { describe, expect, test } from "bun:test";

import { indexPublishedRelease } from "../src/indexing.ts";
import { handleRequest } from "../src/routes.ts";
import { applyMetadataMigrations, writePackageClaim, writePublishedRelease } from "../src/metadata-db.ts";
import type { PackagePublicationManifest, PublishedReleaseRecord } from "../src/types.ts";
import { FakeExecutionContext, makeEnv } from "./helpers.ts";

describe("registry search api", () => {
  test("search route returns metadata when q is absent", async () => {
    const { env } = makeEnv();
    const response = await handleRequest(
      new Request("https://registry.test/v1/search"),
      env,
      new FakeExecutionContext(),
    );

    expect(response.status).toBe(200);
    const payload = (await response.json()) as Record<string, unknown>;
    expect(payload).toEqual({
      service: "riot-package-registry",
      route: "/v1/search?q=<query>",
      source: {
        package_index_base_url: "https://cdn.pkgs.ml/index/v1",
        updated_during_publish: true,
      },
    });
  });

  test("indexed packages are searchable by exact name and provenance", async () => {
    const { env } = makeEnv();
    await indexPublishedRelease(env, makeReleaseRecord());

    const exact = await handleRequest(
      new Request("https://registry.test/v1/search?q=kernel"),
      env,
      new FakeExecutionContext(),
    );
    expect(exact.status).toBe(200);
    expect(await exact.json()).toMatchObject({
      query: "kernel",
      count: 1,
      results: [
        {
          package_name: "kernel",
          latest_version: "0.0.1",
          description: "Actor runtime kernel primitives for Riot",
          repo_owner: "leostera",
          repo_name: "riot-new",
          subdir: "packages/kernel",
          release_count: 1,
        },
      ],
    });

    const provenance = await handleRequest(
      new Request("https://registry.test/v1/search?q=leostera"),
      env,
      new FakeExecutionContext(),
    );
    expect(provenance.status).toBe(200);
    expect(await provenance.json()).toMatchObject({
      count: 1,
      results: [{ package_name: "kernel" }],
    });
  });

  test("search miss returns an empty result set instead of throwing", async () => {
    const { env } = makeEnv();
    await indexPublishedRelease(env, makeReleaseRecord());

    const response = await handleRequest(
      new Request("https://registry.test/v1/search?q=definitely-not-a-package-12345"),
      env,
      new FakeExecutionContext(),
    );

    expect(response.status).toBe(200);
    const payload = (await response.json()) as Record<string, unknown>;
    expect(payload).toEqual({
      query: "definitely-not-a-package-12345",
      count: 0,
      results: [],
    });
  });

  test("artifact-published packages remain searchable without canonical locator provenance", async () => {
    const { env, db } = makeEnv();
    const release = makeReleaseRecord({
      package_name: "std",
      package_version: "0.1.0",
      package_locator: "",
      source_url: "",
      package_subdir: ".",
      artifact_sha256: "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
      package_description: "The Riot standard library",
      package_root_module: "Std",
      dependencies: [],
      source_archive_key:
        "sources/std/0.1.0/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa.tar.gz",
      manifest_key:
        "packages/std/0.1.0/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa.manifest.json",
      published_at: "2026-04-01T09:00:00Z",
    });

    await applyMetadataMigrations(db as unknown as D1Database);
    await writePackageClaim(db as unknown as D1Database, {
      package_name: release.package_name,
      package_locator: "",
      source_url: "",
      package_subdir: ".",
      owner_github_login: "leostera",
      claimed_at: release.published_at,
      updated_at: release.published_at,
    });
    await writePublishedRelease(db as unknown as D1Database, release);
    await indexPublishedRelease(env, release);

    const byName = await handleRequest(
      new Request("https://registry.test/v1/search?q=std"),
      env,
      new FakeExecutionContext(),
    );
    expect(byName.status).toBe(200);
    expect(await byName.json()).toMatchObject({
      count: 1,
      results: [
        {
          package_name: "std",
          repo_owner: "leostera",
          repo_name: "",
          canonical_locator: "",
        },
      ],
    });

    const byOwner = await handleRequest(
      new Request("https://registry.test/v1/search?q=leostera"),
      env,
      new FakeExecutionContext(),
    );
    expect(byOwner.status).toBe(200);
    expect(await byOwner.json()).toMatchObject({
      count: 1,
      results: [{ package_name: "std" }],
    });
  });
});

function makeReleaseRecord(
  overrides: Partial<PublishedReleaseRecord> = {},
): PublishedReleaseRecord {
  return {
    package_name: "kernel",
    package_version: "0.0.1",
    package_locator: "github.com/leostera/riot-new/packages/kernel",
    source_url: "https://github.com/leostera/riot-new",
    package_subdir: "packages/kernel",
    artifact_sha256: "2aef0372bf5b6687db05bda80cde55f960cbfd9d",
    package_description: "Actor runtime kernel primitives for Riot",
    package_license: "Apache-2.0",
    package_homepage: "https://riot.ml",
    package_repository: "https://github.com/leostera/riot-new",
    package_root_module: "Kernel",
    dependencies: [{ name: "std", path: "../std" }],
    source_archive_key: "sources/github.com/leostera/riot-new/2aef0372bf5b6687db05bda80cde55f960cbfd9d.tar.gz",
    manifest_key:
      "packages/github.com/leostera/riot-new/packages/kernel/2aef0372bf5b6687db05bda80cde55f960cbfd9d.manifest.json",
    published_at: "2026-03-27T15:27:35Z",
    ...overrides,
  };
}

function makeManifest(
  overrides: Partial<PackagePublicationManifest> = {},
): PackagePublicationManifest {
  const release = makeReleaseRecord();

  return {
    package_locator: release.package_locator,
    source_url: release.source_url,
    package_subdir: release.package_subdir,
    artifact_sha256: release.artifact_sha256,
    package_name: release.package_name,
    package_version: release.package_version,
    package_public: true,
    package_description: release.package_description,
    package_license: release.package_license,
    package_homepage: release.package_homepage,
    package_repository: release.package_repository,
    package_root_module: release.package_root_module,
    dependencies: release.dependencies,
    source_archive_key: release.source_archive_key,
    manifest_key: release.manifest_key,
    materialized_at: "2026-03-27T15:20:00Z",
    ...overrides,
  };
}
