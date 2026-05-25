import { describe, expect, test } from "bun:test";

import { getConfig } from "../src/config.ts";
import { indexPublishedRelease } from "../src/indexing.ts";
import {
  applyMetadataMigrations,
  readCategoriesIndexDocument,
  listPackageRegistryEvents,
  readOwnerPackagesDocument,
  readPackageOverviewDocument,
  readPackageRelationsDocument,
  readPopularPackagesDocument,
  readRecentPackagesDocument,
  writePackageClaim,
  writePublishedRelease,
} from "../src/metadata-db.ts";
import {
  indexConfigKey,
  packageIndexKey,
} from "../src/storage.ts";
import type {
  CategoriesIndexDocument,
  OwnerPackagesDocument,
  PackageIndexDocument,
  PackagePublicationManifest,
  PackageOverviewDocument,
  PackageRelationsDocument,
  PopularPackagesDocument,
  PublishedReleaseRecord,
  RecentPackagesDocument,
} from "../src/types.ts";
import { makeEnv } from "./helpers.ts";

describe("registry indexing", () => {
  test("indexing a published release writes config, package doc, and package.indexed", async () => {
    const { env, bucket, db, indexedQueue } = makeEnv();
    const release = makeReleaseRecord();
    const manifest = makeManifest(release);

    const result = await indexSeededRelease(env, db, release, manifest);

    expect(result).toEqual({
      changed: true,
      latest: "0.0.1",
      indexedAt: "2026-03-27T15:27:35Z",
      packageIndexKey: "index/v1/ke/rn/kernel.json",
      packageIndexUrl: "https://cdn.pkgs.ml/index/v1/ke/rn/kernel.json",
    });

    const config = getConfig(env);
    expect(await bucket.text(indexConfigKey(config))).not.toBeNull();

    const document = JSON.parse(
      (await bucket.text(packageIndexKey(config, "kernel"))) ?? "null",
    ) as PackageIndexDocument;

    expect(document).toEqual({
      schema_version: 1,
      name: "kernel",
      latest: "0.0.1",
      updated_at: "2026-03-27T15:27:35Z",
      releases: [
        {
          version: "0.0.1",
          published_at: "2026-03-27T15:27:35Z",
          canonical_locator: "github.com/leostera/riot-new/packages/kernel",
          repo_url: "https://github.com/leostera/riot-new",
          subdir: "packages/kernel",
          artifact_sha256: "2aef0372bf5b6687db05bda80cde55f960cbfd9d",
          description: "Actor runtime kernel primitives for Riot",
          license: "Apache-2.0",
          homepage: "https://riot.ml",
          repository: "https://github.com/leostera/riot-new",
          root_module: "Kernel",
          manifest_key:
            "packages/github.com/leostera/riot-new/packages/kernel/2aef0372bf5b6687db05bda80cde55f960cbfd9d.manifest.json",
          source_key:
            "sources/github.com/leostera/riot-new/2aef0372bf5b6687db05bda80cde55f960cbfd9d.tar.gz",
          dependencies: [{ name: "std", path: "../std" }],
        },
      ],
    });

    expect(indexedQueue.messages).toEqual([
      {
        type: "package.indexed",
        package_name: "kernel",
        package_version: "0.0.1",
        package_locator: "github.com/leostera/riot-new/packages/kernel",
        artifact_sha256: "2aef0372bf5b6687db05bda80cde55f960cbfd9d",
        package_index_key: "index/v1/ke/rn/kernel.json",
        package_index_url: "https://cdn.pkgs.ml/index/v1/ke/rn/kernel.json",
        latest: "0.0.1",
        indexed_at: "2026-03-27T15:27:35Z",
      },
    ]);

    await applyMetadataMigrations(db as unknown as D1Database);
    const overview = await readPackageOverviewDocument(db as unknown as D1Database, "kernel") as PackageOverviewDocument;
    expect(overview).toMatchObject({
      package_name: "kernel",
      latest_version: "0.0.1",
      owner_github_login: "leostera",
      dependency_count: 1,
      dependent_count: 0,
      download_count: 0,
      categories: [],
    });

    const relations = await readPackageRelationsDocument(db as unknown as D1Database, "kernel") as PackageRelationsDocument;
    expect(relations).toEqual({
      schema_version: 1,
      package_name: "kernel",
      updated_at: "2026-03-27T15:27:35Z",
      dependencies: [{ package_name: "std", requirement: "unspecified" }],
      dependents: [],
    });

    const recent = await readRecentPackagesDocument(db as unknown as D1Database) as RecentPackagesDocument;
    expect(recent.packages).toHaveLength(1);
    expect(recent.packages[0]).toMatchObject({
      package_name: "kernel",
      latest_version: "0.0.1",
      owner_github_login: "leostera",
    });

    const popular = await readPopularPackagesDocument(db as unknown as D1Database) as PopularPackagesDocument;
    expect(popular.packages).toHaveLength(1);
    expect(popular.packages[0]).toMatchObject({
      package_name: "kernel",
      dependent_count: 0,
      release_count: 1,
    });

    const categories = await readCategoriesIndexDocument(db as unknown as D1Database) as CategoriesIndexDocument;
    expect(categories.categories).toEqual([]);

    const ownerPackages = await readOwnerPackagesDocument(
      db as unknown as D1Database,
      "leostera",
    ) as OwnerPackagesDocument;
    expect(ownerPackages).toMatchObject({
      owner_github_login: "leostera",
      package_count: 1,
      packages: [
        {
          package_name: "kernel",
          latest_version: "0.0.1",
          repo_url: "https://github.com/leostera/riot-new",
          release_count: 1,
        },
      ],
    });

    const events = await listPackageRegistryEvents(
      db as unknown as D1Database,
      "kernel",
      "0.0.1",
    );
    expect(events.map((event) => event.event_type)).toEqual([
      "package.indexed",
      "package.searchable",
    ]);
    expect(events[0]).toMatchObject({
      payload: {
        latest: "0.0.1",
        package_index_key: "index/v1/ke/rn/kernel.json",
        changed: true,
      },
    });
  });

  test("higher versions become latest and releases stay semver-sorted", async () => {
    const { env, bucket, db, indexedQueue } = makeEnv();
    const first = makeReleaseRecord();
    const second = makeReleaseRecord({
      package_version: "0.2.0",
      artifact_sha256: "eeee0372bf5b6687db05bda80cde55f960cbfd9d",
      source_archive_key:
        "sources/github.com/leostera/riot-new/eeee0372bf5b6687db05bda80cde55f960cbfd9d.tar.gz",
      manifest_key:
        "packages/github.com/leostera/riot-new/packages/kernel/eeee0372bf5b6687db05bda80cde55f960cbfd9d.manifest.json",
      published_at: "2026-03-27T16:00:00Z",
    });

    await indexSeededRelease(env, db, first, makeManifest(first));
    await indexSeededRelease(env, db, second, makeManifest(second));

    const document = JSON.parse(
      (await bucket.text(packageIndexKey(getConfig(env), "kernel"))) ?? "null",
    ) as PackageIndexDocument;

    expect(document.latest).toBe("0.2.0");
    expect(document.updated_at).toBe("2026-03-27T16:00:00Z");
    expect(document.releases.map((release) => release.version)).toEqual(["0.2.0", "0.0.1"]);
    expect(indexedQueue.messages).toHaveLength(2);
  });

  test("reprocessing the same release is idempotent and still emits package.indexed for downstream consumers", async () => {
    const { env, bucket, db, indexedQueue } = makeEnv();
    const release = makeReleaseRecord();
    const manifest = makeManifest(release);

    const first = await indexSeededRelease(env, db, release, manifest);
    const second = await indexSeededRelease(env, db, release, manifest);

    const document = JSON.parse(
      (await bucket.text(packageIndexKey(getConfig(env), "kernel"))) ?? "null",
    ) as PackageIndexDocument;

    expect(first.changed).toBe(true);
    expect(second.changed).toBe(false);
    expect(document.releases).toHaveLength(1);
    expect(indexedQueue.messages).toHaveLength(2);
    expect(indexedQueue.messages[1]).toMatchObject({
      type: "package.indexed",
      package_name: "kernel",
      package_version: "0.0.1",
      package_locator: "github.com/leostera/riot-new/packages/kernel",
      artifact_sha256: "2aef0372bf5b6687db05bda80cde55f960cbfd9d",
      package_index_key: "index/v1/ke/rn/kernel.json",
      package_index_url: "https://cdn.pkgs.ml/index/v1/ke/rn/kernel.json",
      latest: "0.0.1",
      indexed_at: "2026-03-27T15:27:35Z",
    });
  });

  test("reindexing the same version replaces stale release metadata", async () => {
    const { env, bucket, db } = makeEnv();
    const release = makeReleaseRecord();
    const manifest = makeManifest(release);

    await indexSeededRelease(env, db, release, manifest);

    await bucket.put(
      packageIndexKey(getConfig(env), "kernel"),
      JSON.stringify({
        schema_version: 1,
        name: "kernel",
        latest: "0.0.1",
        updated_at: "2026-03-27T15:27:35Z",
        releases: [
          {
            version: "0.0.1",
            published_at: "2026-03-27T15:27:35Z",
            canonical_locator: "github.com/leostera/riot-new/packages/kernel",
            repo_url: "https://github.com/leostera/riot-new",
            subdir: "packages/kernel",
            artifact_sha256: "different",
            description: "Old description",
            license: "MIT",
            manifest_key: manifest.manifest_key,
            source_key: release.source_archive_key,
            dependencies: [{ name: "std", path: "../std" }],
          },
        ],
      }),
      {
        httpMetadata: {
          contentType: "application/json; charset=utf-8",
        },
      },
    );

    const result = await indexSeededRelease(env, db, release, manifest);

    expect(result.changed).toBe(true);

    const document = JSON.parse(
      (await bucket.text(packageIndexKey(getConfig(env), "kernel"))) ?? "null",
    ) as PackageIndexDocument;

    expect(document.releases).toHaveLength(1);
    expect(document.releases[0]).toMatchObject({
      version: "0.0.1",
      artifact_sha256: "2aef0372bf5b6687db05bda80cde55f960cbfd9d",
      description: "Actor runtime kernel primitives for Riot",
      license: "Apache-2.0",
    });
  });

  test("derived package view documents track dependents and categories across indexed packages", async () => {
    const { env, bucket, db } = makeEnv();
    const kernel = makeReleaseRecord({
      package_categories: ["runtime", "concurrency"],
    });
    const actors = makeReleaseRecord({
      package_name: "actors",
      package_version: "0.0.1",
      package_locator: "github.com/leostera/riot-new/packages/actors",
      package_description: "Actor runtime building blocks.",
      package_root_module: "Actors",
      dependencies: [{ name: "kernel", raw: "^0.0.1" }],
      source_archive_key:
        "sources/github.com/leostera/riot-new/abcf0372bf5b6687db05bda80cde55f960cbfd9d.tar.gz",
      manifest_key:
        "packages/github.com/leostera/riot-new/packages/actors/abcf0372bf5b6687db05bda80cde55f960cbfd9d.manifest.json",
      artifact_sha256: "abcf0372bf5b6687db05bda80cde55f960cbfd9d",
      published_at: "2026-03-27T16:10:00Z",
      package_categories: ["runtime", "actors"],
    });

    await indexSeededRelease(
      env,
      db,
      kernel,
      makeManifest(kernel, { package_categories: ["runtime", "concurrency"] }),
    );
    await indexSeededRelease(
      env,
      db,
      actors,
      makeManifest(actors, { package_categories: ["runtime", "actors"] }),
    );

    await applyMetadataMigrations(db as unknown as D1Database);
    const kernelRelations = await readPackageRelationsDocument(
      db as unknown as D1Database,
      "kernel",
    ) as PackageRelationsDocument;
    expect(kernelRelations.dependents).toEqual([
      {
        package_name: "actors",
        latest_version: "0.0.1",
        requirement: "^0.0.1",
      },
    ]);

    const categories = await readCategoriesIndexDocument(db as unknown as D1Database) as CategoriesIndexDocument;
    expect(categories.categories).toEqual([
      {
        name: "runtime",
        slug: "runtime",
        package_count: 2,
        packages: ["actors", "kernel"],
      },
      {
        name: "actors",
        slug: "actors",
        package_count: 1,
        packages: ["actors"],
      },
      {
        name: "concurrency",
        slug: "concurrency",
        package_count: 1,
        packages: ["kernel"],
      },
    ]);

    const ownerPackages = await readOwnerPackagesDocument(
      db as unknown as D1Database,
      "leostera",
    ) as OwnerPackagesDocument;
    expect(ownerPackages.packages.map((item) => item.package_name)).toEqual(["actors", "kernel"]);
  });

  test("derived package view documents tolerate legacy non-semver releases", async () => {
    const { db } = makeEnv();
    const legacy = makeReleaseRecord({
      package_version: "main",
      artifact_sha256: "11110372bf5b6687db05bda80cde55f960cbfd9d",
      source_archive_key:
        "sources/github.com/leostera/riot-new/11110372bf5b6687db05bda80cde55f960cbfd9d.tar.gz",
      manifest_key:
        "packages/github.com/leostera/riot-new/packages/kernel/11110372bf5b6687db05bda80cde55f960cbfd9d.manifest.json",
      published_at: "2026-03-27T15:00:00Z",
      package_categories: ["runtime"],
    });
    const current = makeReleaseRecord({
      package_version: "0.1.0",
      artifact_sha256: "22220372bf5b6687db05bda80cde55f960cbfd9d",
      source_archive_key:
        "sources/github.com/leostera/riot-new/22220372bf5b6687db05bda80cde55f960cbfd9d.tar.gz",
      manifest_key:
        "packages/github.com/leostera/riot-new/packages/kernel/22220372bf5b6687db05bda80cde55f960cbfd9d.manifest.json",
      published_at: "2026-03-27T16:00:00Z",
      package_categories: ["runtime", "concurrency"],
    });

    await applyMetadataMigrations(db as unknown as D1Database);
    await writePackageClaim(db as unknown as D1Database, {
      package_name: "kernel",
      package_locator: current.package_locator,
      source_url: current.source_url,
      package_subdir: current.package_subdir,
      owner_github_login: "leostera",
      claimed_at: legacy.published_at,
      updated_at: current.published_at,
    });
    await writePublishedRelease(db as unknown as D1Database, legacy);
    await writePublishedRelease(db as unknown as D1Database, current);

    const overview = await readPackageOverviewDocument(
      db as unknown as D1Database,
      "kernel",
    ) as PackageOverviewDocument;
    expect(overview.latest_version).toBe("0.1.0");
    expect(overview.categories).toEqual(["runtime", "concurrency"]);

    const categories = await readCategoriesIndexDocument(
      db as unknown as D1Database,
    ) as CategoriesIndexDocument;
    expect(categories.categories).toEqual([
      {
        name: "concurrency",
        slug: "concurrency",
        package_count: 1,
        packages: ["kernel"],
      },
      {
        name: "runtime",
        slug: "runtime",
        package_count: 1,
        packages: ["kernel"],
      },
    ]);
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
    package_categories: undefined,
    package_keywords: undefined,
    dependencies: [{ name: "std", path: "../std" }],
    source_archive_key: "sources/github.com/leostera/riot-new/2aef0372bf5b6687db05bda80cde55f960cbfd9d.tar.gz",
    manifest_key:
      "packages/github.com/leostera/riot-new/packages/kernel/2aef0372bf5b6687db05bda80cde55f960cbfd9d.manifest.json",
    published_at: "2026-03-27T15:27:35Z",
    ...overrides,
  };
}

function makeManifest(
  release: PublishedReleaseRecord,
  overrides: Partial<PackagePublicationManifest> = {},
): PackagePublicationManifest {
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
    package_categories: release.package_categories,
    package_keywords: release.package_keywords,
    dependencies: release.dependencies,
    source_archive_key: release.source_archive_key,
    manifest_key: release.manifest_key,
    materialized_at: "2026-03-27T15:20:00Z",
    ...overrides,
  };
}

async function indexSeededRelease(
  env: Parameters<typeof indexPublishedRelease>[0],
  db: D1Database,
  release: PublishedReleaseRecord,
  _manifest: PackagePublicationManifest,
) {
  await applyMetadataMigrations(db);
  await writePackageClaim(db, {
    package_name: release.package_name,
    package_locator: release.package_locator,
    source_url: release.source_url,
    package_subdir: release.package_subdir,
    owner_github_login: "leostera",
    claimed_at: release.published_at,
    updated_at: release.published_at,
  });
  await writePublishedRelease(db, release);
  return await indexPublishedRelease(env, release);
}
