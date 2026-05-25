import { getConfig } from "./config.ts";
import { buildIndexedRelease, upsertPackageDocument } from "./index-document.ts";
import { readPackageClaim, readPublishedRelease, writeRegistryEvent } from "./metadata-db.ts";
import { upsertSearchRow } from "./search-db.ts";
import { buildSearchRow } from "./search-document.ts";
import { v7 as uuidv7 } from "uuid";
import {
  packageIndexKey,
  packageIndexUrl,
  readPackageIndexDocument,
  writeIndexConfig,
  writePackageIndexDocument,
} from "./storage.ts";
import type {
  Env,
  PackageIndexedEvent,
  PublishedReleaseRecord,
} from "./types.ts";

export async function indexPublishedRelease(
  env: Env,
  releaseRecord: PublishedReleaseRecord,
): Promise<{
  changed: boolean;
  latest: string;
  indexedAt: string;
  packageIndexKey: string;
  packageIndexUrl: string;
}> {
  const config = getConfig(env);
  await writeIndexConfig(env.ML_PKGS_CDN, config);

  const currentDocument = await readPackageIndexDocument(
    env.ML_PKGS_CDN,
    config,
    releaseRecord.package_name,
  );

  const release = buildIndexedRelease(releaseRecord);
  const { document, changed } = upsertPackageDocument({
    existing: currentDocument,
    packageName: releaseRecord.package_name,
    release,
    updatedAt: releaseRecord.published_at,
  });

  const key = packageIndexKey(config, releaseRecord.package_name);
  const url = packageIndexUrl(config, releaseRecord.package_name);
  const claim = await readPackageClaim(env.SEARCH_DB, releaseRecord.package_name);
  const searchRow = buildSearchRow(document, {
    ownerGithubLogin: claim?.owner_github_login,
  });
  const searchableAt = addMilliseconds(document.updated_at, 1);
  const indexedAt = addMilliseconds(document.updated_at, 2);

  if (!changed) {
    await upsertSearchRow(env.SEARCH_DB, searchRow);
    await writeRegistryEvent(
      env.SEARCH_DB,
      makeIndexEvent("package.searchable", releaseRecord, searchableAt, {
        latest: document.latest,
        package_index_key: key,
        package_index_url: url,
        changed: false,
      }),
    );
    await writeRegistryEvent(
      env.SEARCH_DB,
      makeIndexEvent("package.indexed", releaseRecord, indexedAt, {
        latest: document.latest,
        package_index_key: key,
        package_index_url: url,
        changed: false,
      }),
    );

    await env.PACKAGE_INDEXED_QUEUE.send({
      type: "package.indexed",
      package_name: releaseRecord.package_name,
      package_version: releaseRecord.package_version,
      package_locator: releaseRecord.package_locator,
      artifact_sha256: releaseRecord.artifact_sha256,
      package_index_key: key,
      package_index_url: url,
      latest: document.latest,
      indexed_at: document.updated_at,
    } satisfies PackageIndexedEvent);

    return {
      changed: false,
      latest: document.latest,
      indexedAt: document.updated_at,
      packageIndexKey: key,
      packageIndexUrl: url,
    };
  }

  await writePackageIndexDocument(env.ML_PKGS_CDN, config, document);
  await upsertSearchRow(env.SEARCH_DB, searchRow);
  await writeRegistryEvent(
    env.SEARCH_DB,
    makeIndexEvent("package.searchable", releaseRecord, searchableAt, {
      latest: document.latest,
      package_index_key: key,
      package_index_url: url,
      changed: true,
    }),
  );
  await writeRegistryEvent(
    env.SEARCH_DB,
    makeIndexEvent("package.indexed", releaseRecord, indexedAt, {
      latest: document.latest,
      package_index_key: key,
      package_index_url: url,
      changed: true,
    }),
  );
  await env.PACKAGE_INDEXED_QUEUE.send({
    type: "package.indexed",
    package_name: releaseRecord.package_name,
    package_version: releaseRecord.package_version,
    package_locator: releaseRecord.package_locator,
    artifact_sha256: releaseRecord.artifact_sha256,
    package_index_key: key,
    package_index_url: url,
    latest: document.latest,
    indexed_at: document.updated_at,
  } satisfies PackageIndexedEvent);

  return {
    changed: true,
    latest: document.latest,
    indexedAt: document.updated_at,
    packageIndexKey: key,
    packageIndexUrl: url,
  };
}

export async function reindexPublishedRelease(
  env: Env,
  packageName: string,
  version: string,
  updatedAt: string,
): Promise<{
  changed: boolean;
  latest: string;
  indexedAt: string;
  packageIndexKey: string;
  packageIndexUrl: string;
}> {
  const releaseRecord = await readPublishedRelease(env.SEARCH_DB, packageName, version);
  if (releaseRecord === null) {
    throw new Error(`Published release ${packageName}@${version} no longer exists.`);
  }

  const config = getConfig(env);
  await writeIndexConfig(env.ML_PKGS_CDN, config);
  const currentDocument = await readPackageIndexDocument(env.ML_PKGS_CDN, config, packageName);
  const release = buildIndexedRelease(releaseRecord);
  const { document, changed } = upsertPackageDocument({
    existing: currentDocument,
    packageName,
    release,
    updatedAt,
  });

  const key = packageIndexKey(config, packageName);
  const url = packageIndexUrl(config, packageName);
  const claim = await readPackageClaim(env.SEARCH_DB, packageName);
  const searchRow = buildSearchRow(document, {
    ownerGithubLogin: claim?.owner_github_login,
  });
  const searchableAt = addMilliseconds(document.updated_at, 1);
  const indexedAt = addMilliseconds(document.updated_at, 2);

  if (changed) {
    await writePackageIndexDocument(env.ML_PKGS_CDN, config, document);
  }

  await upsertSearchRow(env.SEARCH_DB, searchRow);
  await writeRegistryEvent(
    env.SEARCH_DB,
    makeIndexEvent("package.searchable", releaseRecord, searchableAt, {
      latest: document.latest,
      package_index_key: key,
      package_index_url: url,
      changed,
    }),
  );
  await writeRegistryEvent(
    env.SEARCH_DB,
    makeIndexEvent("package.indexed", releaseRecord, indexedAt, {
      latest: document.latest,
      package_index_key: key,
      package_index_url: url,
      changed,
    }),
  );
  await env.PACKAGE_INDEXED_QUEUE.send({
    type: "package.indexed",
    package_name: releaseRecord.package_name,
    package_version: releaseRecord.package_version,
    package_locator: releaseRecord.package_locator,
    artifact_sha256: releaseRecord.artifact_sha256,
    package_index_key: key,
    package_index_url: url,
    latest: document.latest,
    indexed_at: document.updated_at,
  } satisfies PackageIndexedEvent);

  return {
    changed,
    latest: document.latest,
    indexedAt: document.updated_at,
    packageIndexKey: key,
    packageIndexUrl: url,
  };
}

function makeIndexEvent(
  eventType: "package.indexed" | "package.searchable",
  releaseRecord: PublishedReleaseRecord,
  createdAt: string,
  payload: Record<string, unknown>,
) {
  return {
    event_id: uuidv7({
      msecs: Date.parse(createdAt),
    }),
    event_type: eventType,
    package_name: releaseRecord.package_name,
    package_version: releaseRecord.package_version,
    package_locator: releaseRecord.package_locator,
    payload: {
      artifact_sha256: releaseRecord.artifact_sha256,
      ...payload,
    },
    created_at: createdAt,
  } as const;
}

function addMilliseconds(timestamp: string, milliseconds: number): string {
  return new Date(Date.parse(timestamp) + milliseconds).toISOString();
}
