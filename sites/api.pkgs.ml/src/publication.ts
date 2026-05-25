import gitUrlParse from "git-url-parse";
import semver from "semver";
import spdxExpressionValidate from "spdx-expression-validate";
import { v7 as uuidv7 } from "uuid";

import { HttpError } from "./errors.ts";
import { indexPublishedRelease } from "./indexing.ts";
import { buildPublicationManifestFromArtifact } from "./manifest.ts";
import {
  hasPublishedRelease,
  readPackageClaim,
  readPublishedRelease,
  writePackageClaim,
  writePublishedRelease,
  writeRegistryEvent,
} from "./metadata-db.ts";
import { packageClaimKey, publishedReleaseKey } from "./storage.ts";
import type {
  AuthenticatedActor,
  Env,
  PackageClaimRecord,
  PackagePublicationManifest,
  PackagePublishedEvent,
  PublishedPackageRelease,
  PublishedReleaseRecord,
} from "./types.ts";

const OCAML_BUILTIN_DEPENDENCIES = new Set([
  "stdlib",
  "unix",
  "dynlink",
]);

interface StoredArtifact {
  artifactSha256: string;
  sourceKey: string;
  manifestKey: string;
  sourceCreated: boolean;
  manifestCreated: boolean;
}

export async function publishPackageArtifact(
  env: Env,
  archiveBytes: Uint8Array<ArrayBuffer>,
  actor: AuthenticatedActor,
): Promise<PublishedPackageRelease> {
  const materializedAt = new Date().toISOString();
  const artifactSha256 = await sha256Hex(archiveBytes);
  const manifest = await buildPublicationManifestFromArtifact({
    archiveBytes,
    artifactSha256,
    materializedAt,
  });

  const existingSource = await env.ML_PKGS_CDN.head(manifest.source_archive_key);
  const existingManifest = await env.ML_PKGS_CDN.head(manifest.manifest_key);
  let sourceCreated = false;
  let manifestCreated = false;

  if (existingSource === null) {
    await env.ML_PKGS_CDN.put(manifest.source_archive_key, archiveBytes, {
      httpMetadata: {
        contentType: "application/gzip",
      },
    });
    sourceCreated = true;
  }

  if (existingManifest === null) {
    await env.ML_PKGS_CDN.put(manifest.manifest_key, JSON.stringify(manifest, null, 2), {
      httpMetadata: {
        contentType: "application/json; charset=utf-8",
      },
    });
    manifestCreated = true;
  }

  return await publishDerivedManifest(env, manifest, actor, {
    artifactSha256: manifest.artifact_sha256,
    sourceKey: manifest.source_archive_key,
    manifestKey: manifest.manifest_key,
    sourceCreated,
    manifestCreated,
  });
}

function assertPublishableManifest(manifest: PackagePublicationManifest): void {
  const manifestLabel = publicationSubject(manifest);
  if (!manifest.package_public) {
    throw new HttpError(
      422,
      "package_not_public",
      `Package ${manifestLabel} is not publishable because package.public is false.`,
    );
  }

  if (!isValidSemver(manifest.package_version)) {
    throw new HttpError(
      422,
      "invalid_package_version",
      `Package ${manifestLabel} has non-semver version ${manifest.package_version}.`,
    );
  }

  if (manifest.package_description === undefined) {
    throw new HttpError(
      422,
      "missing_package_description",
      `Package ${manifestLabel} must declare package.description to be publishable.`,
    );
  }

  if (manifest.package_license === undefined) {
    throw new HttpError(
      422,
      "missing_package_license",
      `Package ${manifestLabel} must declare package.license to be publishable.`,
    );
  }

  if (!spdxExpressionValidate(manifest.package_license)) {
    throw new HttpError(
      422,
      "invalid_package_license",
      `Package ${manifestLabel} must declare an SPDX-compatible package.license value.`,
    );
  }
}

async function assertPublishableDependencies(
  db: D1Database,
  manifest: PackagePublicationManifest,
): Promise<void> {
  for (const dependency of manifest.dependencies) {
    const normalized = normalizeDependencyRecord(dependency);
    if (normalized === null) {
      throw new HttpError(
        422,
        "invalid_dependency_reference",
        `Invalid dependency entry in ${manifest.package_name}: dependency declarations must include a package name.`,
      );
    }

    if (normalized.kind === "path_only") {
      throw new HttpError(
        422,
        "invalid_dependency_reference",
        `Dependency ${normalized.name} in ${manifest.package_name} uses a path-only reference and is not publishable.`,
      );
    }

    if (normalized.kind === "git") {
      continue;
    }

    if (isBuiltinOcamlDependency(normalized.name)) {
      continue;
    }

    if (!(await hasPublishedRelease(db, normalized.name))) {
      throw new HttpError(
        422,
        "missing_dependency",
        `Dependency ${normalized.name}@${normalized.requirement} is missing from the registry.`,
      );
    }
  }
}

function normalizeDependencyRecord(dependency: Record<string, unknown>): DependencyReference | null {
  const packageName = readDependencyPackageName(dependency);
  if (packageName === null) {
    return null;
  }

  const pathReference = readStringField(dependency, "path");
  const gitReference =
    readStringField(dependency, "git") ??
    readStringField(dependency, "url") ??
    readStringField(dependency, "source") ??
    readStringField(dependency, "github");
  if (gitReference !== null) {
    if (!isValidGitReference(gitReference) && !isValidSourceReference(gitReference)) {
      throw new HttpError(
        422,
        "invalid_dependency_reference",
        `Dependency ${packageName} has invalid git reference ${gitReference}.`,
      );
    }

    return {
      name: packageName,
      kind: "git",
      requirement: gitReference,
    };
  }

  const requirement = readStringField(dependency, "requirement") ??
    readStringField(dependency, "version") ??
    readStringField(dependency, "raw");

  if (requirement === null) {
    if (pathReference !== null) {
      return {
        name: packageName,
        kind: "path_only",
      };
    }

    throw new HttpError(
      422,
      "invalid_dependency_reference",
      `Dependency ${packageName} must declare a requirement, path, or git reference.`,
    );
  }

  if (!isValidSemverRange(requirement)) {
    throw new HttpError(
      422,
      "invalid_dependency_reference",
      `Dependency ${packageName} has non-semver requirement ${requirement}.`,
    );
  }

  return {
    name: packageName,
    requirement,
    requirementKind: "semver",
    kind: "registry",
  };
}

function readDependencyPackageName(dependency: Record<string, unknown>): string | null {
  if (typeof dependency.package === "string") {
    return dependency.package.trim().length > 0 ? dependency.package.trim() : null;
  }

  if (typeof dependency.name === "string") {
    return dependency.name.trim().length > 0 ? dependency.name.trim() : null;
  }

  return null;
}

function readStringField(dependency: Record<string, unknown>, key: string): string | null {
  const value = dependency[key];
  return typeof value === "string" && value.trim().length > 0 ? value.trim() : null;
}

function isValidSemverRange(value: string): boolean {
  const trimmed = value.trim();
  if (trimmed.toLowerCase() === "latest") {
    return true;
  }

  return semver.validRange(trimmed) !== null;
}

function isValidGitReference(value: string): boolean {
  const trimmed = value.trim();
  if (trimmed.length === 0) {
    return false;
  }

  try {
    const parsed = gitUrlParse(trimmed);
    return parsed.owner.length > 0 && parsed.name.length > 0;
  } catch {
    return false;
  }
}

function isValidSourceReference(value: string): boolean {
  const trimmed = value.trim();
  if (trimmed.length === 0) {
    return false;
  }

  if (trimmed.startsWith("http://") || trimmed.startsWith("https://")) {
    try {
      const parsed = new URL(trimmed);
      return parsed.hostname.length > 0;
    } catch {
      return false;
    }
  }

  const segments = trimmed.split("/").filter((segment) => segment.length > 0);
  return segments.length >= 2;
}

function isBuiltinOcamlDependency(value: string): boolean {
  return OCAML_BUILTIN_DEPENDENCIES.has(value.trim().toLowerCase());
}

type DependencyReference =
  | { name: string; kind: "path_only" }
  | { name: string; kind: "git"; requirement: string }
  | { name: string; kind: "registry"; requirement: string; requirementKind: "semver" };

function buildClaimRecord(
  manifest: PackagePublicationManifest,
  existingClaim: PackageClaimRecord | null,
  actor: AuthenticatedActor,
  timestamp: string,
): PackageClaimRecord {
  if (existingClaim !== null) {
    if (actor.kind === "user") {
      const ownerUserIdMatches =
        existingClaim.owner_user_id !== undefined && existingClaim.owner_user_id === actor.userId;
      const ownerLoginMatches =
        existingClaim.owner_github_login !== undefined &&
        existingClaim.owner_github_login.toLowerCase() === actor.githubLogin.toLowerCase();
      const claimUnowned =
        existingClaim.owner_user_id === undefined && existingClaim.owner_github_login === undefined;

      if (claimUnowned) {
        return {
          ...existingClaim,
          owner_user_id: actor.userId,
          owner_github_login: actor.githubLogin,
          package_locator: pickPublishedMetadata(manifest.package_locator, existingClaim.package_locator),
          source_url: pickPublishedMetadata(manifest.source_url, existingClaim.source_url),
          package_subdir: pickPublishedMetadata(manifest.package_subdir, existingClaim.package_subdir),
          updated_at: timestamp,
        };
      }

      if (!ownerUserIdMatches && !ownerLoginMatches) {
        throw new HttpError(
          403,
          "package_claim_forbidden",
          `Package ${manifest.package_name} is owned by another publisher.`,
        );
      }
    }

    return {
      ...existingClaim,
      package_locator: pickPublishedMetadata(manifest.package_locator, existingClaim.package_locator),
      source_url: pickPublishedMetadata(manifest.source_url, existingClaim.source_url),
      package_subdir: pickPublishedMetadata(manifest.package_subdir, existingClaim.package_subdir),
      updated_at: timestamp,
    };
  }

  return {
    package_name: manifest.package_name,
    package_locator: manifest.package_locator,
    source_url: manifest.source_url,
    package_subdir: manifest.package_subdir,
    owner_user_id: actor.kind === "user" ? actor.userId : undefined,
    owner_github_login: actor.kind === "user" ? actor.githubLogin : undefined,
    claimed_at: timestamp,
    updated_at: timestamp,
  };
}

function buildPublishedReleaseRecord(
  manifest: PackagePublicationManifest,
  publishedAt: string,
): PublishedReleaseRecord {
  return {
    package_name: manifest.package_name,
    package_version: manifest.package_version,
    package_locator: manifest.package_locator,
    source_url: manifest.source_url,
    package_subdir: manifest.package_subdir,
    artifact_sha256: manifest.artifact_sha256,
    package_description: manifest.package_description,
    package_license: manifest.package_license,
    package_homepage: manifest.package_homepage,
    package_repository: manifest.package_repository,
    package_root_module: manifest.package_root_module,
    package_categories: manifest.package_categories,
    package_keywords: manifest.package_keywords,
    dependencies: manifest.dependencies,
    source_archive_key: manifest.source_archive_key,
    manifest_key: manifest.manifest_key,
    published_at: publishedAt,
  };
}

function isValidSemver(value: string): boolean {
  return semver.valid(value.trim()) !== null;
}

function claimMatches(left: PackageClaimRecord, right: PackageClaimRecord): boolean {
  return (
    left.package_name === right.package_name &&
    left.package_locator === right.package_locator &&
    left.source_url === right.source_url &&
    left.package_subdir === right.package_subdir &&
    left.owner_user_id === right.owner_user_id &&
    left.owner_github_login === right.owner_github_login
  );
}

function makePackageEvent(
  eventType: "package.submitted" | "package.verified" | "package.published",
  createdAt: string,
  manifest: PackagePublicationManifest,
  payload: Record<string, unknown>,
) {
  return {
    event_id: uuidv7({
      msecs: Date.parse(createdAt),
    }),
    event_type: eventType,
    package_name: manifest.package_name,
    package_version: manifest.package_version,
    package_locator: manifest.package_locator.length === 0 ? undefined : manifest.package_locator,
    payload,
    created_at: createdAt,
  } as const;
}

async function publishDerivedManifest(
  env: Env,
  manifest: PackagePublicationManifest,
  actor: AuthenticatedActor,
  storedArtifact: StoredArtifact,
): Promise<PublishedPackageRelease> {
  const existingRelease = await readPublishedRelease(
    env.SEARCH_DB,
    manifest.package_name,
    manifest.package_version,
  );

  if (existingRelease !== null) {
    if (existingRelease.artifact_sha256 !== manifest.artifact_sha256) {
      throw new HttpError(
        409,
        "package_version_already_published",
        `Package ${manifest.package_name}@${manifest.package_version} is already published.`,
      );
    }

    return {
      ...storedArtifact,
      packageName: manifest.package_name,
      packageVersion: manifest.package_version,
      claimKey: packageClaimKey(manifest.package_name),
      releaseKey: publishedReleaseKey(manifest.package_name, manifest.package_version),
      claimCreated: false,
      releaseCreated: false,
      indexChanged: false,
    };
  }

  assertPublishableManifest(manifest);
  await assertPublishableDependencies(env.SEARCH_DB, manifest);
  const submittedAt = new Date().toISOString();
  const verifiedAt = addMilliseconds(submittedAt, 1);
  const publishedEventAt = addMilliseconds(submittedAt, 2);
  await writeRegistryEvent(
    env.SEARCH_DB,
    makePackageEvent("package.submitted", submittedAt, manifest, {
      artifact_sha256: manifest.artifact_sha256,
      actor_kind: actor.kind,
    }),
  );

  const now = publishedEventAt;
  await writeRegistryEvent(
    env.SEARCH_DB,
    makePackageEvent("package.verified", verifiedAt, manifest, {
      artifact_sha256: manifest.artifact_sha256,
      dependency_count: manifest.dependencies.length,
    }),
  );
  const existingClaim = await readPackageClaim(env.SEARCH_DB, manifest.package_name);
  const claimRecord = buildClaimRecord(manifest, existingClaim, actor, now);

  if (existingClaim === null || !claimMatches(existingClaim, claimRecord)) {
    await writePackageClaim(env.SEARCH_DB, claimRecord);
  }

  const releaseRecord = buildPublishedReleaseRecord(manifest, now);
  await writePublishedRelease(env.SEARCH_DB, releaseRecord);
  await writeRegistryEvent(
    env.SEARCH_DB,
    makePackageEvent("package.published", publishedEventAt, manifest, {
      artifact_sha256: releaseRecord.artifact_sha256,
      claim_created: existingClaim === null,
      release_created: true,
    }),
  );
  const indexResult = await indexPublishedRelease(env, releaseRecord);
  await env.PACKAGE_PUBLISHED_QUEUE.send({
    type: "package.published",
    ...releaseRecord,
  } satisfies PackagePublishedEvent);

  return {
    ...storedArtifact,
    packageName: manifest.package_name,
    packageVersion: manifest.package_version,
    claimKey: packageClaimKey(manifest.package_name),
    releaseKey: publishedReleaseKey(manifest.package_name, manifest.package_version),
    claimCreated: existingClaim === null,
    releaseCreated: true,
    indexChanged: indexResult.changed,
  };
}

function pickPublishedMetadata(current: string, previous: string): string {
  return current.length > 0 ? current : previous;
}

function publicationSubject(manifest: PackagePublicationManifest): string {
  return manifest.package_locator.length > 0 ? manifest.package_locator : manifest.package_name;
}

async function sha256Hex(bytes: Uint8Array<ArrayBuffer>): Promise<string> {
  const digest = await crypto.subtle.digest("SHA-256", bytes);
  return [...new Uint8Array(digest)]
    .map((value) => value.toString(16).padStart(2, "0"))
    .join("");
}

function addMilliseconds(timestamp: string, milliseconds: number): string {
  return new Date(Date.parse(timestamp) + milliseconds).toISOString();
}
