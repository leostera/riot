import semver from "semver";

import type {
  IndexedPackageRelease,
  PackageIndexDocument,
  PublishedReleaseRecord,
} from "./types.ts";

export function buildIndexedRelease(release: PublishedReleaseRecord): IndexedPackageRelease {
  return {
    version: release.package_version,
    published_at: release.published_at,
    canonical_locator: release.package_locator,
    repo_url: release.source_url,
    subdir: release.package_subdir,
    artifact_sha256: release.artifact_sha256,
    description: release.package_description,
    license: release.package_license,
    homepage: release.package_homepage,
    repository: release.package_repository,
    root_module: release.package_root_module,
    categories: release.package_categories,
    keywords: release.package_keywords,
    manifest_key: release.manifest_key,
    source_key: release.source_archive_key,
    dependencies: release.dependencies,
    ...(release.yanked_at !== undefined
      ? {
          yanked: true,
          yanked_at: release.yanked_at,
          yanked_by_github_login: release.yanked_by_github_login,
        }
      : {}),
  };
}

export function upsertPackageDocument(args: {
  existing: PackageIndexDocument | null;
  packageName: string;
  release: IndexedPackageRelease;
  updatedAt: string;
}): {
  document: PackageIndexDocument;
  changed: boolean;
} {
  const existing = args.existing;
  if (existing === null) {
    return {
      changed: true,
        document: {
          schema_version: 1,
          name: args.packageName,
          latest: args.release.version,
          updated_at: args.updatedAt,
          releases: [args.release],
        },
    };
  }

  if (existing.name !== args.packageName) {
    throw new Error(
      `Package document ${existing.name} does not match published package ${args.packageName}.`,
    );
  }

  const releases = [...existing.releases];
  const index = releases.findIndex((candidate) => candidate.version === args.release.version);
  let changed = false;

  if (index === -1) {
    releases.push(args.release);
    changed = true;
  } else {
    const existingRelease = releases[index];
    if (existingRelease === undefined) {
      throw new Error(
        `Package document ${args.packageName} lost release ${args.release.version} during upsert.`,
      );
    }

    if (!isSameIndexedRelease(existingRelease, args.release)) {
      releases[index] = args.release;
      changed = true;
    }
  }

  releases.sort((left, right) => semver.rcompare(left.version, right.version));
  const latest = releases.find((release) => release.yanked !== true)?.version ?? releases[0]?.version;
  if (latest === undefined) {
    throw new Error(`Package document ${args.packageName} has no releases after upsert.`);
  }

  if (!changed && existing.latest === latest) {
    return {
      changed: false,
      document: existing,
    };
  }

  return {
    changed: true,
    document: {
      schema_version: 1,
      name: args.packageName,
      latest,
      updated_at: args.updatedAt,
      releases,
    },
  };
}

function isSameIndexedRelease(left: IndexedPackageRelease, right: IndexedPackageRelease): boolean {
  return (
    left.version === right.version &&
    left.published_at === right.published_at &&
    left.canonical_locator === right.canonical_locator &&
    left.repo_url === right.repo_url &&
    left.subdir === right.subdir &&
    left.artifact_sha256 === right.artifact_sha256 &&
    left.description === right.description &&
    left.license === right.license &&
    left.homepage === right.homepage &&
    left.repository === right.repository &&
    left.root_module === right.root_module &&
    JSON.stringify(left.categories ?? []) === JSON.stringify(right.categories ?? []) &&
    JSON.stringify(left.keywords ?? []) === JSON.stringify(right.keywords ?? []) &&
    left.manifest_key === right.manifest_key &&
    left.source_key === right.source_key &&
    JSON.stringify(left.dependencies) === JSON.stringify(right.dependencies) &&
    left.yanked === right.yanked &&
    left.yanked_at === right.yanked_at &&
    left.yanked_by_github_login === right.yanked_by_github_login
  );
}
