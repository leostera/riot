import { parse as parseToml } from "smol-toml";

import { readArchiveFileFromTarGz } from "./archive.ts";
import { HttpError } from "./errors.ts";
import {
  artifactManifestKey,
  artifactSourceArchiveKey,
} from "./storage.ts";
import type { PackagePublicationManifest } from "./types.ts";

export async function buildPublicationManifestFromArtifact(args: {
  archiveBytes: Uint8Array<ArrayBuffer>;
  artifactSha256: string;
  materializedAt: string;
  packageLocator?: string;
  sourceUrl?: string;
  packageSubdir?: string;
}): Promise<PackagePublicationManifest> {
  const riotToml = await readArchiveFileFromTarGz(args.archiveBytes, "riot.toml");

  if (riotToml === null) {
    throw new HttpError(
      422,
      "invalid_package_archive",
      "Published package artifact must contain riot.toml at archive root.",
    );
  }

  const parsed = parseRiotToml(riotToml, args.packageLocator ?? "artifact");
  const packageSection = asRecord(parsed.package, "package", args.packageLocator ?? "artifact");
  const packageName = expectString(packageSection.name, "package.name", args.packageLocator ?? "artifact");
  const packageVersion = expectString(
    packageSection.version,
    "package.version",
    args.packageLocator ?? packageName,
  );
  const packagePublic = readBoolean(
    packageSection.public,
    "package.public",
    args.packageLocator ?? packageName,
  );
  const packageDescription = readOptionalString(
    packageSection.description,
    "package.description",
    args.packageLocator ?? packageName,
  );
  const packageLicense = readOptionalString(
    packageSection.license,
    "package.license",
    args.packageLocator ?? packageName,
  );
  const packageHomepage = readOptionalString(
    packageSection.homepage,
    "package.homepage",
    args.packageLocator ?? packageName,
  );
  const packageRepository = readOptionalString(
    packageSection.repository,
    "package.repository",
    args.packageLocator ?? packageName,
  );
  const packageRootModule = readOptionalString(
    packageSection.root_module,
    "package.root_module",
    args.packageLocator ?? packageName,
  );
  const packageCategories = readOptionalStringArray(
    packageSection.categories,
    "package.categories",
    args.packageLocator ?? packageName,
  );
  const packageKeywords = readOptionalStringArray(
    packageSection.keywords,
    "package.keywords",
    args.packageLocator ?? packageName,
  );

  return {
    package_locator: args.packageLocator ?? "",
    source_url: args.sourceUrl ?? "",
    package_subdir: args.packageSubdir ?? ".",
    artifact_sha256: args.artifactSha256,
    package_name: packageName,
    package_version: packageVersion,
    package_public: packagePublic,
    package_description: packageDescription,
    package_license: packageLicense,
    package_homepage: packageHomepage,
    package_repository: packageRepository,
    package_root_module: packageRootModule,
    package_categories: packageCategories,
    package_keywords: packageKeywords,
    dependencies: extractDependencies(parsed.dependencies),
    source_archive_key: artifactSourceArchiveKey(packageName, packageVersion, args.artifactSha256),
    manifest_key: artifactManifestKey(packageName, packageVersion, args.artifactSha256),
    materialized_at: args.materializedAt,
  };
}

function parseRiotToml(source: string, locator: string): Record<string, unknown> {
  const parsed = parseToml(source);
  return asRecord(parsed, "document", locator);
}

function extractDependencies(section: unknown): Array<Record<string, unknown>> {
  if (section === undefined) {
    return [];
  }

  const dependencies = asRecord(section, "dependencies", "dependencies");
  return Object.entries(dependencies).map(([name, specification]) => {
    if (typeof specification === "string") {
      return {
        name,
        raw: specification,
      };
    }

    if (specification !== null && typeof specification === "object" && !Array.isArray(specification)) {
      return {
        name,
        ...specification,
      };
    }

    return {
      name,
      raw: specification,
    };
  });
}

function asRecord(value: unknown, field: string, locator: string): Record<string, unknown> {
  if (value !== null && typeof value === "object" && !Array.isArray(value)) {
    return value as Record<string, unknown>;
  }

  throw new HttpError(
    422,
    "invalid_package_manifest",
    `Field ${field} in ${locator} must be a TOML table.`,
  );
}

function expectString(value: unknown, field: string, locator: string): string {
  if (typeof value === "string" && value.length > 0) {
    return value;
  }

  throw new HttpError(
    422,
    "invalid_package_manifest",
    `Field ${field} in ${locator} must be a non-empty string.`,
  );
}

function readBoolean(value: unknown, field: string, locator: string): boolean {
  if (value === undefined) {
    return false;
  }

  if (typeof value === "boolean") {
    return value;
  }

  throw new HttpError(
    422,
    "invalid_package_manifest",
    `Field ${field} in ${locator} must be a boolean when present.`,
  );
}

function readOptionalString(value: unknown, field: string, locator: string): string | undefined {
  if (value === undefined) {
    return undefined;
  }

  if (typeof value === "string" && value.length > 0) {
    return value;
  }

  throw new HttpError(
    422,
    "invalid_package_manifest",
    `Field ${field} in ${locator} must be a non-empty string when present.`,
  );
}

function readOptionalStringArray(
  value: unknown,
  field: string,
  locator: string,
): string[] | undefined {
  if (value === undefined) {
    return undefined;
  }

  if (!Array.isArray(value)) {
    throw new HttpError(
      422,
      "invalid_package_manifest",
      `Field ${field} in ${locator} must be an array of non-empty strings when present.`,
    );
  }

  const items = value.map((item) => {
    if (typeof item === "string" && item.length > 0) {
      return item;
    }

    throw new HttpError(
      422,
      "invalid_package_manifest",
      `Field ${field} in ${locator} must be an array of non-empty strings when present.`,
    );
  });

  return items.length === 0 ? undefined : items;
}
