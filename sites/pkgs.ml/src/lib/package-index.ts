import { getConfig } from "./config.ts";
import type { IndexedPackageRelease, PackageFact, PackageIndexDocument } from "./types.ts";

export function packageIndexPath(packageName: string, indexBasePath = getConfig().indexBasePath): string {
  const normalized = packageName.trim().toLowerCase();

  if (normalized.length === 1) {
    return `${indexBasePath}/1/${normalized}.json`;
  }

  if (normalized.length === 2) {
    return `${indexBasePath}/2/${normalized}.json`;
  }

  if (normalized.length === 3) {
    return `${indexBasePath}/3/${normalized[0]}/${normalized}.json`;
  }

  return `${indexBasePath}/${normalized.slice(0, 2)}/${normalized.slice(2, 4)}/${normalized}.json`;
}

export function packageIndexUrl(packageName: string): string {
  const config = getConfig();
  return `${config.indexBaseUrl}/${packageIndexPath(packageName, config.indexBasePath)}`;
}

export async function fetchPackageDocument(packageName: string): Promise<PackageIndexDocument | null> {
  const response = await fetch(packageIndexUrl(packageName), {
    headers: {
      accept: "application/json",
    },
  });

  if (response.status === 404) {
    return null;
  }

  if (!response.ok) {
    throw new Error(`Package index request failed for ${packageName}: ${response.status}`);
  }

  return (await response.json()) as PackageIndexDocument;
}

export async function fetchPackageRelease(
  packageName: string,
  version?: string,
): Promise<{ document: PackageIndexDocument; release: IndexedPackageRelease } | null> {
  const document = await fetchPackageDocument(packageName);
  if (document === null) {
    return null;
  }

  const resolvedVersion = version ?? document.latest;
  const release = document.releases.find((candidate) => candidate.version === resolvedVersion);
  if (release === undefined) {
    return null;
  }

  return { document, release };
}

export function buildPackageFacts(
  document: PackageIndexDocument,
  release: IndexedPackageRelease,
): PackageFact[] {
  return [
    {
      label: "Install",
      value: `riot add ${document.name}`,
      code: true,
    },
    {
      label: "riot.toml",
      value: `${document.name} = "${release.version}"`,
      code: true,
    },
  ];
}

export function formatDate(value: string): string {
  const date = new Date(value);
  if (Number.isNaN(date.getTime())) {
    return "Unknown";
  }

  return new Intl.DateTimeFormat("en", {
    year: "numeric",
    month: "short",
    day: "numeric",
  }).format(date);
}
