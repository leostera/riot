import { getConfig } from "./config.ts";
import type {
  CategoriesIndexDocument,
  OwnerPackagesDocument,
  PackageDownloadsDocument,
  PackageExamplesDocument,
  PackageOverviewDocument,
  PackageReadmeDocument,
  PackageRelationsDocument,
  PopularPackagesDocument,
  RegistryServiceRootDocument,
  RegistryStatsDashboardDocument,
  RegistryStatsWindowKey,
  RecentPackagesDocument,
  RegistryStatsSummaryDocument,
  RegistryEventsDocument,
} from "./types.ts";

function viewUrl(path: string): string {
  const config = getConfig();
  return `${config.registryBaseUrl}/v1/views/${path}`;
}

async function fetchJsonOrNull<T>(url: string): Promise<T | null> {
  const response = await fetch(url, {
    headers: {
      accept: "application/json",
    },
  });

  if (response.status === 404) {
    return null;
  }

  if (!response.ok) {
    throw new Error(`View request failed for ${url}: ${response.status}`);
  }

  return (await response.json()) as T;
}

export async function fetchRegistryServiceDocument(): Promise<RegistryServiceRootDocument> {
  const config = getConfig();
  const response = await fetch(`${config.registryBaseUrl}/`, {
    headers: {
      accept: "application/json",
    },
  });

  if (!response.ok) {
    throw new Error(`Registry service request failed: ${response.status}`);
  }

  return (await response.json()) as RegistryServiceRootDocument;
}

export async function fetchPackageOverview(packageName: string): Promise<PackageOverviewDocument | null> {
  return await fetchJsonOrNull<PackageOverviewDocument>(
    viewUrl(`packages/${encodeURIComponent(packageName)}/overview`),
  );
}

export async function fetchPackageReadme(
  packageName: string,
  options?: { version?: string },
): Promise<PackageReadmeDocument | null> {
  const params = new URLSearchParams();
  if (options?.version !== undefined) {
    params.set("version", options.version);
  }

  const path = `packages/${encodeURIComponent(packageName)}/readme${
    params.size > 0 ? `?${params.toString()}` : ""
  }`;

  return await fetchJsonOrNull<PackageReadmeDocument>(viewUrl(path));
}

export async function fetchPackageExamples(
  packageName: string,
  options?: { version?: string },
): Promise<PackageExamplesDocument | null> {
  const params = new URLSearchParams();
  if (options?.version !== undefined) {
    params.set("version", options.version);
  }

  const path = `packages/${encodeURIComponent(packageName)}/examples${
    params.size > 0 ? `?${params.toString()}` : ""
  }`;

  return await fetchJsonOrNull<PackageExamplesDocument>(viewUrl(path));
}

export async function fetchPackageDownloads(packageName: string): Promise<PackageDownloadsDocument | null> {
  return await fetchJsonOrNull<PackageDownloadsDocument>(
    viewUrl(`packages/${encodeURIComponent(packageName)}/downloads`),
  );
}

export async function fetchPackageRelations(packageName: string): Promise<PackageRelationsDocument | null> {
  return await fetchJsonOrNull<PackageRelationsDocument>(
    viewUrl(`packages/${encodeURIComponent(packageName)}/relations`),
  );
}

export async function fetchRecentPackages(): Promise<RecentPackagesDocument | null> {
  return await fetchJsonOrNull<RecentPackagesDocument>(viewUrl("recent/packages"));
}

export async function fetchPopularPackages(): Promise<PopularPackagesDocument | null> {
  return await fetchJsonOrNull<PopularPackagesDocument>(viewUrl("popular/packages"));
}

export async function fetchCategoriesIndex(): Promise<CategoriesIndexDocument | null> {
  return await fetchJsonOrNull<CategoriesIndexDocument>(viewUrl("categories"));
}

export async function fetchRegistryStatsSummary(): Promise<RegistryStatsSummaryDocument> {
  const config = getConfig();
  const response = await fetch(`${config.registryBaseUrl}/v1/views/stats/summary`, {
    headers: {
      accept: "application/json",
    },
  });

  if (!response.ok) {
    throw new Error(`Stats summary request failed: ${response.status}`);
  }

  return (await response.json()) as RegistryStatsSummaryDocument;
}

export async function fetchRegistryStatsDashboard(
  window: RegistryStatsWindowKey = "30d",
): Promise<RegistryStatsDashboardDocument> {
  const config = getConfig();
  const response = await fetch(`${config.registryBaseUrl}/v1/views/stats/dashboard?window=${encodeURIComponent(window)}`, {
    headers: {
      accept: "application/json",
    },
  });

  if (!response.ok) {
    throw new Error(`Stats dashboard request failed: ${response.status}`);
  }

  return (await response.json()) as RegistryStatsDashboardDocument;
}

export async function fetchOwnerPackages(ownerGithubLogin: string): Promise<OwnerPackagesDocument | null> {
  return await fetchJsonOrNull<OwnerPackagesDocument>(
    viewUrl(`owners/${encodeURIComponent(ownerGithubLogin.toLowerCase())}/packages`),
  );
}

export async function fetchRegistryEvents(limit = 100, after?: string): Promise<RegistryEventsDocument> {
  const config = getConfig();
  const searchParams = new URLSearchParams({
    limit: String(limit),
  });
  if (after !== undefined) {
    searchParams.set("after", String(after));
  }

  const response = await fetch(`${config.registryBaseUrl}/v1/events?${searchParams.toString()}`, {
    headers: {
      accept: "application/json",
    },
  });

  if (!response.ok) {
    throw new Error(`Events request failed: ${response.status}`);
  }

  return (await response.json()) as RegistryEventsDocument;
}

export async function fetchPackageEvents(
  packageName: string,
  options?: { version?: string; limit?: number },
): Promise<RegistryEventsDocument> {
  const config = getConfig();
  const searchParams = new URLSearchParams({
    limit: String(options?.limit ?? 25),
  });

  if (options?.version !== undefined) {
    searchParams.set("version", options.version);
  }

  const response = await fetch(
    `${config.registryBaseUrl}/v1/packages/${encodeURIComponent(packageName)}/events?${searchParams.toString()}`,
    {
      headers: {
        accept: "application/json",
      },
    },
  );

  if (!response.ok) {
    throw new Error(`Package events request failed: ${response.status}`);
  }

  return (await response.json()) as RegistryEventsDocument;
}
