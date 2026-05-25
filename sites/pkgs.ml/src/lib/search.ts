import { getConfig } from "./config.ts";
import type { SearchResponse, SearchResult } from "./types.ts";

export async function fetchSearchResults(query: string): Promise<SearchResponse> {
  const trimmed = query.trim();
  const response = await fetch(
    `${getConfig().searchApiBaseUrl}?q=${encodeURIComponent(trimmed)}`,
    {
      headers: {
        accept: "application/json",
      },
    },
  );

  if (!response.ok) {
    throw new Error(`Search request failed: ${response.status}`);
  }

  return (await response.json()) as SearchResponse;
}

export async function fetchPackagesByOwner(owner: string): Promise<SearchResult[]> {
  const trimmed = owner.trim();
  const response = await fetch(
    `${getConfig().searchApiBaseUrl}?q=${encodeURIComponent(trimmed)}&limit=100`,
    {
      headers: {
        accept: "application/json",
      },
    },
  );

  if (!response.ok) {
    throw new Error(`Owner search request failed: ${response.status}`);
  }

  const payload = (await response.json()) as SearchResponse;
  return payload.results
    .filter((result) => result.repo_owner.toLowerCase() === trimmed.toLowerCase())
    .sort((left, right) => {
      const rightUpdatedAt = Date.parse(right.updated_at);
      const leftUpdatedAt = Date.parse(left.updated_at);

      if (!Number.isNaN(rightUpdatedAt) && !Number.isNaN(leftUpdatedAt) && rightUpdatedAt !== leftUpdatedAt) {
        return rightUpdatedAt - leftUpdatedAt;
      }

      return left.package_name.localeCompare(right.package_name);
    });
}
