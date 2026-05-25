export interface WebConfig {
  searchApiBaseUrl: string;
  indexBaseUrl: string;
  indexBasePath: string;
  registryBaseUrl: string;
}

export function getConfig(): WebConfig {
  const registryBaseUrl =
    import.meta.env.PUBLIC_REGISTRY_BASE_URL?.trim() || "https://api.pkgs.ml";

  return {
    searchApiBaseUrl:
      import.meta.env.PUBLIC_SEARCH_API_BASE_URL?.trim() || `${registryBaseUrl}/v1/search`,
    indexBaseUrl: import.meta.env.PUBLIC_INDEX_BASE_URL?.trim() || "https://cdn.pkgs.ml",
    indexBasePath: import.meta.env.PUBLIC_INDEX_BASE_PATH?.trim() || "index/v1",
    registryBaseUrl,
  };
}
