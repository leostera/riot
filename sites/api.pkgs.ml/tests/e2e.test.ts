import { describe, expect, test } from "bun:test";

import { makeTarGz } from "./helpers.ts";

const baseUrl = trimTrailingSlash(process.env.REGISTRY_E2E_BASE_URL) ?? "https://api.pkgs.ml";
const liveTest = test;
const rootAuthToken = process.env.REGISTRY_E2E_ROOT_AUTH_TOKEN ?? null;
const livePublishTest = rootAuthToken === null ? test.skip : test;
const sessionCookie = process.env.REGISTRY_E2E_SESSION_COOKIE ?? null;
const githubLogin = process.env.REGISTRY_E2E_GITHUB_LOGIN ?? null;
const liveAuthenticatedTest =
  sessionCookie === null || githubLogin === null ? test.skip : test;
const cdnBaseUrl = trimTrailingSlash(process.env.REGISTRY_INDEX_E2E_CDN_BASE_URL) ?? "https://cdn.pkgs.ml";
const indexBaseUrl = trimTrailingSlash(process.env.REGISTRY_INDEX_E2E_BASE_URL) ?? cdnBaseUrl;
const indexBasePath = trimSlashes(process.env.REGISTRY_INDEX_E2E_BASE_PATH) ?? "index/v1";
const artifactBaseUrl =
  trimTrailingSlash(process.env.REGISTRY_ARTIFACT_E2E_BASE_URL) ?? cdnBaseUrl;

describe("riot package registry live e2e", () => {
  liveTest("root route returns service metadata", async () => {
    const response = await fetch(`${baseUrl}/`);
    expect(response.status).toBe(200);

    const payload = (await response.json()) as Record<string, unknown>;
    expect(payload.service).toBe("riot-package-registry");
    expect(payload.routes).toEqual({
      publish_artifact: "/v1/publish",
      views_package_overview: "/v1/views/packages/<package-name>/overview",
      views_package_downloads: "/v1/views/packages/<package-name>/downloads",
      views_package_relations: "/v1/views/packages/<package-name>/relations",
      views_recent_packages: "/v1/views/recent/packages",
      views_popular_packages: "/v1/views/popular/packages",
      views_categories: "/v1/views/categories",
      views_owner_packages: "/v1/views/owners/<github-login>/packages",
      views_stats_summary: "/v1/views/stats/summary",
      views_stats_dashboard: "/v1/views/stats/dashboard",
      auth_github_start: "/v1/auth/github/start?return_to=<url>",
      auth_github_callback: "/v1/auth/github/callback?code=<code>&state=<state>",
      auth_logout: "/v1/auth/logout",
      me: "/v1/me",
      tokens: "/v1/me/tokens",
      search: "/v1/search?q=<query>",
      events: "/v1/events?limit=<count>&after=<event-id>",
      package_events: "/v1/packages/<package-name>/events?version=<version>&limit=<count>",
    });
    expect(payload.cdn_routes).toEqual({
      index_config: "/index/v1/config.json",
      index_package: "/index/v1/<sharded-package-document>.json",
      artifact_download: "/<artifact-key>",
      riot_latest_metadata: "/riot/latest.json",
      riot_release_metadata: "/riot/riot-<version>.json",
    });
    expect(payload.cdn_base_url).toBe(cdnBaseUrl);
    expect(payload.index_base_url).toBe(`${indexBaseUrl}/${indexBasePath}`);
  });

  liveTest("github auth start redirects to GitHub authorize", async () => {
    const response = await fetch(
      `${baseUrl}/v1/auth/github/start?return_to=${encodeURIComponent("https://pkgs.ml/login")}`,
      {
        redirect: "manual",
      },
    );

    expect(response.status).toBe(302);
    const location = response.headers.get("location");
    expect(location).not.toBeNull();

    const redirectUrl = new URL(location ?? "");
    expect(redirectUrl.origin).toBe("https://github.com");
    expect(redirectUrl.pathname).toBe("/login/oauth/authorize");
    expect(redirectUrl.searchParams.get("client_id")).not.toBeNull();
    expect(redirectUrl.searchParams.get("redirect_uri")).toBe(
      `${baseUrl}/v1/auth/github/callback`,
    );
    expect(redirectUrl.searchParams.get("state")).not.toBeNull();
  });

  liveTest("anonymous session and token routes behave as expected", async () => {
    const meResponse = await fetch(`${baseUrl}/v1/me`, {
      headers: {
        accept: "application/json",
      },
    });

    expect(meResponse.status).toBe(200);
    const mePayload = (await meResponse.json()) as { authenticated: boolean };
    expect(mePayload).toEqual({
      authenticated: false,
    });

    const tokenListResponse = await fetch(`${baseUrl}/v1/me/tokens`, {
      headers: {
        accept: "application/json",
      },
    });

    expect(tokenListResponse.status).toBe(401);
    expect(await tokenListResponse.json()).toMatchObject({
      error: "unauthorized",
    });
  });

  liveTest("search miss returns an empty result set", async () => {
    const query = `definitely-not-a-package-${Date.now()}`;
    const response = await fetch(`${baseUrl}/v1/search?q=${encodeURIComponent(query)}`, {
      headers: {
        accept: "application/json",
      },
    });

    expect(response.status).toBe(200);
    const payload = (await response.json()) as SearchResponsePayload;
    expect(payload).toEqual({
      query,
      count: 0,
      results: [],
    });
  });

  liveAuthenticatedTest("authenticated session can list and manage publish tokens", async () => {
    const tokenName = `e2e-${Date.now()}`;

    const meResponse = await fetch(`${baseUrl}/v1/me`, {
      headers: authenticatedHeaders(),
    });
    expect(meResponse.status).toBe(200);
    expect(await meResponse.json()).toMatchObject({
      authenticated: true,
      user: {
        github_login: githubLogin,
      },
    });

    const createResponse = await fetch(`${baseUrl}/v1/me/tokens`, {
      method: "POST",
      headers: {
        ...authenticatedHeaders(),
        "content-type": "application/json",
      },
      body: JSON.stringify({ name: tokenName }),
    });
    expect(createResponse.status).toBe(201);
    const created = (await createResponse.json()) as {
      plaintext_token: string;
      token: { token_id: string; name: string };
    };
    expect(created.plaintext_token.startsWith("sk-")).toBe(true);
    expect(created.token.name).toBe(tokenName);

    const listResponse = await fetch(`${baseUrl}/v1/me/tokens`, {
      headers: authenticatedHeaders(),
    });
    expect(listResponse.status).toBe(200);
    const listed = (await listResponse.json()) as {
      user: { github_login: string };
      tokens: Array<{ token_id: string; name: string }>;
    };
    expect(listed.user.github_login).toBe(githubLogin ?? "");
    expect(listed.tokens.some((token) => token.token_id === created.token.token_id)).toBe(true);

    const revokeResponse = await fetch(
      `${baseUrl}/v1/me/tokens/${encodeURIComponent(created.token.token_id)}`,
      {
        method: "DELETE",
        headers: authenticatedHeaders(),
      },
    );
    expect(revokeResponse.status).toBe(200);
    expect(await revokeResponse.json()).toMatchObject({
      token: {
        token_id: created.token.token_id,
        revoked_at: expect.any(String),
      },
    });
  });

  livePublishTest("artifact publish returns an immutable release and index fast path", async () => {
    const expectedManifest = {
      name: `registry-fast-path-${Date.now()}`,
      version: "0.1.0",
    };
    const archive = await buildInlinePackageArtifact({
      packageName: expectedManifest.name,
      packageVersion: expectedManifest.version,
      description: "Artifact publish fast-path live e2e package",
      license: "Apache-2.0",
      rootModule: "Registry_fast_path",
    });
    const publication = await publishArtifactArchive(archive);

    expect(publication.package_name).toBe(expectedManifest.name);
    expect(publication.package_version).toBe(expectedManifest.version);
    expect(publication.artifact_sha256).toMatch(/^[0-9a-f]{64}$/);
    expect(publication.claim.key).toBe(`claims/${expectedManifest.name}.json`);
    expect(publication.release.key).toBe(
      `releases/${expectedManifest.name}/${expectedManifest.version}.json`,
    );

    const manifestResponse = await fetch(publication.manifest.url);
    expect(manifestResponse.status).toBe(200);
    const manifest = (await manifestResponse.json()) as Record<string, unknown>;
    expect(manifest.package_name).toBe(expectedManifest.name);
    expect(manifest.package_version).toBe(expectedManifest.version);
    expect(manifest.artifact_sha256).toBe(publication.artifact_sha256);

    const sourceResponse = await fetch(publication.source_archive.url);
    expect(sourceResponse.status).toBe(200);
    expect(sourceResponse.headers.get("cache-control")).toBe("public, max-age=31536000, immutable");

    const config = await pollJson<IndexConfigPayload>(
      withCacheBust(`${indexBaseUrl}/${indexBasePath}/config.json`),
      (value) => value !== null && value.kind === "sparse",
    );
    expect(config).toEqual({
      schema_version: 1,
      kind: "sparse",
      package_path_strategy: "cargo-lowercase-v1",
      index_base_url: `${indexBaseUrl}/${indexBasePath}`,
      artifact_base_url: artifactBaseUrl,
    });

    const packageDocument = await pollJson<PackageIndexDocumentPayload>(
      withCacheBust(`${indexBaseUrl}/${packageIndexKey(expectedManifest.name)}`),
      (value) =>
        value !== null &&
        value.name === expectedManifest.name &&
        Array.isArray(value.releases) &&
        value.releases.some(
          (release) =>
            release.version === expectedManifest.version &&
            release.artifact_sha256 === publication.artifact_sha256,
        ),
    );
    expect(packageDocument.latest).toBe(expectedManifest.version);

    const indexedRelease = packageDocument.releases.find(
      (release) =>
        release.version === expectedManifest.version &&
        release.artifact_sha256 === publication.artifact_sha256,
    );
    expect(indexedRelease).toBeDefined();
    expect(indexedRelease?.manifest_key).toBe(publication.manifest.key);
    expect(indexedRelease?.source_key).toBe(publication.source_archive.key);
  });

  livePublishTest("published artifact becomes searchable and visible through views and events", async () => {
    const packageName = `registry-e2e-${Date.now()}`;
    const packageVersion = "0.1.0";
    const archive = await buildInlinePackageArtifact({
      packageName,
      packageVersion,
      description: "Artifact-published live e2e package",
      license: "Apache-2.0",
      categories: ["tooling"],
      rootModule: "Registry_e2e",
    });
    const publication = await publishArtifactArchive(archive);

    const searchResponse = await pollJson<SearchResponsePayload>(
      `${baseUrl}/v1/search?q=${encodeURIComponent(packageName)}`,
      (value) =>
        value !== null &&
        Array.isArray(value.results) &&
        value.results.some(
          (result) =>
            result.package_name === packageName &&
            result.latest_version === packageVersion,
        ),
    );
    expect(searchResponse.count).toBeGreaterThanOrEqual(1);

    const overview = await pollJson<Record<string, unknown>>(
      `${baseUrl}/v1/views/packages/${encodeURIComponent(packageName)}/overview`,
      (value) =>
        value !== null &&
        value.package_name === packageName &&
        value.latest_version === packageVersion,
    );
    expect(overview.package_name).toBe(packageName);

    const relations = await pollJson<Record<string, unknown>>(
      `${baseUrl}/v1/views/packages/${encodeURIComponent(packageName)}/relations`,
      (value) =>
        value !== null &&
        value.package_name === packageName &&
        Array.isArray(value.dependencies) &&
        Array.isArray(value.dependents),
    );
    expect(relations.package_name).toBe(packageName);

    const recent = await pollJson<Record<string, unknown>>(
      `${baseUrl}/v1/views/recent/packages`,
      (value) =>
        value !== null &&
        Array.isArray(value.packages) &&
        value.packages.some(
          (item) =>
            item !== null &&
            typeof item === "object" &&
            "package_name" in item &&
            item.package_name === packageName,
        ),
    );
    expect(Array.isArray(recent.packages)).toBe(true);

    const popular = await pollJson<Record<string, unknown>>(
      `${baseUrl}/v1/views/popular/packages`,
      (value) => value !== null && Array.isArray(value.packages),
    );
    expect(Array.isArray(popular.packages)).toBe(true);

    const categories = await pollJson<Record<string, unknown>>(
      `${baseUrl}/v1/views/categories`,
      (value) =>
        value !== null &&
        Array.isArray(value.categories) &&
        value.categories.some(
          (category) =>
            category !== null &&
            typeof category === "object" &&
            "name" in category &&
            category.name === "tooling",
        ),
    );
    expect(Array.isArray(categories.categories)).toBe(true);

    const events = await pollJson<RegistryEventsPayload>(
      `${baseUrl}/v1/events?limit=20`,
      (value) =>
        value !== null &&
        Array.isArray(value.events) &&
        value.events.some(
          (event) =>
            event.package_name === packageName &&
            event.package_version === packageVersion &&
            event.event_type === "package.published",
        ),
    );
    expect(events.events[0]?.created_at).toBeDefined();

    const packageEvents = await pollJson<RegistryEventsPayload>(
      `${baseUrl}/v1/packages/${encodeURIComponent(packageName)}/events?version=${encodeURIComponent(packageVersion)}`,
      (value) =>
        value !== null &&
        Array.isArray(value.events) &&
        value.events.some((event) => event.event_type === "package.published"),
    );
    expect(packageEvents.events.some((event) => event.event_type === "package.indexed")).toBe(true);
    expect(
      packageEvents.events.some(
        (event) =>
          event.payload !== null &&
          typeof event.payload === "object" &&
          "artifact_sha256" in event.payload &&
          event.payload.artifact_sha256 === publication.artifact_sha256,
      ),
    ).toBe(true);
  });

  liveAuthenticatedTest("user-owned artifact publishes show up on owner views", async () => {
    const publishToken = await createPublishToken(`owner-e2e-${Date.now()}`);
    const packageName = `registry-owned-e2e-${Date.now()}`;
    const packageVersion = "0.1.0";
    const archive = await buildInlinePackageArtifact({
      packageName,
      packageVersion,
      description: "User-owned artifact publish",
      license: "Apache-2.0",
    });

    const publication = await publishArtifactArchive(archive, publishToken);
    expect(publication.package_name).toBe(packageName);

    const overview = await pollJson<Record<string, unknown>>(
      `${baseUrl}/v1/views/packages/${encodeURIComponent(packageName)}/overview`,
      (value) =>
        value !== null &&
        value.package_name === packageName &&
        value.latest_version === packageVersion &&
        value.owner_github_login === githubLogin,
    );
    expect(overview.owner_github_login).toBe(githubLogin);

    const ownerPackages = await pollJson<Record<string, unknown>>(
      `${baseUrl}/v1/views/owners/${encodeURIComponent(githubLogin ?? "")}/packages`,
      (value) =>
        value !== null &&
        value.owner_github_login === githubLogin &&
        Array.isArray(value.packages) &&
        value.packages.some(
          (item) =>
            item !== null &&
            typeof item === "object" &&
            "package_name" in item &&
            item.package_name === packageName,
        ),
    );
    expect(ownerPackages.package_count).toBeGreaterThanOrEqual(1);
  });
});

async function buildInlinePackageArtifact(args: {
  packageName: string;
  packageVersion: string;
  description: string;
  license: string;
  categories?: string[];
  rootModule?: string;
}): Promise<Uint8Array<ArrayBuffer>> {
  const packageLines = [
    "[package]",
    `name = "${args.packageName}"`,
    `version = "${args.packageVersion}"`,
    "public = true",
    `description = "${args.description}"`,
    `license = "${args.license}"`,
  ];

  if (args.rootModule !== undefined) {
    packageLines.push(`root_module = "${args.rootModule}"`);
  }

  if (args.categories !== undefined && args.categories.length > 0) {
    packageLines.push(`categories = [${args.categories.map((value) => `"${value}"`).join(", ")}]`);
  }

  return await makeTarGz({
    "riot.toml": packageLines.join("\n"),
  }, "");
}

async function publishArtifactArchive(
  archive: Uint8Array<ArrayBuffer>,
  token = rootAuthToken ?? "",
): Promise<PublishPayload> {
  if (token.length === 0) {
    throw new Error("A publish token is required for artifact publish e2e tests.");
  }

  const response = await fetch(`${baseUrl}/v1/publish`, {
    method: "POST",
    headers: {
      authorization: `Bearer ${token}`,
      "content-type": "application/gzip",
    },
    body: archive,
  });

  if (response.status !== 200) {
    throw new Error(`Artifact publish failed with status ${response.status}: ${await response.text()}`);
  }

  return (await response.json()) as PublishPayload;
}

async function createPublishToken(name: string): Promise<string> {
  const response = await fetch(`${baseUrl}/v1/me/tokens`, {
    method: "POST",
    headers: {
      ...authenticatedHeaders(),
      "content-type": "application/json",
    },
    body: JSON.stringify({ name }),
  });

  if (response.status !== 201) {
    throw new Error(`Token creation failed with status ${response.status}: ${await response.text()}`);
  }

  const payload = (await response.json()) as {
    plaintext_token: string;
  };
  return payload.plaintext_token;
}

function trimTrailingSlash(value: string | undefined): string | null {
  if (value === undefined || value.length === 0) {
    return null;
  }

  return value.endsWith("/") ? value.slice(0, -1) : value;
}

function trimSlashes(value: string | undefined): string | null {
  if (value === undefined || value.length === 0) {
    return null;
  }

  return value.replace(/^\/+|\/+$/g, "");
}

function packageIndexKey(packageName: string): string {
  const normalized = packageName.toLowerCase();

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

async function pollJson<T>(
  url: string,
  accept: (value: T | null) => boolean,
  timeoutMs = 15000,
  intervalMs = 500,
): Promise<T> {
  const deadline = Date.now() + timeoutMs;
  let lastStatus = 0;

  while (Date.now() <= deadline) {
    const response = await fetch(url);
    lastStatus = response.status;

    if (response.status === 200) {
      const payload = (await response.json()) as T;
      if (accept(payload)) {
        return payload;
      }
    } else if (response.status !== 404) {
      throw new Error(`Unexpected status ${response.status} while polling ${url}.`);
    }

    await Bun.sleep(intervalMs);
  }

  throw new Error(`Timed out waiting for expected payload at ${url}. Last status was ${lastStatus}.`);
}

interface PublishPayload {
  package_name: string;
  package_version: string;
  artifact_sha256: string;
  manifest: {
    key: string;
    url: string;
    cdn_url: string;
  };
  source_archive: {
    key: string;
    url: string;
    cdn_url: string;
  };
  claim: {
    key: string;
    created: boolean;
  };
  release: {
    key: string;
    created: boolean;
  };
  materialization: {
    manifest: boolean;
    source: boolean;
  };
}

interface IndexConfigPayload {
  schema_version: number;
  kind: string;
  package_path_strategy: string;
  index_base_url: string;
  artifact_base_url: string;
}

interface PackageIndexDocumentPayload {
  schema_version: number;
  name: string;
  latest: string;
  updated_at: string;
  releases: PackageIndexReleasePayload[];
}

interface PackageIndexReleasePayload {
  version: string;
  published_at: string;
  canonical_locator: string;
  repo_url: string;
  subdir: string;
  artifact_sha256: string;
  manifest_key: string;
  source_key: string;
  dependencies: unknown[];
}

interface SearchResultPayload {
  package_name: string;
  latest_version: string;
  canonical_locator: string;
  repo_owner: string;
  repo_name: string;
}

interface SearchResponsePayload {
  query: string;
  count: number;
  results: SearchResultPayload[];
}

interface RegistryEventPayload {
  event_id: string;
  event_type: string;
  package_name?: string;
  package_version?: string;
  payload: Record<string, unknown>;
  created_at: string;
}

interface RegistryEventsPayload {
  events: RegistryEventPayload[];
}

function withCacheBust(url: string): string {
  const separator = url.includes("?") ? "&" : "?";
  return `${url}${separator}cb=${Date.now()}`;
}

function authenticatedHeaders(): Record<string, string> {
  return {
    accept: "application/json",
    cookie: sessionCookie ?? "",
  };
}
