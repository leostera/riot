import { describe, expect, test } from "bun:test";
import { gzipSync } from "node:zlib";

const pkgsBaseUrl = trimTrailingSlash(process.env.PKGS_E2E_BASE_URL) ?? "https://pkgs.ml";
const publicRegistryBaseUrl =
  trimTrailingSlash(process.env.PKGS_E2E_REGISTRY_BASE_URL) ??
  trimTrailingSlash(process.env.PUBLIC_REGISTRY_BASE_URL) ??
  "https://api.pkgs.ml";
const registryApiBaseUrl =
  trimTrailingSlash(process.env.REGISTRY_E2E_BASE_URL) ??
  publicRegistryBaseUrl;
const searchApiBaseUrl =
  trimTrailingSlash(process.env.PKGS_E2E_SEARCH_API_BASE_URL) ?? `${registryApiBaseUrl}/v1/search`;
const indexBaseUrl =
  trimTrailingSlash(process.env.PKGS_E2E_INDEX_BASE_URL) ??
  trimTrailingSlash(process.env.PUBLIC_INDEX_BASE_URL) ??
  "https://cdn.pkgs.ml";
const indexBasePath = trimSlashes(process.env.PUBLIC_INDEX_BASE_PATH) ?? "index/v1";
const rootAuthToken = process.env.REGISTRY_E2E_ROOT_AUTH_TOKEN ?? null;
const sessionCookie = process.env.PKGS_E2E_SESSION_COOKIE ?? process.env.REGISTRY_E2E_SESSION_COOKIE ?? null;
const githubLogin = process.env.PKGS_E2E_GITHUB_LOGIN ?? process.env.REGISTRY_E2E_GITHUB_LOGIN ?? null;

const liveTest = test;
const livePublishTest = rootAuthToken === null ? test.skip : test;
const liveAuthenticatedTest =
  sessionCookie === null || githubLogin === null ? test.skip : test;

describe("pkgs.ml live e2e", () => {
  liveTest("landing page renders the search-first registry UI", async () => {
    const response = await fetch(`${pkgsBaseUrl}/`);
    expect(response.status).toBe(200);

    const html = await response.text();
    expect(html).toContain("pkgs.ml");
    expect(html).toContain("The Riot Community&apos;s Package Registry");
    expect(html).toContain('name="q"');
    expect(html).toContain("Login with GitHub");
    expect(html).toContain(
      `${publicRegistryBaseUrl}/v1/auth/github/start?return_to=${encodeURIComponent(`${pkgsBaseUrl}/`)}`,
    );
  });

  liveTest("homepage popular categories only showcases existing categories", async () => {
    const categories = await fetchCategoriesView();
    const html = await pollText(`${pkgsBaseUrl}/`, (page) => page.includes("Popular categories"));

    if (categories.categories.length === 0) {
      expect(html).toContain("No categories have been published yet.");
      return;
    }

    for (const category of categories.categories.slice(0, 6)) {
      expect(html).toContain(category.name);
      for (const packageName of category.packages.slice(0, 3)) {
        expect(html).toContain(packageName);
      }
    }
  });

  liveTest("homepage popular packages only showcases existing packages", async () => {
    const popular = await fetchPopularPackagesView();
    const html = await pollText(`${pkgsBaseUrl}/`, (page) => page.includes("Popular packages"));

    if (popular.packages.length === 0) {
      expect(html).toContain("No popular packages available yet.");
      return;
    }

    for (const item of popular.packages.slice(0, 6)) {
      expect(html).toContain(item.package_name);
      expect(html).toContain(`v${item.latest_version}`);
    }
  });

  liveTest("homepage recently updated only shows recently updated packages", async () => {
    const recent = await fetchRecentPackagesView();
    const html = await pollText(`${pkgsBaseUrl}/`, (page) => page.includes("Recently updated"));

    if (recent.packages.length === 0) {
      expect(html).toContain("No package releases have been indexed yet.");
      return;
    }

    for (const item of recent.packages.slice(0, 6)) {
      expect(html).toContain(item.package_name);
      expect(html).toContain(`v${item.latest_version}`);
    }
  });

  liveTest("login page links to the registry github auth start route", async () => {
    const returnTo = `${pkgsBaseUrl}/u/leostera/tokens`;
    const response = await fetch(
      `${pkgsBaseUrl}/login?return_to=${encodeURIComponent(returnTo)}`,
    );
    expect(response.status).toBe(200);

    const html = await response.text();
    expect(html).toContain("Continue with GitHub");
    expect(html).toContain(
      `${publicRegistryBaseUrl}/v1/auth/github/start?return_to=${encodeURIComponent(returnTo)}`,
    );
  });

  liveTest("unauthenticated token page redirects to login", async () => {
    const response = await fetch(`${pkgsBaseUrl}/u/leostera/tokens`, {
      redirect: "manual",
    });

    expect(response.status).toBe(302);
    const location = response.headers.get("location");
    expect(location).not.toBeNull();
    expect(location).toContain("/login?return_to=");
  });

  liveTest("search miss shows the empty-state copy", async () => {
    const query = `definitely-not-a-package-${Date.now()}`;
    const response = await fetch(`${pkgsBaseUrl}/?q=${encodeURIComponent(query)}`);

    expect(response.status).toBe(200);
    const html = await response.text();
    expect(html).toContain("No packages matched");
    expect(html).toContain(query);
  });

  liveTest("search results page lists all packages returned by the search api", async () => {
    const query = "leostera";
    const searchApi = await fetchSearchApi(query, 20);
    expect(searchApi.results.length).toBeGreaterThan(1);

    const html = await pollText(
      `${pkgsBaseUrl}/?q=${encodeURIComponent(query)}`,
      (page) =>
        page.includes("Search OCaml packages") &&
        searchApi.results.every((result) => page.includes(result.package_name)),
    );

    expect(html).toContain(`${searchApi.count} results`);
    for (const result of searchApi.results) {
      expect(html).toContain(result.package_name);
      expect(html).toContain(`v${result.latest_version}`);
    }
  });

  livePublishTest("published package appears across search and package pages", async () => {
    const publication = await publishPackage();

    const searchHtml = await pollText(
      `${pkgsBaseUrl}/?q=${encodeURIComponent(publication.package_name)}`,
      (html) =>
        html.includes("Search OCaml packages") &&
        html.includes(publication.package_name) &&
        html.includes(`v${publication.package_version}`),
    );
    expect(searchHtml).toContain(publication.package_name);
    expect(searchHtml).toContain(`v${publication.package_version}`);

    const packageHtml = await pollText(
      `${pkgsBaseUrl}/p/${encodeURIComponent(publication.package_name)}`,
      (html) =>
        html.includes(publication.package_name) &&
        html.includes(`v${publication.package_version}`) &&
        html.includes(`riot add ${publication.package_name}`),
    );
    expect(packageHtml).toContain("Discover Packages");
    expect(packageHtml).toContain(`riot add ${publication.package_name}`);

    const versionHtml = await pollText(
      `${pkgsBaseUrl}/p/${encodeURIComponent(publication.package_name)}/${encodeURIComponent(publication.package_version)}`,
      (html) =>
        html.includes(publication.package_name) && html.includes(`v${publication.package_version}`),
    );
    expect(versionHtml).toContain(publication.package_name);
    expect(versionHtml).toContain(`v${publication.package_version}`);
  });

  livePublishTest("package page shows every indexed version for the published package", async () => {
    const publication = await publishPackage();
    const document = await fetchPackageIndexDocument(publication.package_name);

    const packageHtml = await pollText(
      `${pkgsBaseUrl}/p/${encodeURIComponent(publication.package_name)}`,
      (html) =>
        html.includes('id="package-version-select"') &&
        document.releases.every((release) => html.includes(`v${release.version}`)),
    );

    expect(packageHtml).toContain('id="package-version-select"');
    for (const release of document.releases) {
      expect(packageHtml).toContain(`v${release.version}`);
    }
  });

  livePublishTest("every indexed package version has an accessible version page", async () => {
    const publication = await publishPackage();
    const document = await fetchPackageIndexDocument(publication.package_name);

    for (const release of document.releases) {
      const html = await pollText(
        `${pkgsBaseUrl}/p/${encodeURIComponent(publication.package_name)}/${encodeURIComponent(release.version)}`,
        (page) => page.includes(`v${release.version}`) && page.includes(publication.package_name),
      );

      expect(html).toContain(publication.package_name);
      expect(html).toContain(`v${release.version}`);
    }
  });

  liveTest("owner page lists recently published packages for the owner", async () => {
    const owner = "leostera";
    const packages = await fetchSearchApi(owner);
    const ownerPackages = packages.results.filter(
      (result) => result.repo_owner.toLowerCase() === owner,
    );

    expect(ownerPackages.length).toBeGreaterThan(0);

    const html = await pollText(
      `${pkgsBaseUrl}/u/${encodeURIComponent(owner)}`,
      (page) =>
        page.includes("Recently published packages") &&
        ownerPackages.every((result) => page.includes(result.package_name)),
    );

    expect(html).toContain(`@${owner}`);
    expect(html).toContain("Recently published packages");
    for (const result of ownerPackages) {
      expect(html).toContain(result.package_name);
    }
  });

  liveAuthenticatedTest("authenticated owner page shows the api tokens section", async () => {
    const response = await fetch(`${pkgsBaseUrl}/u/${encodeURIComponent(githubLogin ?? "")}`, {
      headers: {
        cookie: sessionCookie ?? "",
      },
    });

    expect(response.status).toBe(200);
    const html = await response.text();
    expect(html).toContain("API Tokens");
    expect(html).toContain("Manage API tokens");
  });

  liveAuthenticatedTest("token page shows a new publish token only once", async () => {
    const tokenName = `pkgs-e2e-${Date.now()}`;
    const createResponse = await fetch(
      `${pkgsBaseUrl}/api/me/tokens`,
      {
        method: "POST",
        headers: {
          accept: "application/json",
          "content-type": "application/json",
          cookie: sessionCookie ?? "",
        },
        body: JSON.stringify({ name: tokenName }),
      },
    );

    expect(createResponse.status).toBe(201);
    const created = (await createResponse.json()) as {
      plaintext_token: string;
      token: { token_id: string; name: string };
    };
    expect(created.token.name).toBe(tokenName);
    const plaintextToken = created.plaintext_token;
    expect(plaintextToken).toMatch(/^sk-[A-Za-z0-9_-]+$/);
    expect(plaintextToken.length).toBeGreaterThan(0);

    const revisitResponse = await fetch(
      `${pkgsBaseUrl}/u/${encodeURIComponent(githubLogin ?? "")}/tokens`,
      {
        headers: {
          cookie: sessionCookie ?? "",
        },
      },
    );
    expect(revisitResponse.status).toBe(200);
    const revisitHtml = await revisitResponse.text();
    expect(revisitHtml).toContain("API Tokens");
    expect(revisitHtml).not.toContain("New publish token created");
    expect(revisitHtml).not.toContain(plaintextToken);

    const revokeResponse = await fetch(
      `${pkgsBaseUrl}/api/me/tokens/${encodeURIComponent(created.token.token_id)}`,
      {
        method: "DELETE",
        headers: {
          accept: "application/json",
          cookie: sessionCookie ?? "",
        },
      },
    );
    expect(revokeResponse.status).toBe(200);
  });
});

async function publishPackage(): Promise<PublishPayload> {
  const packageName = `pkgs-e2e-${Date.now()}`;
  const packageVersion = "0.1.0";
  const response = await fetch(`${registryApiBaseUrl}/v1/publish`, {
    method: "POST",
    headers: {
      authorization: `Bearer ${rootAuthToken}`,
      "content-type": "application/gzip",
    },
    body: buildInlinePackageArtifact(packageName, packageVersion),
  });

  expect(response.status).toBe(200);
  return (await response.json()) as PublishPayload;
}

async function pollText(
  url: string,
  accept: (html: string) => boolean,
  timeoutMs = 15_000,
  intervalMs = 500,
): Promise<string> {
  const deadline = Date.now() + timeoutMs;
  let lastStatus = 0;

  while (Date.now() <= deadline) {
    const response = await fetch(url, {
      headers: sessionCookie === null ? undefined : { cookie: sessionCookie },
    });
    lastStatus = response.status;

    if (response.status === 200) {
      const html = await response.text();
      if (accept(html)) {
        return html;
      }
    } else if (response.status !== 404) {
      throw new Error(`Unexpected status ${response.status} while polling ${url}.`);
    }

    await Bun.sleep(intervalMs);
  }

  throw new Error(`Timed out waiting for HTML at ${url}. Last status was ${lastStatus}.`);
}

function trimTrailingSlash(value: string | undefined): string | null {
  if (value === undefined || value.length === 0) {
    return null;
  }

  return value.endsWith("/") ? value.slice(0, -1) : value;
}

interface PublishPayload {
  package_name: string;
  package_version: string;
}

interface SearchApiResponse {
  query: string;
  count: number;
  results: Array<{
    package_name: string;
    latest_version: string;
    repo_owner: string;
  }>;
}

interface WebPackageListItem {
  package_name: string;
  latest_version: string;
}

interface CategoriesIndexDocument {
  categories: Array<{
    name: string;
    packages: string[];
  }>;
}

interface RecentPackagesDocument {
  packages: WebPackageListItem[];
}

interface PopularPackagesDocument {
  packages: WebPackageListItem[];
}

interface PackageIndexDocument {
  name: string;
  releases: Array<{
    version: string;
  }>;
}

async function fetchSearchApi(query: string, limit = 100): Promise<SearchApiResponse> {
  const response = await fetch(`${searchApiBaseUrl}?q=${encodeURIComponent(query)}&limit=${limit}`, {
    headers: {
      accept: "application/json",
    },
  });

  expect(response.status).toBe(200);
  return (await response.json()) as SearchApiResponse;
}

async function fetchPackageIndexDocument(packageName: string): Promise<PackageIndexDocument> {
  const response = await fetch(`${indexBaseUrl}/${packageIndexKey(packageName)}`, {
    headers: {
      accept: "application/json",
    },
  });

  expect(response.status).toBe(200);
  return (await response.json()) as PackageIndexDocument;
}

function buildInlinePackageArtifact(packageName: string, packageVersion: string): ArrayBuffer {
  const entries = [
    tarEntry(
      "riot.toml",
      [
        "[package]",
        `name = "${packageName}"`,
        `version = "${packageVersion}"`,
        "public = true",
        `description = "${packageName} package for pkgs.ml live e2e"`,
        'license = "Apache-2.0"',
      ].join("\n"),
    ),
    tarEntry("src/main.ml", "let hello = \"riot\"\n"),
  ];

  const tarBody = Buffer.concat([...entries, Buffer.alloc(1024, 0)]);
  const bytes = new Uint8Array(gzipSync(tarBody));
  return bytes.buffer.slice(bytes.byteOffset, bytes.byteOffset + bytes.byteLength);
}

function tarEntry(path: string, contents: string): Buffer {
  const data = Buffer.from(contents, "utf8");
  const header = Buffer.alloc(512, 0);

  writeString(header, 0, 100, path);
  writeOctal(header, 100, 8, 0o644);
  writeOctal(header, 108, 8, 0);
  writeOctal(header, 116, 8, 0);
  writeOctal(header, 124, 12, data.length);
  writeOctal(header, 136, 12, Math.floor(Date.now() / 1000));
  header.fill(0x20, 148, 156);
  header[156] = "0".charCodeAt(0);
  writeString(header, 257, 6, "ustar");
  writeString(header, 263, 2, "00");

  const checksum = header.reduce((sum, value) => sum + value, 0);
  writeOctal(header, 148, 8, checksum);

  const padding = (512 - (data.length % 512)) % 512;
  return Buffer.concat([header, data, Buffer.alloc(padding, 0)]);
}

function writeString(target: Buffer, offset: number, length: number, value: string): void {
  const source = Buffer.from(value, "utf8");
  source.copy(target, offset, 0, Math.min(source.length, length));
}

function writeOctal(target: Buffer, offset: number, length: number, value: number): void {
  const rendered = value.toString(8).padStart(length - 2, "0");
  writeString(target, offset, length - 1, rendered);
  target[offset + length - 1] = 0;
}

async function fetchCategoriesView(): Promise<CategoriesIndexDocument> {
  return await fetchViewDocument<CategoriesIndexDocument>("categories");
}

async function fetchRecentPackagesView(): Promise<RecentPackagesDocument> {
  return await fetchViewDocument<RecentPackagesDocument>("recent/packages");
}

async function fetchPopularPackagesView(): Promise<PopularPackagesDocument> {
  return await fetchViewDocument<PopularPackagesDocument>("popular/packages");
}

async function fetchViewDocument<T>(path: string): Promise<T> {
  const response = await fetch(`${registryApiBaseUrl}/v1/views/${path}`, {
    headers: {
      accept: "application/json",
    },
  });

  expect(response.status).toBe(200);
  return (await response.json()) as T;
}

function packageIndexKey(packageName: string): string {
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

function trimSlashes(value: string | undefined): string | null {
  if (value === undefined || value.length === 0) {
    return null;
  }

  return value.replace(/^\/+|\/+$/g, "");
}
