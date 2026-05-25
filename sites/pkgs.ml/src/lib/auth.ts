import { getConfig } from "./config.ts";
import type {
  ApiTokensResponse,
  CreateApiTokenResponse,
  SessionResponse,
} from "./types.ts";

export async function fetchSession(request: Request): Promise<SessionResponse> {
  try {
    const response = await fetchFromRegistry(request, "/v1/me", {
      headers: {
        accept: "application/json",
      },
    });

    if (!response.ok) {
      return { authenticated: false };
    }

    return (await response.json()) as SessionResponse;
  } catch {
    return { authenticated: false };
  }
}

export async function fetchApiTokens(request: Request): Promise<ApiTokensResponse> {
  const response = await fetchFromRegistry(request, "/v1/me/tokens", {
    headers: {
      accept: "application/json",
    },
  });

  if (response.status === 401) {
    throw new Error("unauthorized");
  }

  if (!response.ok) {
    throw new Error(`Token list request failed: ${response.status}`);
  }

  return (await response.json()) as ApiTokensResponse;
}

export async function createApiToken(
  request: Request,
  name: string,
): Promise<CreateApiTokenResponse> {
  const response = await fetchFromRegistry(request, "/v1/me/tokens", {
    method: "POST",
    headers: {
      accept: "application/json",
      "content-type": "application/json",
    },
    body: JSON.stringify({ name }),
  });

  if (response.status === 401) {
    throw new Error("unauthorized");
  }

  if (!response.ok) {
    const payload = (await safeJson(response)) as { message?: string } | null;
    throw new Error(payload?.message ?? `Token creation failed: ${response.status}`);
  }

  return (await response.json()) as CreateApiTokenResponse;
}

export async function revokeApiToken(
  request: Request,
  tokenId: string,
): Promise<void> {
  const response = await fetchFromRegistry(request, `/v1/me/tokens/${encodeURIComponent(tokenId)}`, {
    method: "DELETE",
    headers: {
      accept: "application/json",
    },
  });

  if (response.status === 401) {
    throw new Error("unauthorized");
  }

  if (!response.ok) {
    const payload = (await safeJson(response)) as { message?: string } | null;
    throw new Error(payload?.message ?? `Token revocation failed: ${response.status}`);
  }
}

export async function yankPackageRelease(
  request: Request,
  packageName: string,
  version: string,
): Promise<{
  package_name: string;
  package_version: string;
  yanked: boolean;
  yanked_at?: string;
  yanked_by_github_login?: string;
}> {
  const response = await fetchFromRegistry(
    request,
    `/v1/me/packages/${encodeURIComponent(packageName)}/versions/${encodeURIComponent(version)}/yank`,
    {
      method: "POST",
      headers: {
        accept: "application/json",
      },
    },
  );

  if (response.status === 401) {
    throw new Error("unauthorized");
  }

  if (!response.ok) {
    const payload = (await safeJson(response)) as { message?: string } | null;
    throw new Error(payload?.message ?? `Release yank failed: ${response.status}`);
  }

  return (await response.json()) as {
    package_name: string;
    package_version: string;
    yanked: boolean;
    yanked_at?: string;
    yanked_by_github_login?: string;
  };
}

export function buildGitHubLoginUrl(returnTo: string): string {
  const { registryBaseUrl } = getConfig();
  return `${registryBaseUrl}/v1/auth/github/start?return_to=${encodeURIComponent(returnTo)}`;
}

export function buildLogoutUrl(returnTo: string): string {
  const { registryBaseUrl } = getConfig();
  return `${registryBaseUrl}/v1/auth/logout?return_to=${encodeURIComponent(returnTo)}`;
}

function buildRegistryUrl(path: string): string {
  const { registryBaseUrl } = getConfig();
  return `${registryBaseUrl}${path}`;
}

function forwardedCookieHeader(request: Request): string | null {
  return request.headers.get("cookie");
}

async function fetchFromRegistry(
  request: Request,
  path: string,
  init: RequestInit = {},
): Promise<Response> {
  const headers = new Headers(init.headers);
  const cookie = forwardedCookieHeader(request);
  if (cookie !== null) {
    headers.set("cookie", cookie);
  }

  return await fetch(buildRegistryUrl(path), {
    ...init,
    headers,
  });
}

async function safeJson(response: Response): Promise<unknown | null> {
  try {
    return await response.json();
  } catch {
    return null;
  }
}
