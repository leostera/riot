import type { Env, RegistryConfig } from "./types.ts";

export function getConfig(env: Env): RegistryConfig {
  return {
    cdnBaseUrl: trimTrailingSlash(env.CDN_BASE_URL ?? "https://cdn.pkgs.ml"),
    indexBaseUrl: trimTrailingSlash(env.INDEX_BASE_URL ?? "https://cdn.pkgs.ml"),
    indexBasePath: trimSlashes(env.INDEX_BASE_PATH ?? "index/v1"),
    indexRoutePath: trimSlashes(env.INDEX_ROUTE_PATH ?? "v1/index"),
    viewsBasePath: trimSlashes(env.VIEWS_BASE_PATH ?? "views/v1"),
    authCookieDomain: trimLeadingDot(env.AUTH_COOKIE_DOMAIN ?? "pkgs.ml"),
    pkgsWebBaseUrl: trimTrailingSlash(env.PKGS_WEB_BASE_URL ?? "https://pkgs.ml"),
    playWebBaseUrl: trimTrailingSlash(env.PLAY_WEB_BASE_URL ?? "https://play.riot.ml"),
  };
}

export function getGitHubApiBaseUrl(env: Env): string {
  return trimTrailingSlash(env.GITHUB_API_BASE_URL ?? "https://api.github.com");
}

function trimTrailingSlash(value: string): string {
  return value.endsWith("/") ? value.slice(0, -1) : value;
}

function trimSlashes(value: string): string {
  return value.replace(/^\/+|\/+$/g, "");
}

function trimLeadingDot(value: string): string {
  return value.startsWith(".") ? value.slice(1) : value;
}
