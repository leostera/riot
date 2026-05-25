import {
  buildClearedSessionCookie,
  buildPlaySessionCompletionUrl,
  buildSessionCookie,
  completeGitHubAuthorization,
  createSessionHandoff,
  createGitHubAuthorizationUrl,
  createPublishApiToken,
  isPlayReturnTo,
  logoutSession,
  readAuthenticatedSession,
  requireAuthenticatedSession,
  requirePublishActor,
  resolveReturnTo,
  revokeApiToken,
} from "./auth.ts";
import { getConfig } from "./config.ts";
import { HttpError } from "./errors.ts";
import { json, methodNotAllowed } from "./http.ts";
import {
  listRegistryEvents,
  listPackageRegistryEvents,
  listApiTokenRecords,
  readCategoriesIndexDocument,
  readPackageClaim,
  readPublishedRelease,
  readOwnerPackagesDocument,
  readPackageDownloadsDocument,
  readPackageOverviewDocument,
  readPackageRelationsDocument,
  readPopularPackagesDocument,
  readRecentPackagesDocument,
  readRegistryStatsDashboardDocument,
  readRegistryStatsSummaryDocument,
  writeRegistryEvent,
  yankPublishedRelease,
} from "./metadata-db.ts";
import {
  listArchiveFilesFromTarGz,
  readArchiveFilesFromTarGz,
  readFirstArchiveFileFromTarGz,
} from "./archive.ts";
import { searchPackages } from "./search-db.ts";
import { reindexPublishedRelease } from "./indexing.ts";
import { v7 as uuidv7 } from "uuid";
import {
  artifactManifestKey,
  artifactProxyUrl,
  artifactSourceArchiveKey,
  buildIndexConfigDocument,
  cdnObjectUrl,
  indexConfigKey,
  riotLatestMetadataKey,
} from "./storage.ts";
import type {
  ApiTokenRecord,
  AuthenticatedActor,
  Env,
  IndexedPackageRelease,
  PackageIndexDocument,
  PublishedPackageRelease,
  RegistryStatsWindowKey,
} from "./types.ts";

export async function handleRequest(
  request: Request,
  env: Env,
  _ctx: ExecutionContext,
): Promise<Response> {
  try {
    return await routeRequest(request, env);
  } catch (error) {
    const httpError = normalizeError(error);
    const requestId = crypto.randomUUID();

    return json(
      {
        error: httpError.error,
        message: httpError.message,
        request_id: requestId,
      },
      { status: httpError.status },
    );
  }
}

async function routeRequest(
  request: Request,
  env: Env,
): Promise<Response> {
  const url = new URL(request.url);
  const path = trimSlashes(url.pathname);

  if (path === "") {
    if (request.method !== "GET") {
      return methodNotAllowed(["GET"]);
    }

    return json({
      service: "riot-package-registry",
      routes: {
        publish_artifact: "/v1/publish",
        views_package_overview: "/v1/views/packages/<package-name>/overview",
        views_package_readme: "/v1/views/packages/<package-name>/readme?version=<version>",
        views_package_examples: "/v1/views/packages/<package-name>/examples?version=<version>",
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
        yank_release: "/v1/me/packages/<package-name>/versions/<version>/yank",
        search: "/v1/search?q=<query>",
        events: "/v1/events?limit=<count>&after=<event-id>",
        package_events: "/v1/packages/<package-name>/events?version=<version>&limit=<count>",
      },
      cdn_routes: {
        index_config: "/index/v1/config.json",
        index_package: "/index/v1/<sharded-package-document>.json",
        artifact_download: "/<artifact-key>",
        riot_latest_metadata: "/riot/latest.json",
        riot_release_metadata: "/riot/riot-<version>.json",
      },
      legacy_routes: {
        publish_artifact: "/api/v1/publish",
        index_config: "/api/v1/index/config.json",
        index_package: "/api/v1/index/<sharded-package-document>.json",
        artifact_download: "/api/v1/artifacts/<artifact-key>",
        riot_latest_metadata: "/api/v1/riot/latest.json",
        riot_release_metadata: "/api/v1/riot/riot-<version>.json",
        views_package_overview: "/api/v1/views/packages/<package-name>/overview",
        views_package_readme: "/api/v1/views/packages/<package-name>/readme?version=<version>",
        views_package_examples: "/api/v1/views/packages/<package-name>/examples?version=<version>",
        views_package_downloads: "/api/v1/views/packages/<package-name>/downloads",
        views_package_relations: "/api/v1/views/packages/<package-name>/relations",
        views_recent_packages: "/api/v1/views/recent/packages",
        views_popular_packages: "/api/v1/views/popular/packages",
        views_categories: "/api/v1/views/categories",
        views_owner_packages: "/api/v1/views/owners/<github-login>/packages",
        views_stats_summary: "/api/v1/views/stats/summary",
        views_stats_dashboard: "/api/v1/views/stats/dashboard",
        auth_github_start: "/auth/github/start?return_to=<url>",
        auth_github_callback: "/auth/github/callback?code=<code>&state=<state>",
        auth_logout: "/auth/logout",
        me: "/api/v1/me",
        tokens: "/api/v1/me/tokens",
        yank_release: "/api/v1/me/packages/<package-name>/versions/<version>/yank",
        search: "/api/v1/search?q=<query>",
        events: "/api/v1/events?limit=<count>&after=<event-id>",
        package_events: "/api/v1/packages/<package-name>/events?version=<version>&limit=<count>",
      },
      cdn_base_url: getConfig(env).cdnBaseUrl,
      index_base_url: `${getConfig(env).indexBaseUrl}/${getConfig(env).indexBasePath}`,
    });
  }

  const indexStorageKey = resolveIndexStorageKey(path, getConfig(env));
  if (indexStorageKey !== null) {
    if (request.method !== "GET" && request.method !== "HEAD") {
      return methodNotAllowed(["GET", "HEAD"]);
    }

    return await handleIndexDocument(request, env, indexStorageKey);
  }

  const artifactStorageKey = resolveArtifactStorageKey(path);
  if (artifactStorageKey !== null) {
    if (request.method !== "GET" && request.method !== "HEAD") {
      return methodNotAllowed(["GET", "HEAD"]);
    }
    return await handleArtifactObject(request, env, artifactStorageKey);
  }

  if (matchesPath(path, "v1/riot/latest.json", "api/v1/riot/latest.json")) {
    if (request.method !== "GET" && request.method !== "HEAD") {
      return methodNotAllowed(["GET", "HEAD"]);
    }

    return await handleRiotLatestMetadata(request, env);
  }

  const riotReleaseMetadataKey = resolveRiotReleaseMetadataStorageKey(path);
  if (riotReleaseMetadataKey !== null) {
    if (request.method !== "GET" && request.method !== "HEAD") {
      return methodNotAllowed(["GET", "HEAD"]);
    }

    return await handleRiotReleaseMetadata(request, env, riotReleaseMetadataKey);
  }

  if (matchesPath(path, "v1/auth/github/start", "auth/github/start")) {
    if (request.method !== "GET") {
      return methodNotAllowed(["GET"]);
    }

    const authorizationUrl = await createGitHubAuthorizationUrl(
      env,
      url,
      url.searchParams.get("return_to"),
    );
    return redirect(authorizationUrl);
  }

  if (matchesPath(path, "v1/auth/github/callback", "auth/github/callback")) {
    if (request.method !== "GET") {
      return methodNotAllowed(["GET"]);
    }

    const code = url.searchParams.get("code");
    const state = url.searchParams.get("state");
    if (code === null || state === null) {
      throw new HttpError(
        400,
        "invalid_oauth_callback",
        "GitHub OAuth callback requires both code and state parameters.",
      );
    }

    const { session, returnTo } = await completeGitHubAuthorization(env, url, code, state);
    if (isPlayReturnTo(env, returnTo)) {
      const handoff = await createSessionHandoff(env, session, returnTo);
      return redirect(buildPlaySessionCompletionUrl(env, handoff.handoff_id));
    }

    return redirect(returnTo, {
      "set-cookie": buildSessionCookie(env, session),
    });
  }

  if (matchesPath(path, "v1/auth/logout", "auth/logout")) {
    if (request.method !== "POST") {
      return methodNotAllowed(["POST"]);
    }

    return await handleLogout(request, env, url);
  }

  if (matchesPath(path, "v1/me", "api/v1/me")) {
    if (request.method !== "GET") {
      return methodNotAllowed(["GET"]);
    }

    return await handleCurrentSession(request, env);
  }

  if (matchesPath(path, "v1/search", "api/v1/search")) {
    if (request.method !== "GET") {
      return methodNotAllowed(["GET"]);
    }

    return await handleSearch(env, url);
  }

  if (matchesPath(path, "v1/events", "api/v1/events")) {
    if (request.method !== "GET") {
      return methodNotAllowed(["GET"]);
    }

    return await handleEvents(env, url);
  }

  const packageEventsRoute = parsePackageEventsRoute(path);
  if (packageEventsRoute !== null) {
    if (request.method !== "GET") {
      return methodNotAllowed(["GET"]);
    }

    return await handlePackageEvents(env, url, packageEventsRoute.packageName);
  }

  const viewRoute = parseViewRoute(path);
  if (viewRoute !== null) {
    if (request.method !== "GET") {
      return methodNotAllowed(["GET"]);
    }

    return await handleViewDocument(env, url, viewRoute);
  }

  if (matchesPath(path, "v1/me/tokens", "api/v1/me/tokens")) {
    if (request.method === "GET") {
      return await handleListTokens(request, env);
    }

    if (request.method === "POST") {
      return await handleCreateToken(request, env);
    }

    return methodNotAllowed(["GET", "POST"]);
  }

  if (path.startsWith("v1/me/tokens/") || path.startsWith("api/v1/me/tokens/")) {
    if (request.method !== "DELETE") {
      return methodNotAllowed(["DELETE"]);
    }

    const tokenId = decodeURIComponent(
      path.startsWith("v1/me/tokens/")
        ? path.slice("v1/me/tokens/".length)
        : path.slice("api/v1/me/tokens/".length),
    );
    return await handleDeleteToken(request, env, tokenId);
  }

  const yankRoute = parseYankRoute(path);
  if (yankRoute !== null) {
    if (request.method !== "POST") {
      return methodNotAllowed(["POST"]);
    }

    return await handleYankRelease(request, env, yankRoute.packageName, yankRoute.version);
  }

  if (matchesPath(path, "v1/publish", "api/v1/publish")) {
    if (request.method !== "POST") {
      return methodNotAllowed(["POST"]);
    }

    return await handleArtifactPublish(request, env);
  }

  throw new HttpError(404, "not_found", "Route does not exist.");
}

async function handleArtifactPublish(
  request: Request,
  env: Env,
): Promise<Response> {
  const contentType = request.headers.get("content-type") ?? "";
  if (!contentType.includes("application/gzip") && !contentType.includes("application/x-gzip")) {
    throw new HttpError(
      415,
      "unsupported_media_type",
      "Artifact publish requires Content-Type: application/gzip.",
    );
  }

  const actor = await requirePublishActor(request, env);
  const archiveBytes = new Uint8Array(await request.arrayBuffer());
  if (archiveBytes.byteLength === 0) {
    throw new HttpError(400, "invalid_package_archive", "Artifact publish requires a non-empty tarball body.");
  }

  const publishResult = await publishArtifactThroughCoordinator(env, archiveBytes, actor);

  return json({
    package_name: publishResult.packageName,
    package_version: publishResult.packageVersion,
    artifact_sha256: publishResult.artifactSha256,
    manifest: {
      key: publishResult.manifestKey,
      url: artifactProxyUrl(getConfig(env), publishResult.manifestKey),
      cdn_url: cdnObjectUrl(getConfig(env), publishResult.manifestKey),
    },
    source_archive: {
      key: publishResult.sourceKey,
      url: artifactProxyUrl(getConfig(env), publishResult.sourceKey),
      cdn_url: artifactProxyUrl(getConfig(env), publishResult.sourceKey),
    },
    claim: {
      key: publishResult.claimKey,
      created: publishResult.claimCreated,
    },
    release: {
      key: publishResult.releaseKey,
      created: publishResult.releaseCreated,
    },
    materialization: {
      manifest: !publishResult.manifestCreated,
      source: !publishResult.sourceCreated,
    },
  });
}

async function publishArtifactThroughCoordinator(
  env: Env,
  archiveBytes: Uint8Array<ArrayBuffer>,
  actor: AuthenticatedActor,
): Promise<PublishedPackageRelease> {
  const id = env.PUBLICATION_COORDINATOR.idFromName("global");
  const stub = env.PUBLICATION_COORDINATOR.get(id);
  const response = await stub.fetch("https://publication-coordinator.internal/publish", {
    method: "POST",
    headers: {
      "content-type": "application/gzip",
      "x-publication-operation": "publish-artifact",
      "x-publication-actor": JSON.stringify(actor),
    },
    body: archiveBytes,
  });

  if (!response.ok) {
    const payload = (await response.json()) as { error?: string; message?: string };
    throw new HttpError(
      response.status,
      payload.error ?? "publish_failed",
      payload.message ?? "Package publish failed.",
    );
  }

  const payload = (await response.json()) as {
    artifact_sha256: string;
    source_key: string;
    manifest_key: string;
    source_created: boolean;
    manifest_created: boolean;
    package_name: string;
    package_version: string;
    claim_key: string;
    release_key: string;
    claim_created: boolean;
    release_created: boolean;
    index_changed: boolean;
  };

  return {
    artifactSha256: payload.artifact_sha256,
    sourceKey: payload.source_key,
    manifestKey: payload.manifest_key,
    sourceCreated: payload.source_created,
    manifestCreated: payload.manifest_created,
    packageName: payload.package_name,
    packageVersion: payload.package_version,
    claimKey: payload.claim_key,
    releaseKey: payload.release_key,
    claimCreated: payload.claim_created,
    releaseCreated: payload.release_created,
    indexChanged: payload.index_changed,
  };
}

async function handleLogout(request: Request, env: Env, url: URL): Promise<Response> {
  await logoutSession(request, env);
  const returnTo =
    (await readReturnToFromRequest(request)) ?? url.searchParams.get("return_to");

  if (wantsJson(request)) {
    return json(
      { ok: true },
      {
        headers: {
          "set-cookie": buildClearedSessionCookie(env),
        },
      },
    );
  }

  return redirect(resolveReturnTo(env, returnTo), {
    "set-cookie": buildClearedSessionCookie(env),
  });
}

async function handleCurrentSession(request: Request, env: Env): Promise<Response> {
  const authenticated = await readAuthenticatedSession(request, env);
  if (authenticated === null) {
    return json({
      authenticated: false,
    });
  }

  return json({
    authenticated: true,
    user: authenticated.user,
  });
}

async function handleSearch(env: Env, url: URL): Promise<Response> {
  const query = url.searchParams.get("q");

  if (query === null || query.trim().length === 0) {
    const config = getConfig(env);
    return json({
      service: "riot-package-registry",
      route: "/v1/search?q=<query>",
      source: {
        package_index_base_url: `${config.indexBaseUrl}/${config.indexBasePath}`,
        updated_during_publish: true,
      },
    });
  }

  const limit = clampInteger(url.searchParams.get("limit"), 20, 1, 100);
  const offset = clampInteger(url.searchParams.get("offset"), 0, 0, 10_000);
  const results = await searchPackages(env.SEARCH_DB, query, limit, offset);

  return json({
    query,
    count: results.length,
    results,
  });
}

async function handleEvents(env: Env, url: URL): Promise<Response> {
  const limit = clampInteger(url.searchParams.get("limit"), 100, 1, 500);
  const after = url.searchParams.get("after") ?? undefined;
  const events = await listRegistryEvents(env.SEARCH_DB, limit, after);

  return json({
    limit,
    after,
    events,
  });
}

async function handlePackageEvents(
  env: Env,
  url: URL,
  packageName: string,
): Promise<Response> {
  const limit = clampInteger(url.searchParams.get("limit"), 25, 1, 100);
  const packageVersion = url.searchParams.get("version") ?? undefined;
  const events = await listPackageRegistryEvents(env.SEARCH_DB, packageName, packageVersion, limit);

  return json({
    package_name: packageName,
    package_version: packageVersion,
    limit,
    events,
  });
}

async function handleListTokens(request: Request, env: Env): Promise<Response> {
  const { user } = await requireAuthenticatedSession(request, env);
  const records = await listApiTokenRecords(env.SEARCH_DB, user.user_id);

  return json({
    user,
    tokens: records.filter((record) => record.revoked_at === undefined).map(serializeTokenRecord),
  });
}

async function handleViewDocument(
  env: Env,
  url: URL,
  viewRoute: ParsedViewRoute,
): Promise<Response> {
  switch (viewRoute.kind) {
    case "package_overview": {
      const document = await readPackageOverviewDocument(env.SEARCH_DB, viewRoute.packageName);
      if (document === null) {
        throw new HttpError(404, "view_not_found", "Package overview was not found.");
      }

      return json(document);
    }
    case "package_readme": {
      const document = await readPackageReadmeDocument(
        env,
        viewRoute.packageName,
        url.searchParams.get("version") ?? undefined,
      );
      if (document === null) {
        throw new HttpError(404, "view_not_found", "Package README was not found.");
      }

      return json(document);
    }
    case "package_examples": {
      const document = await readPackageExamplesDocument(
        env,
        viewRoute.packageName,
        url.searchParams.get("version") ?? undefined,
      );
      if (document === null) {
        throw new HttpError(404, "view_not_found", "Package examples were not found.");
      }

      return json(document);
    }
    case "package_downloads": {
      const document = await readPackageDownloadsDocument(env.SEARCH_DB, viewRoute.packageName);
      if (document === null) {
        throw new HttpError(404, "view_not_found", "Package download stats were not found.");
      }

      return json(document);
    }
    case "package_relations": {
      const document = await readPackageRelationsDocument(env.SEARCH_DB, viewRoute.packageName);
      if (document === null) {
        throw new HttpError(404, "view_not_found", "Package relations were not found.");
      }

      return json(document);
    }
    case "recent_packages": {
      const document = await readRecentPackagesDocument(env.SEARCH_DB);
      if (document === null) {
        throw new HttpError(404, "view_not_found", "Recent packages view was not found.");
      }

      return json(document);
    }
    case "popular_packages": {
      const document = await readPopularPackagesDocument(env.SEARCH_DB);
      if (document === null) {
        throw new HttpError(404, "view_not_found", "Popular packages view was not found.");
      }

      return json(document);
    }
    case "categories": {
      const document = await readCategoriesIndexDocument(env.SEARCH_DB);
      if (document === null) {
        throw new HttpError(404, "view_not_found", "Categories view was not found.");
      }

      return json(document);
    }
    case "owner_packages": {
      const document = await readOwnerPackagesDocument(env.SEARCH_DB, viewRoute.ownerGithubLogin);
      if (document === null) {
        throw new HttpError(404, "view_not_found", "Owner packages view was not found.");
      }

      return json(document);
    }
    case "stats_summary":
      return json(await readRegistryStatsSummaryDocument(env.SEARCH_DB));
    case "stats_dashboard":
      return json(await readRegistryStatsDashboardDocument(env.SEARCH_DB, parseStatsWindow(url)));
  }
}

function parseStatsWindow(url: URL): RegistryStatsWindowKey {
  const value = url.searchParams.get("window");

  switch (value) {
    case "all":
    case "year":
    case "30d":
    case "7d":
      return value;
    default:
      return "30d";
  }
}

async function handleIndexDocument(
  request: Request,
  env: Env,
  storageKey: string,
): Promise<Response> {
  const config = getConfig(env);
  if (storageKey === indexConfigKey(config)) {
    return await respondWithIndexJson(request, buildIndexConfigDocument(config));
  }

  const object = await env.ML_PKGS_CDN.get(storageKey);
  if (object === null) {
    throw new HttpError(404, "index_not_found", "Index document was not found.");
  }

  const document = sanitizePackageIndexDocument(await object.json<PackageIndexDocument>());
  if (document === null) {
    throw new HttpError(404, "index_not_found", "Index document was not found.");
  }

  return await respondWithIndexJson(request, document);
}

async function handleArtifactObject(
  request: Request,
  env: Env,
  storageKey: string,
): Promise<Response> {
  const object = await env.ML_PKGS_CDN.get(storageKey);
  if (object === null) {
    throw new HttpError(404, "artifact_not_found", "Artifact was not found.");
  }

  const headers = new Headers();
  object.writeHttpMetadata(headers);
  headers.set("cache-control", "no-store");
  headers.set("etag", object.httpEtag);
  headers.set("content-length", String(object.size));

  if (request.method === "HEAD") {
    return new Response(null, {
      status: 200,
      headers,
    });
  }

  return new Response(object.body, {
    status: 200,
    headers,
  });
}

async function handleRiotLatestMetadata(request: Request, env: Env): Promise<Response> {
  return await handleRiotReleaseMetadata(request, env, riotLatestMetadataKey());
}

async function handleRiotReleaseMetadata(
  request: Request,
  env: Env,
  storageKey: string,
): Promise<Response> {
  const object = await env.ML_PKGS_CDN.get(storageKey);
  if (object === null) {
    throw new HttpError(404, "artifact_not_found", "Release metadata was not found.");
  }

  const headers = new Headers();
  object.writeHttpMetadata(headers);
  headers.set("cache-control", "no-store");
  headers.set("etag", object.httpEtag);
  headers.set("content-length", String(object.size));

  if (request.method === "HEAD") {
    return new Response(null, {
      status: 200,
      headers,
    });
  }

  return new Response(object.body, {
    status: 200,
    headers,
  });
}

async function handleCreateToken(request: Request, env: Env): Promise<Response> {
  const { user } = await requireAuthenticatedSession(request, env);
  const payload = await readBodyObject(request);
  const rawName = payload.name;

  if (typeof rawName !== "string") {
    throw new HttpError(400, "invalid_token_name", "Token creation requires a name string.");
  }

  const { plaintext, record } = await createPublishApiToken(env, user, rawName);
  return json(
    {
      plaintext_token: plaintext,
      token: serializeTokenRecord(record),
    },
    { status: 201 },
  );
}

async function handleDeleteToken(
  request: Request,
  env: Env,
  tokenId: string,
): Promise<Response> {
  const { user } = await requireAuthenticatedSession(request, env);
  const revoked = await revokeApiToken(env, user, tokenId);
  if (revoked === null) {
    throw new HttpError(404, "token_not_found", "Token does not exist.");
  }

  return json({
    token: serializeTokenRecord(revoked),
  });
}

async function handleYankRelease(
  request: Request,
  env: Env,
  packageName: string,
  version: string,
): Promise<Response> {
  const { user } = await requireAuthenticatedSession(request, env);
  const claim = await readPackageClaim(env.SEARCH_DB, packageName);
  if (
    claim === null ||
    claim.owner_github_login === undefined ||
    claim.owner_github_login.toLowerCase() !== user.github_login.toLowerCase()
  ) {
    throw new HttpError(403, "forbidden", "Only the package owner can yank releases.");
  }

  const now = new Date().toISOString();
  const release = await yankPublishedRelease(
    env.SEARCH_DB,
    packageName,
    version,
    user.github_login,
    now,
  );
  if (release === null) {
    throw new HttpError(404, "release_not_found", "Release does not exist.");
  }

  await reindexPublishedRelease(env, packageName, version, now);
  await writeRegistryEvent(
    env.SEARCH_DB,
    {
      event_id: uuidv7({
        msecs: Date.parse(now),
      }),
      event_type: "package.yanked",
      package_name: release.package_name,
      package_version: release.package_version,
      package_locator: release.package_locator,
      payload: {
        artifact_sha256: release.artifact_sha256,
        yanked_at: release.yanked_at,
        yanked_by_github_login: release.yanked_by_github_login,
      },
      created_at: now,
    },
  );

  return json({
    package_name: release.package_name,
    package_version: release.package_version,
    yanked: true,
    yanked_at: release.yanked_at,
    yanked_by_github_login: release.yanked_by_github_login,
  });
}

interface ParsedPackageEventsRoute {
  packageName: string;
}

interface ParsedYankRoute {
  packageName: string;
  version: string;
}

type ParsedViewRoute =
  | { kind: "package_overview"; packageName: string }
  | { kind: "package_readme"; packageName: string }
  | { kind: "package_examples"; packageName: string }
  | { kind: "package_downloads"; packageName: string }
  | { kind: "package_relations"; packageName: string }
  | { kind: "recent_packages" }
  | { kind: "popular_packages" }
  | { kind: "categories" }
  | { kind: "owner_packages"; ownerGithubLogin: string }
  | { kind: "stats_summary" }
  | { kind: "stats_dashboard" };

function parseYankRoute(path: string): ParsedYankRoute | null {
  const match = path.match(
    /^(?:api\/)?v1\/me\/packages\/([^/]+)\/versions\/([^/]+)\/yank$/,
  );
  if (match === null) {
    return null;
  }

  return {
    packageName: decodeURIComponent(match[1] ?? ""),
    version: decodeURIComponent(match[2] ?? ""),
  };
}

function trimSlashes(value: string): string {
  return value.replace(/^\/+|\/+$/g, "");
}

function matchesPath(path: string, ...candidates: string[]): boolean {
  return candidates.includes(path);
}

function parseViewRoute(path: string): ParsedViewRoute | null {
  const normalizedPath = path.startsWith("api/v1/views/")
    ? path.slice("api/v1/views/".length)
    : path.startsWith("v1/views/")
      ? path.slice("v1/views/".length)
      : null;

  if (normalizedPath === null) {
    return null;
  }

  if (normalizedPath === "recent/packages") {
    return { kind: "recent_packages" };
  }

  if (normalizedPath === "popular/packages") {
    return { kind: "popular_packages" };
  }

  if (normalizedPath === "categories") {
    return { kind: "categories" };
  }

  if (normalizedPath === "stats/summary") {
    return { kind: "stats_summary" };
  }

  if (normalizedPath === "stats/dashboard") {
    return { kind: "stats_dashboard" };
  }

  const packageOverviewMatch = normalizedPath.match(/^packages\/([^/]+)\/overview$/);
  if (packageOverviewMatch !== null) {
    return {
      kind: "package_overview",
      packageName: decodeURIComponent(packageOverviewMatch[1] ?? ""),
    };
  }

  const packageReadmeMatch = normalizedPath.match(/^packages\/([^/]+)\/readme$/);
  if (packageReadmeMatch !== null) {
    return {
      kind: "package_readme",
      packageName: decodeURIComponent(packageReadmeMatch[1] ?? ""),
    };
  }

  const packageExamplesMatch = normalizedPath.match(/^packages\/([^/]+)\/examples$/);
  if (packageExamplesMatch !== null) {
    return {
      kind: "package_examples",
      packageName: decodeURIComponent(packageExamplesMatch[1] ?? ""),
    };
  }

  const packageDownloadsMatch = normalizedPath.match(/^packages\/([^/]+)\/downloads$/);
  if (packageDownloadsMatch !== null) {
    return {
      kind: "package_downloads",
      packageName: decodeURIComponent(packageDownloadsMatch[1] ?? ""),
    };
  }

  const packageRelationsMatch = normalizedPath.match(/^packages\/([^/]+)\/relations$/);
  if (packageRelationsMatch !== null) {
    return {
      kind: "package_relations",
      packageName: decodeURIComponent(packageRelationsMatch[1] ?? ""),
    };
  }

  const ownerPackagesMatch = normalizedPath.match(/^owners\/([^/]+)\/packages$/);
  if (ownerPackagesMatch !== null) {
    return {
      kind: "owner_packages",
      ownerGithubLogin: decodeURIComponent(ownerPackagesMatch[1] ?? ""),
    };
  }

  throw new HttpError(404, "not_found", "View route does not exist.");
}

async function readPackageReadmeDocument(
  env: Env,
  packageName: string,
  version?: string,
): Promise<{
  schema_version: 1;
  package_name: string;
  package_version: string;
  source_key: string;
  readme_path: string;
  readme_markdown: string;
} | null> {
  const resolvedRelease = await resolvePackageArtifactRelease(env, packageName, version);
  if (resolvedRelease === null) {
    return null;
  }

  const object = await env.ML_PKGS_CDN.get(resolvedRelease.source_key);
  if (object === null) {
    return null;
  }

  const archiveBytes = new Uint8Array(await object.arrayBuffer());
  const readme = await readFirstArchiveFileFromTarGz(archiveBytes, [
    "README.md",
    "README.markdown",
    "README.mdown",
    "README.txt",
    "README",
    "readme.md",
    "readme.markdown",
    "readme.mdown",
    "readme.txt",
    "readme",
  ]);

  if (readme === null) {
    return null;
  }

  return {
    schema_version: 1,
    package_name: packageName,
    package_version: resolvedRelease.package_version,
    source_key: resolvedRelease.source_key,
    readme_path: readme.path,
    readme_markdown: readme.contents,
  };
}

async function readPackageExamplesDocument(
  env: Env,
  packageName: string,
  version?: string,
): Promise<{
  schema_version: 1;
  package_name: string;
  package_version: string;
  source_key: string;
  examples: Array<{
    name: string;
    path: string;
    source_code: string;
  }>;
} | null> {
  const resolvedRelease = await resolvePackageArtifactRelease(env, packageName, version);
  if (resolvedRelease === null) {
    return null;
  }

  const object = await env.ML_PKGS_CDN.get(resolvedRelease.source_key);
  if (object === null) {
    return null;
  }

  const archiveBytes = new Uint8Array(await object.arrayBuffer());
  const examplePaths = (await listArchiveFilesFromTarGz(archiveBytes, { prefix: "examples" }))
    .filter((path) => isPublishedExamplePath(path))
    .sort((left, right) => left.localeCompare(right));
  const exampleFiles = await readArchiveFilesFromTarGz(archiveBytes, examplePaths);
  const exampleFileByPath = new Map(exampleFiles.map((example) => [example.path, example.contents]));

  return {
    schema_version: 1,
    package_name: packageName,
    package_version: resolvedRelease.package_version,
    source_key: resolvedRelease.source_key,
    examples: examplePaths.flatMap((path) => {
      const sourceCode = exampleFileByPath.get(path);
      if (sourceCode === undefined) {
        return [];
      }

      return [{
        name: exampleNameFromPath(path),
        path,
        source_code: sourceCode,
      }];
    }),
  };
}

async function resolvePackageArtifactRelease(
  env: Env,
  packageName: string,
  version?: string,
): Promise<{ package_version: string; source_key: string } | null> {
  if (version === undefined) {
    const overview = await readPackageOverviewDocument(env.SEARCH_DB, packageName);
    if (overview === null) {
      return null;
    }

    return {
      package_version: overview.latest_version,
      source_key: overview.source_key,
    };
  }

  const release = await readPublishedRelease(env.SEARCH_DB, packageName, version);
  if (release === null) {
    return null;
  }

  return {
    package_version: release.package_version,
    source_key: release.source_archive_key,
  };
}

function isPublishedExamplePath(path: string): boolean {
  const normalized = trimSlashes(path);
  if (!normalized.startsWith("examples/") || !normalized.endsWith(".ml")) {
    return false;
  }

  return normalized
    .split("/")
    .every((segment) => segment.length > 0 && !segment.startsWith("."));
}

function exampleNameFromPath(path: string): string {
  const normalized = trimSlashes(path);
  const fileName = normalized.split("/").pop() ?? normalized;
  return fileName.endsWith(".ml") ? fileName.slice(0, -".ml".length) : fileName;
}

function resolveIndexStorageKey(path: string, config: ReturnType<typeof getConfig>): string | null {
  const normalizedPath = trimSlashes(path);
  const routePrefixes = [config.indexRoutePath, `api/${config.indexRoutePath}`];

  for (const prefix of routePrefixes) {
    if (normalizedPath === `${prefix}/config.json`) {
      return indexConfigKey(config);
    }

    if (normalizedPath.startsWith(`${prefix}/`)) {
      const suffix = normalizedPath.slice(prefix.length + 1);
      if (suffix.length === 0 || !suffix.endsWith(".json")) {
        return null;
      }

      return `${config.indexBasePath}/${suffix}`;
    }
  }

  return null;
}

function resolveArtifactStorageKey(path: string): string | null {
  const normalizedPath = trimSlashes(path);
  const prefixes = ["v1/artifacts", "api/v1/artifacts"];

  for (const prefix of prefixes) {
    if (normalizedPath.startsWith(`${prefix}/`)) {
      const storageKey = normalizedPath.slice(prefix.length + 1);
      return storageKey.length === 0 ? null : storageKey;
    }
  }

  return null;
}

function resolveRiotReleaseMetadataStorageKey(path: string): string | null {
  const normalizedPath = trimSlashes(path);
  const prefixes = ["v1/riot", "api/v1/riot"];

  for (const prefix of prefixes) {
    if (normalizedPath === `${prefix}/latest.json`) {
      return riotLatestMetadataKey();
    }

    const match = normalizedPath.match(new RegExp(`^${escapeRegExp(prefix)}/(riot-[^/]+\\.json)$`));
    if (match !== null) {
      return `riot/${match[1]}`;
    }
  }

  return null;
}

function escapeRegExp(value: string): string {
  return value.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
}

function sanitizePackageIndexDocument(document: PackageIndexDocument): PackageIndexDocument | null {
  const releases = document.releases.filter((release) => isServableIndexedRelease(document.name, release));
  if (releases.length === 0) {
    return null;
  }

  const updatedAt = releases
    .map((release) => Date.parse(release.published_at))
    .filter((value) => Number.isFinite(value))
    .sort((left, right) => right - left)[0];

  return {
    ...document,
    latest: releases.find((release) => release.yanked !== true)?.version ?? releases[0]?.version ?? document.latest,
    updated_at: updatedAt === undefined ? document.updated_at : new Date(updatedAt).toISOString(),
    releases,
  };
}

function isServableIndexedRelease(packageName: string, release: IndexedPackageRelease): boolean {
  if (
    typeof release.version !== "string" ||
    typeof release.artifact_sha256 !== "string" ||
    release.version.length === 0 ||
    release.artifact_sha256.length === 0
  ) {
    return false;
  }

  return (
    release.manifest_key === artifactManifestKey(packageName, release.version, release.artifact_sha256) &&
    release.source_key === artifactSourceArchiveKey(packageName, release.version, release.artifact_sha256)
  );
}

async function respondWithIndexJson(request: Request, body: unknown): Promise<Response> {
  const payload = JSON.stringify(body, null, 2);
  const etag = `"${await sha256Hex(payload)}"`;

  if (request.headers.get("if-none-match") === etag) {
    return new Response(null, {
      status: 304,
      headers: {
        etag,
        "cache-control": "no-store",
      },
    });
  }

  const headers = new Headers({
    "content-type": "application/json; charset=utf-8",
    "cache-control": "no-store",
    etag,
  });

  if (request.method === "HEAD") {
    return new Response(null, {
      status: 200,
      headers,
    });
  }

  return new Response(payload, {
    status: 200,
    headers,
  });
}

async function sha256Hex(value: string): Promise<string> {
  const bytes = new TextEncoder().encode(value);
  const digest = await crypto.subtle.digest("SHA-256", bytes);
  return [...new Uint8Array(digest)].map((byte) => byte.toString(16).padStart(2, "0")).join("");
}

function parsePackageEventsRoute(path: string): ParsedPackageEventsRoute | null {
  const normalizedPath = trimSlashes(path);
  const packageEventsMatch = normalizedPath.match(/^(?:api\/)?v1\/packages\/([^/]+)\/events$/);
  if (packageEventsMatch === null) {
    return null;
  }

  const packageName = packageEventsMatch[1];
  if (packageName === undefined) {
    return null;
  }

  return {
    packageName: decodeURIComponent(packageName),
  };
}

function clampInteger(
  value: string | null,
  fallback: number,
  minimum: number,
  maximum: number,
): number {
  if (value === null) {
    return fallback;
  }

  const parsed = Number.parseInt(value, 10);
  if (!Number.isFinite(parsed)) {
    return fallback;
  }

  return Math.min(maximum, Math.max(minimum, parsed));
}

async function readBodyObject(request: Request): Promise<Record<string, unknown>> {
  const contentType = request.headers.get("content-type") ?? "";
  if (contentType.includes("application/json")) {
    return (await request.json()) as Record<string, unknown>;
  }

  if (
    contentType.includes("application/x-www-form-urlencoded") ||
    contentType.includes("multipart/form-data")
  ) {
    const form = await request.formData();
    const entries = [...form.entries()].map(([key, value]) => [key, String(value)]);
    return Object.fromEntries(entries);
  }

  return {};
}

async function readReturnToFromRequest(request: Request): Promise<string | null> {
  const body = await readBodyObject(request);
  return typeof body.return_to === "string" ? body.return_to : null;
}

function serializeTokenRecord(record: ApiTokenRecord): Record<string, unknown> {
  return {
    token_id: record.token_id,
    user_id: record.user_id,
    github_login: record.github_login,
    name: record.name,
    capabilities: record.capabilities,
    created_at: record.created_at,
    last_used_at: record.last_used_at,
    revoked_at: record.revoked_at,
  };
}

function wantsJson(request: Request): boolean {
  return request.headers.get("accept")?.includes("application/json") ?? false;
}

function redirect(location: string, extraHeaders?: Record<string, string>): Response {
  const headers = new Headers(extraHeaders);
  headers.set("location", location);
  return new Response(null, {
    status: 302,
    headers,
  });
}

function normalizeError(error: unknown): HttpError {
  if (error instanceof HttpError) {
    return error;
  }

  if (error instanceof Error) {
    return new HttpError(500, "internal_error", error.message);
  }

  return new HttpError(500, "internal_error", "Unknown error.");
}
