import {
  writeBinaryDownloadRecord,
  writeIndexReadRecord,
  writePackageDownloadRecord,
} from "../../api.pkgs.ml/src/access-db.ts";
import {
  artifactManifestKey,
  artifactSourceArchiveKey,
} from "../../api.pkgs.ml/src/storage.ts";
import type {
  IndexedPackageRelease,
  PackageIndexDocument,
} from "../../api.pkgs.ml/src/types.ts";

export interface Env {
  ML_PKGS_CDN: R2Bucket;
  SEARCH_DB: D1Database;
}

const JSON_CONTENT_TYPE = "application/json; charset=utf-8";
const RIOT_AGENT_HEADER = "x-riot-agent";

export default {
  async fetch(request: Request, env: Env, ctx: ExecutionContext): Promise<Response> {
    try {
      return await handleRequest(request, env, ctx);
    } catch {
      return new Response("Not Found", {
        status: 404,
        headers: {
          "content-type": "text/plain; charset=utf-8",
        },
      });
    }
  },
};

async function handleRequest(
  request: Request,
  env: Env,
  ctx: ExecutionContext,
): Promise<Response> {
  const url = new URL(request.url);
  const key = trimLeadingSlash(url.pathname);

  if (request.method !== "GET" && request.method !== "HEAD") {
    return new Response("Method Not Allowed", {
      status: 405,
      headers: {
        allow: "GET, HEAD",
        "content-type": "text/plain; charset=utf-8",
      },
    });
  }

  if (key.length === 0) {
    return Response.json({
      service: "riot-cdn",
      routes: {
        root_proxy: "/<bucket-key>",
      },
    });
  }

  if (isPrivateObjectKey(key)) {
    throw new Error("private_object_not_found");
  }

  const response = isIndexDocumentKey(key)
    ? await handleIndexDocument(request, env, key)
    : await handleObject(request, env, key);

  if (request.method === "GET" && response.status === 200) {
    const riotAgent = extractRiotAgent(request);
    const access = riotAgent === undefined ? null : classifyAccess(key, riotAgent);
    if (access !== null) {
      ctx.waitUntil(recordAccess(env, access));
    }
  }

  return response;
}

async function handleIndexDocument(
  request: Request,
  env: Env,
  key: string,
): Promise<Response> {
  if (key === "index/v1/config.json") {
    return await respondWithJsonDocument(request, buildPublicIndexConfigDocument(request), "no-store");
  }

  const object = await env.ML_PKGS_CDN.get(key);
  if (object === null) {
    throw new Error("index_not_found");
  }

  const document = sanitizePackageIndexDocument(await object.json<PackageIndexDocument>());
  if (document === null) {
    throw new Error("index_not_found");
  }

  return await respondWithJsonDocument(request, document, "no-store");
}

async function handleObject(
  request: Request,
  env: Env,
  key: string,
): Promise<Response> {
  const object = await env.ML_PKGS_CDN.get(key);
  if (object === null) {
    throw new Error("object_not_found");
  }

  return await respondWithStoredObject(request, object, cacheControlForKey(key));
}

async function respondWithStoredObject(
  request: Request,
  object: R2ObjectBody,
  cacheControl: string,
): Promise<Response> {
  const etag = object.httpEtag;
  if (request.headers.get("if-none-match") === etag) {
    return new Response(null, {
      status: 304,
      headers: {
        etag,
        "cache-control": cacheControl,
      },
    });
  }

  const headers = new Headers();
  object.writeHttpMetadata(headers);
  headers.set("cache-control", cacheControl);
  headers.set("etag", etag);
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

async function respondWithJsonDocument(
  request: Request,
  body: unknown,
  cacheControl: string,
): Promise<Response> {
  const payload = JSON.stringify(body, null, 2);
  const etag = `"${await sha256Hex(payload)}"`;

  if (request.headers.get("if-none-match") === etag) {
    return new Response(null, {
      status: 304,
      headers: {
        etag,
        "cache-control": cacheControl,
      },
    });
  }

  const headers = new Headers({
    "content-type": JSON_CONTENT_TYPE,
    "cache-control": cacheControl,
    etag,
    "content-length": String(new TextEncoder().encode(payload).byteLength),
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

type AccessEvent =
  | { kind: "index_read"; documentKey: string; packageName?: string; riotAgent: string }
  | {
      kind: "package_download";
      packageName: string;
      packageVersion: string;
      artifactSha256: string;
      sourceArchiveKey: string;
      riotAgent: string;
    }
  | {
      kind: "binary_download";
      binaryName: "riot" | "ocaml";
      objectKey: string;
      riotAgent: string;
    };

function classifyAccess(key: string, riotAgent: string): AccessEvent | null {
  if (key === "index/v1/config.json") {
    return {
      kind: "index_read",
      documentKey: key,
      riotAgent,
    };
  }

  const indexMatch = key.match(/^index\/v1\/(?:.+\/)?([^/]+)\.json$/);
  if (indexMatch !== null) {
    return {
      kind: "index_read",
      documentKey: key,
      packageName: decodeURIComponent(indexMatch[1] ?? ""),
      riotAgent,
    };
  }

  const sourceMatch = key.match(/^sources\/([^/]+)\/([^/]+)\/([^/]+)\.tar\.gz$/);
  if (sourceMatch !== null) {
    return {
      kind: "package_download",
      packageName: decodeURIComponent(sourceMatch[1] ?? ""),
      packageVersion: decodeURIComponent(sourceMatch[2] ?? ""),
      artifactSha256: decodeURIComponent(sourceMatch[3] ?? ""),
      sourceArchiveKey: key,
      riotAgent,
    };
  }

  if (/^riot\/riot-[^/]+\.tar\.gz$/.test(key)) {
    return {
      kind: "binary_download",
      binaryName: "riot",
      objectKey: key,
      riotAgent,
    };
  }

  if (/^ocaml\/ocaml-[^/]+\.tar\.gz$/.test(key)) {
    return {
      kind: "binary_download",
      binaryName: "ocaml",
      objectKey: key,
      riotAgent,
    };
  }

  return null;
}

async function recordAccess(env: Env, access: AccessEvent): Promise<void> {
  const now = new Date().toISOString();

  switch (access.kind) {
    case "index_read":
      await writeIndexReadRecord(env.SEARCH_DB, {
        read_id: crypto.randomUUID(),
        document_key: access.documentKey,
        package_name: access.packageName,
        riot_agent: access.riotAgent,
        read_at: now,
      });
      return;
    case "package_download":
      await writePackageDownloadRecord(env.SEARCH_DB, {
        download_id: crypto.randomUUID(),
        package_name: access.packageName,
        package_version: access.packageVersion,
        artifact_sha256: access.artifactSha256,
        source_archive_key: access.sourceArchiveKey,
        riot_agent: access.riotAgent,
        downloaded_at: now,
      });
      return;
    case "binary_download":
      await writeBinaryDownloadRecord(env.SEARCH_DB, {
        download_id: crypto.randomUUID(),
        binary_name: access.binaryName,
        object_key: access.objectKey,
        riot_agent: access.riotAgent,
        downloaded_at: now,
      });
      return;
  }
}

function extractRiotAgent(request: Request): string | undefined {
  const value = request.headers.get(RIOT_AGENT_HEADER)?.trim();
  if (value === undefined || value.length === 0) {
    return undefined;
  }

  return value.slice(0, 128);
}

function isIndexDocumentKey(key: string): boolean {
  return key === "index/v1/config.json" || /^index\/v1\/.+\.json$/.test(key);
}

function isPrivateObjectKey(key: string): boolean {
  return key.startsWith("pipelines/") || key.includes("/_pipeline/");
}

function cacheControlForKey(key: string): string {
  if (key.startsWith("index/v1/")) {
    return "no-store";
  }

  if (key === "riot/install.sh") {
    return "public, max-age=300";
  }

  if (key === "riot/latest.json" || key.startsWith("riot/latest-")) {
    return "no-store";
  }

  return "public, max-age=31536000, immutable";
}

function trimLeadingSlash(value: string): string {
  return value.replace(/^\/+/, "");
}

function buildPublicIndexConfigDocument(request: Request): {
  schema_version: number;
  kind: "sparse";
  package_path_strategy: "cargo-lowercase-v1";
  index_base_url: string;
  artifact_base_url: string;
} {
  const origin = new URL(request.url).origin;

  return {
    schema_version: 1,
    kind: "sparse",
    package_path_strategy: "cargo-lowercase-v1",
    index_base_url: `${origin}/index/v1`,
    artifact_base_url: origin,
  };
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
    latest: releases[0]?.version ?? document.latest,
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

async function sha256Hex(value: string): Promise<string> {
  const bytes = new TextEncoder().encode(value);
  const digest = await crypto.subtle.digest("SHA-256", bytes);
  return [...new Uint8Array(digest)].map((byte) => byte.toString(16).padStart(2, "0")).join("");
}
