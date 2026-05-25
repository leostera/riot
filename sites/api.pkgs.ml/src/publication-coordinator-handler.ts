import { HttpError } from "./errors.ts";
import { publishPackageArtifact } from "./publication.ts";
import { json } from "./http.ts";
import type {
  AuthenticatedActor,
  Env,
  PublishedPackageRelease,
} from "./types.ts";

export async function handlePublicationCoordinatorRequest(
  request: Request,
  env: Env,
): Promise<Response> {
  const operation = request.headers.get("x-publication-operation");
  if (operation !== "publish-artifact") {
    throw new HttpError(
      404,
      "not_found",
      "Publication coordinator only accepts artifact publish operations.",
    );
  }

  const actorHeader = request.headers.get("x-publication-actor");
  if (actorHeader === null) {
    throw new HttpError(400, "invalid_actor", "Artifact publish requests must include an actor.");
  }

  const actor = parseAuthenticatedActor(JSON.parse(actorHeader));
  const archiveBytes = new Uint8Array(await request.arrayBuffer());
  if (archiveBytes.byteLength === 0) {
    throw new HttpError(400, "invalid_package_archive", "Artifact publish requires a non-empty tarball body.");
  }

  const publication = await publishPackageArtifact(env, archiveBytes, actor);
  return json(serializePublishedRelease(publication));
}

function parseAuthenticatedActor(value: unknown): AuthenticatedActor {
  if (value !== null && typeof value === "object") {
    const candidate = value as Record<string, unknown>;

    if (candidate.kind === "root") {
      return { kind: "root" };
    }

    if (
      candidate.kind === "user" &&
      typeof candidate.userId === "string" &&
      candidate.userId.length > 0 &&
      typeof candidate.githubLogin === "string" &&
      candidate.githubLogin.length > 0
    ) {
      return {
        kind: "user",
        userId: candidate.userId,
        githubLogin: candidate.githubLogin,
        tokenId: typeof candidate.tokenId === "string" && candidate.tokenId.length > 0
          ? candidate.tokenId
          : undefined,
      };
    }
  }

  throw new HttpError(400, "invalid_actor", "Coordinator requests must include a valid actor.");
}
function serializePublishedRelease(publication: PublishedPackageRelease): Record<string, unknown> {
  return {
    artifact_sha256: publication.artifactSha256,
    source_key: publication.sourceKey,
    manifest_key: publication.manifestKey,
    source_created: publication.sourceCreated,
    manifest_created: publication.manifestCreated,
    package_name: publication.packageName,
    package_version: publication.packageVersion,
    claim_key: publication.claimKey,
    release_key: publication.releaseKey,
    claim_created: publication.claimCreated,
    release_created: publication.releaseCreated,
    index_changed: publication.indexChanged,
  };
}
