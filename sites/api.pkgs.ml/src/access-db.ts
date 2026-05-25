import { binaryDownloads, packageDownloads, indexReads } from "./schema.ts";
import { registryDb } from "./db.ts";
import type { BinaryDownloadRecord, IndexReadRecord, PackageDownloadRecord } from "./types.ts";

export async function writeIndexReadRecord(
  db: D1Database,
  record: IndexReadRecord,
): Promise<void> {
  const database = registryDb(db);
  await database.insert(indexReads).values({
    readId: record.read_id,
    documentKey: record.document_key,
    packageName: record.package_name ?? null,
    riotAgent: record.riot_agent,
    readAt: record.read_at,
  });
}

export async function writePackageDownloadRecord(
  db: D1Database,
  record: PackageDownloadRecord,
): Promise<void> {
  const database = registryDb(db);
  await database.insert(packageDownloads).values({
    downloadId: record.download_id,
    packageName: record.package_name,
    packageVersion: record.package_version,
    artifactSha256: record.artifact_sha256,
    sourceArchiveKey: record.source_archive_key,
    riotAgent: record.riot_agent,
    downloadedAt: record.downloaded_at,
  });
}

export async function writeBinaryDownloadRecord(
  db: D1Database,
  record: BinaryDownloadRecord,
): Promise<void> {
  const database = registryDb(db);
  await database.insert(binaryDownloads).values({
    downloadId: record.download_id,
    binaryName: record.binary_name,
    objectKey: record.object_key,
    riotAgent: record.riot_agent,
    downloadedAt: record.downloaded_at,
  });
}
