import { and, desc, eq, isNotNull, lte, or, sql } from "drizzle-orm";

import { registryDb } from "./db.ts";
import { packagePipelineRuns, packageReleasesToProcess } from "./schema.ts";
import type {
  PackagePipelineRunKind,
  PackagePipelineRunRecord,
  PackagePipelineRunStatus,
  PackagePipelineRunnerKind,
  PackagePublishedEvent,
  PackageReleaseToProcessRecord,
  PackageReleaseToProcessStatus,
} from "./types.ts";

export async function writePackagePipelineRunRecord(
  db: D1Database,
  record: PackagePipelineRunRecord,
): Promise<void> {
  const database = registryDb(db);
  await database
    .insert(packagePipelineRuns)
    .values({
      runId: record.run_id,
      runKind: record.run_kind,
      packageName: record.package_name,
      packageVersion: record.package_version,
      artifactSha256: record.artifact_sha256,
      sourceArchiveKey: record.source_archive_key,
      runnerKind: record.runner_kind,
      status: record.status,
      outputPrefix: record.output_prefix,
      requestKey: record.request_key,
      createdAt: record.created_at,
      updatedAt: record.updated_at,
      startedAt: record.started_at ?? null,
      finishedAt: record.finished_at ?? null,
      statusMessage: record.status_message ?? null,
      metadataJson: JSON.stringify(record.metadata),
    })
    .onConflictDoUpdate({
      target: [
        packagePipelineRuns.packageName,
        packagePipelineRuns.packageVersion,
        packagePipelineRuns.artifactSha256,
        packagePipelineRuns.runKind,
      ],
      set: {
        sourceArchiveKey: record.source_archive_key,
        runnerKind: record.runner_kind,
        status: record.status,
        outputPrefix: record.output_prefix,
        requestKey: record.request_key,
        updatedAt: record.updated_at,
        startedAt: record.started_at ?? null,
        finishedAt: record.finished_at ?? null,
        statusMessage: record.status_message ?? null,
        metadataJson: JSON.stringify(record.metadata),
      },
    });
}

export async function enqueuePackageReleaseToProcess(
  db: D1Database,
  event: PackagePublishedEvent,
): Promise<void> {
  const now = new Date().toISOString();
  const releaseId = buildPackageReleaseToProcessId(event);
  const database = registryDb(db);

  await database
    .insert(packageReleasesToProcess)
    .values({
      releaseId,
      packageName: event.package_name,
      packageVersion: event.package_version,
      artifactSha256: event.artifact_sha256,
      sourceArchiveKey: event.source_archive_key,
      status: "pending",
      attemptCount: 0,
      nextAttemptAt: now,
      createdAt: now,
      updatedAt: now,
      lastAttemptedAt: null,
      leaseExpiresAt: null,
      finishedAt: null,
      statusMessage: "Queued for package post-publish processing.",
      payloadJson: JSON.stringify(event),
    })
    .onConflictDoUpdate({
      target: packageReleasesToProcess.releaseId,
      set: {
        sourceArchiveKey: event.source_archive_key,
        updatedAt: now,
        nextAttemptAt: sql`
          CASE
            WHEN ${packageReleasesToProcess.status} IN ('finished', 'blocked')
            THEN ${packageReleasesToProcess.nextAttemptAt}
            ELSE ${now}
          END
        `,
        leaseExpiresAt: sql`
          CASE
            WHEN ${packageReleasesToProcess.status} IN ('finished', 'blocked')
            THEN ${packageReleasesToProcess.leaseExpiresAt}
            ELSE NULL
          END
        `,
        finishedAt: sql`
          CASE
            WHEN ${packageReleasesToProcess.status} IN ('finished', 'blocked')
            THEN ${packageReleasesToProcess.finishedAt}
            ELSE NULL
          END
        `,
        status: sql`
          CASE
            WHEN ${packageReleasesToProcess.status} IN ('finished', 'blocked')
            THEN ${packageReleasesToProcess.status}
            ELSE 'pending'
          END
        `,
        statusMessage: sql`
          CASE
            WHEN ${packageReleasesToProcess.status} IN ('finished', 'blocked')
            THEN ${packageReleasesToProcess.statusMessage}
            ELSE 'Queued for package post-publish processing.'
          END
        `,
        payloadJson: JSON.stringify(event),
      },
    });
}

export async function listDuePackageReleasesToProcess(
  db: D1Database,
  now: string,
  limit: number,
): Promise<PackageReleaseToProcessRecord[]> {
  const database = registryDb(db);
  const rows = await database
    .select({
      release_id: packageReleasesToProcess.releaseId,
      package_name: packageReleasesToProcess.packageName,
      package_version: packageReleasesToProcess.packageVersion,
      artifact_sha256: packageReleasesToProcess.artifactSha256,
      source_archive_key: packageReleasesToProcess.sourceArchiveKey,
      status: packageReleasesToProcess.status,
      attempt_count: packageReleasesToProcess.attemptCount,
      next_attempt_at: packageReleasesToProcess.nextAttemptAt,
      created_at: packageReleasesToProcess.createdAt,
      updated_at: packageReleasesToProcess.updatedAt,
      last_attempted_at: packageReleasesToProcess.lastAttemptedAt,
      lease_expires_at: packageReleasesToProcess.leaseExpiresAt,
      finished_at: packageReleasesToProcess.finishedAt,
      status_message: packageReleasesToProcess.statusMessage,
      payload_json: packageReleasesToProcess.payloadJson,
    })
    .from(packageReleasesToProcess)
    .where(
      or(
        and(
          eq(packageReleasesToProcess.status, "pending"),
          lte(packageReleasesToProcess.nextAttemptAt, now),
        ),
        and(
          eq(packageReleasesToProcess.status, "processing"),
          isNotNull(packageReleasesToProcess.leaseExpiresAt),
          lte(packageReleasesToProcess.leaseExpiresAt, now),
        ),
      ),
    )
    .orderBy(packageReleasesToProcess.nextAttemptAt, packageReleasesToProcess.createdAt)
    .limit(limit);

  return rows.map(parsePackageReleaseToProcessRow);
}

export async function countActivePackageReleasesToProcess(
  db: D1Database,
  now: string,
): Promise<number> {
  const database = registryDb(db);
  const [row] = await database
    .select({
      count: sql<number>`count(*)`,
    })
    .from(packageReleasesToProcess)
    .where(
      and(
        eq(packageReleasesToProcess.status, "processing"),
        isNotNull(packageReleasesToProcess.leaseExpiresAt),
        sql`${packageReleasesToProcess.leaseExpiresAt} > ${now}`,
      ),
    );

  return Number(row?.count ?? 0);
}

export async function claimPackageReleaseToProcess(
  db: D1Database,
  releaseId: string,
  now: string,
  leaseExpiresAt: string,
): Promise<PackageReleaseToProcessRecord | null> {
  const database = registryDb(db);
  const [row] = await database
    .update(packageReleasesToProcess)
    .set({
      status: "processing",
      attemptCount: sql`${packageReleasesToProcess.attemptCount} + 1`,
      updatedAt: now,
      lastAttemptedAt: now,
      leaseExpiresAt,
      statusMessage: "Claimed by timer worker for package post-publish processing.",
    })
    .where(
      and(
        eq(packageReleasesToProcess.releaseId, releaseId),
        or(
          and(
            eq(packageReleasesToProcess.status, "pending"),
            lte(packageReleasesToProcess.nextAttemptAt, now),
          ),
          and(
            eq(packageReleasesToProcess.status, "processing"),
            isNotNull(packageReleasesToProcess.leaseExpiresAt),
            lte(packageReleasesToProcess.leaseExpiresAt, now),
          ),
        ),
      ),
    )
    .returning({
      release_id: packageReleasesToProcess.releaseId,
      package_name: packageReleasesToProcess.packageName,
      package_version: packageReleasesToProcess.packageVersion,
      artifact_sha256: packageReleasesToProcess.artifactSha256,
      source_archive_key: packageReleasesToProcess.sourceArchiveKey,
      status: packageReleasesToProcess.status,
      attempt_count: packageReleasesToProcess.attemptCount,
      next_attempt_at: packageReleasesToProcess.nextAttemptAt,
      created_at: packageReleasesToProcess.createdAt,
      updated_at: packageReleasesToProcess.updatedAt,
      last_attempted_at: packageReleasesToProcess.lastAttemptedAt,
      lease_expires_at: packageReleasesToProcess.leaseExpiresAt,
      finished_at: packageReleasesToProcess.finishedAt,
      status_message: packageReleasesToProcess.statusMessage,
      payload_json: packageReleasesToProcess.payloadJson,
    });

  return row ? parsePackageReleaseToProcessRow(row) : null;
}

export async function markPackageReleaseToProcessFinished(
  db: D1Database,
  releaseId: string,
  now: string,
  statusMessage: string,
): Promise<void> {
  const database = registryDb(db);
  await database
    .update(packageReleasesToProcess)
    .set({
      status: "finished",
      updatedAt: now,
      finishedAt: now,
      leaseExpiresAt: null,
      statusMessage,
    })
    .where(eq(packageReleasesToProcess.releaseId, releaseId));
}

export async function reschedulePackageReleaseToProcess(
  db: D1Database,
  releaseId: string,
  now: string,
  nextAttemptAt: string,
  statusMessage: string,
): Promise<void> {
  const database = registryDb(db);
  await database
    .update(packageReleasesToProcess)
    .set({
      status: "pending",
      updatedAt: now,
      nextAttemptAt,
      leaseExpiresAt: null,
      statusMessage,
    })
    .where(eq(packageReleasesToProcess.releaseId, releaseId));
}

export async function markPackageReleaseToProcessBlocked(
  db: D1Database,
  releaseId: string,
  now: string,
  statusMessage: string,
): Promise<void> {
  const database = registryDb(db);
  await database
    .update(packageReleasesToProcess)
    .set({
      status: "blocked",
      updatedAt: now,
      finishedAt: now,
      leaseExpiresAt: null,
      statusMessage,
    })
    .where(eq(packageReleasesToProcess.releaseId, releaseId));
}

export async function readLatestPackageReleaseToProcess(
  db: D1Database,
  packageName: string,
  packageVersion: string,
): Promise<PackageReleaseToProcessRecord | null> {
  const database = registryDb(db);
  const [row] = await database
    .select({
      release_id: packageReleasesToProcess.releaseId,
      package_name: packageReleasesToProcess.packageName,
      package_version: packageReleasesToProcess.packageVersion,
      artifact_sha256: packageReleasesToProcess.artifactSha256,
      source_archive_key: packageReleasesToProcess.sourceArchiveKey,
      status: packageReleasesToProcess.status,
      attempt_count: packageReleasesToProcess.attemptCount,
      next_attempt_at: packageReleasesToProcess.nextAttemptAt,
      created_at: packageReleasesToProcess.createdAt,
      updated_at: packageReleasesToProcess.updatedAt,
      last_attempted_at: packageReleasesToProcess.lastAttemptedAt,
      lease_expires_at: packageReleasesToProcess.leaseExpiresAt,
      finished_at: packageReleasesToProcess.finishedAt,
      status_message: packageReleasesToProcess.statusMessage,
      payload_json: packageReleasesToProcess.payloadJson,
    })
    .from(packageReleasesToProcess)
    .where(
      and(
        eq(packageReleasesToProcess.packageName, packageName),
        eq(packageReleasesToProcess.packageVersion, packageVersion),
      ),
    )
    .orderBy(desc(packageReleasesToProcess.createdAt))
    .limit(1);

  return row ? parsePackageReleaseToProcessRow(row) : null;
}

export async function readPackageReleaseToProcessById(
  db: D1Database,
  releaseId: string,
): Promise<PackageReleaseToProcessRecord | null> {
  const database = registryDb(db);
  const [row] = await database
    .select({
      release_id: packageReleasesToProcess.releaseId,
      package_name: packageReleasesToProcess.packageName,
      package_version: packageReleasesToProcess.packageVersion,
      artifact_sha256: packageReleasesToProcess.artifactSha256,
      source_archive_key: packageReleasesToProcess.sourceArchiveKey,
      status: packageReleasesToProcess.status,
      attempt_count: packageReleasesToProcess.attemptCount,
      next_attempt_at: packageReleasesToProcess.nextAttemptAt,
      created_at: packageReleasesToProcess.createdAt,
      updated_at: packageReleasesToProcess.updatedAt,
      last_attempted_at: packageReleasesToProcess.lastAttemptedAt,
      lease_expires_at: packageReleasesToProcess.leaseExpiresAt,
      finished_at: packageReleasesToProcess.finishedAt,
      status_message: packageReleasesToProcess.statusMessage,
      payload_json: packageReleasesToProcess.payloadJson,
    })
    .from(packageReleasesToProcess)
    .where(eq(packageReleasesToProcess.releaseId, releaseId))
    .limit(1);

  return row ? parsePackageReleaseToProcessRow(row) : null;
}

export async function readLatestPackagePipelineRun(
  db: D1Database,
  packageName: string,
  packageVersion: string,
  runKind: PackagePipelineRunKind,
): Promise<PackagePipelineRunRecord | null> {
  const database = registryDb(db);
  const [row] = await database
    .select({
      run_id: packagePipelineRuns.runId,
      run_kind: packagePipelineRuns.runKind,
      package_name: packagePipelineRuns.packageName,
      package_version: packagePipelineRuns.packageVersion,
      artifact_sha256: packagePipelineRuns.artifactSha256,
      source_archive_key: packagePipelineRuns.sourceArchiveKey,
      runner_kind: packagePipelineRuns.runnerKind,
      status: packagePipelineRuns.status,
      output_prefix: packagePipelineRuns.outputPrefix,
      request_key: packagePipelineRuns.requestKey,
      created_at: packagePipelineRuns.createdAt,
      updated_at: packagePipelineRuns.updatedAt,
      started_at: packagePipelineRuns.startedAt,
      finished_at: packagePipelineRuns.finishedAt,
      status_message: packagePipelineRuns.statusMessage,
      metadata_json: packagePipelineRuns.metadataJson,
    })
    .from(packagePipelineRuns)
    .where(
      and(
        eq(packagePipelineRuns.packageName, packageName),
        eq(packagePipelineRuns.packageVersion, packageVersion),
        eq(packagePipelineRuns.runKind, runKind),
      ),
    )
    .orderBy(desc(packagePipelineRuns.createdAt))
    .limit(1);

  if (!row) {
    return null;
  }

  return {
    run_id: row.run_id,
    run_kind: row.run_kind as PackagePipelineRunKind,
    package_name: row.package_name,
    package_version: row.package_version,
    artifact_sha256: row.artifact_sha256,
    source_archive_key: row.source_archive_key,
    runner_kind: row.runner_kind as PackagePipelineRunnerKind,
    status: row.status as PackagePipelineRunStatus,
    output_prefix: row.output_prefix,
    request_key: row.request_key,
    created_at: row.created_at,
    updated_at: row.updated_at,
    started_at: row.started_at ?? undefined,
    finished_at: row.finished_at ?? undefined,
    status_message: row.status_message ?? undefined,
    metadata: JSON.parse(row.metadata_json) as Record<string, unknown>,
  };
}

function buildPackageReleaseToProcessId(event: PackagePublishedEvent): string {
  return `release:${event.package_name}:${event.package_version}:${event.artifact_sha256}`;
}

function parsePackageReleaseToProcessRow(row: {
  release_id: string;
  package_name: string;
  package_version: string;
  artifact_sha256: string;
  source_archive_key: string;
  status: string;
  attempt_count: number;
  next_attempt_at: string;
  created_at: string;
  updated_at: string;
  last_attempted_at: string | null;
  lease_expires_at: string | null;
  finished_at: string | null;
  status_message: string | null;
  payload_json: string;
}): PackageReleaseToProcessRecord {
  return {
    release_id: row.release_id,
    package_name: row.package_name,
    package_version: row.package_version,
    artifact_sha256: row.artifact_sha256,
    source_archive_key: row.source_archive_key,
    status: row.status as PackageReleaseToProcessStatus,
    attempt_count: row.attempt_count,
    next_attempt_at: row.next_attempt_at,
    created_at: row.created_at,
    updated_at: row.updated_at,
    last_attempted_at: row.last_attempted_at ?? undefined,
    lease_expires_at: row.lease_expires_at ?? undefined,
    finished_at: row.finished_at ?? undefined,
    status_message: row.status_message ?? undefined,
    payload: JSON.parse(row.payload_json) as PackagePublishedEvent,
  };
}
