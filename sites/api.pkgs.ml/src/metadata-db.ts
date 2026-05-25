import semver from "semver";
import { and, asc, desc, eq, gt, gte, sql } from "drizzle-orm";

import type {
  ApiTokenCapability,
  ApiTokenLookupRecord,
  ApiTokenRecord,
  CategoriesIndexDocument,
  CategorySummary,
  RegistryStatsActivityPoint,
  RegistryStatsDashboardDocument,
  RegistryStatsMetricKey,
  RegistryStatsMetricSeries,
  RegistryStatsSummaryDocument,
  RegistryStatsWindowKey,
  RegistryStatsWindowOption,
  OAuthStateRecord,
  OwnerPackagesDocument,
  PackageDownloadsDocument,
  PackageClaimRecord,
  PackageRelationDependency,
  PackageRelationDependent,
  PackageOverviewDocument,
  WebPackageListItem,
  WebPackageReleaseListItem,
  PackagePublicationManifest,
  PackageRelationsDocument,
  PopularPackagesDocument,
  PublishedReleaseRecord,
  RecentPackagesDocument,
  RegistryEventRecord,
  SessionRecord,
  SessionHandoffRecord,
  UserLoginRecord,
  UserRecord,
} from "./types.ts";
import { registryDb } from "./db.ts";
import {
  apiTokenLookups,
  apiTokens,
  binaryDownloads,
  indexReads,
  packageDownloads,
  packages,
  oauthStates,
  packageClaims,
  publishedReleases,
  registryEvents,
  sessionHandoffs,
  sessions,
  userLogins,
  users,
} from "./schema.ts";

const INTERNAL_RIOT_AGENT_PREFIXES = [
  "riot-docs-pipeline@",
];

export async function applyMetadataMigrations(db: D1Database): Promise<void> {
  void db;
}

export async function readUserRecord(db: D1Database, userId: string): Promise<UserRecord | null> {
  const database = registryDb(db);
  const [row] = await database
    .select({
      user_id: users.userId,
      github_id: users.githubId,
      github_login: users.githubLogin,
      github_name: users.githubName,
      github_avatar_url: users.githubAvatarUrl,
      github_email: users.githubEmail,
      github_email_verified: users.githubEmailVerified,
      created_at: users.createdAt,
      updated_at: users.updatedAt,
    })
    .from(users)
    .where(eq(users.userId, userId))
    .limit(1);

  return row ? parseUserRecord(row) : null;
}

export async function writeUserRecord(db: D1Database, record: UserRecord): Promise<void> {
  const database = registryDb(db);
  await database
    .insert(users)
    .values({
      userId: record.user_id,
      githubId: record.github_id,
      githubLogin: record.github_login,
      githubLoginLower: record.github_login.toLowerCase(),
      githubName: record.github_name ?? null,
      githubAvatarUrl: record.github_avatar_url ?? null,
      githubEmail: record.github_email ?? null,
      githubEmailVerified: record.github_email_verified ?? false,
      createdAt: record.created_at,
      updatedAt: record.updated_at,
    })
    .onConflictDoUpdate({
      target: users.userId,
      set: {
        githubId: record.github_id,
        githubLogin: record.github_login,
        githubLoginLower: record.github_login.toLowerCase(),
        githubName: record.github_name ?? null,
        githubAvatarUrl: record.github_avatar_url ?? null,
        githubEmail: record.github_email ?? null,
        githubEmailVerified: record.github_email_verified ?? false,
        updatedAt: record.updated_at,
      },
    });
}

export async function readUserLoginRecord(
  db: D1Database,
  githubLogin: string,
): Promise<UserLoginRecord | null> {
  const database = registryDb(db);
  const [row] = await database
    .select({
      github_login: userLogins.githubLogin,
      user_id: userLogins.userId,
      updated_at: userLogins.updatedAt,
    })
    .from(userLogins)
    .where(eq(userLogins.githubLoginLower, githubLogin.toLowerCase()))
    .limit(1);

  return row ?? null;
}

export async function writeUserLoginRecord(
  db: D1Database,
  record: UserLoginRecord,
): Promise<void> {
  const database = registryDb(db);
  await database
    .insert(userLogins)
    .values({
      githubLoginLower: record.github_login.toLowerCase(),
      githubLogin: record.github_login,
      userId: record.user_id,
      updatedAt: record.updated_at,
    })
    .onConflictDoUpdate({
      target: userLogins.githubLoginLower,
      set: {
        githubLogin: record.github_login,
        userId: record.user_id,
        updatedAt: record.updated_at,
      },
    });
}

export async function readOAuthStateRecord(
  db: D1Database,
  stateId: string,
): Promise<OAuthStateRecord | null> {
  const database = registryDb(db);
  const [row] = await database
    .select({
      state_id: oauthStates.stateId,
      return_to: oauthStates.returnTo,
      created_at: oauthStates.createdAt,
    })
    .from(oauthStates)
    .where(eq(oauthStates.stateId, stateId))
    .limit(1);

  return row ?? null;
}

export async function writeOAuthStateRecord(
  db: D1Database,
  record: OAuthStateRecord,
): Promise<void> {
  const database = registryDb(db);
  await database
    .insert(oauthStates)
    .values({
      stateId: record.state_id,
      returnTo: record.return_to,
      createdAt: record.created_at,
    })
    .onConflictDoUpdate({
      target: oauthStates.stateId,
      set: {
        returnTo: record.return_to,
        createdAt: record.created_at,
      },
    });
}

export async function deleteOAuthStateRecord(db: D1Database, stateId: string): Promise<void> {
  const database = registryDb(db);
  await database.delete(oauthStates).where(eq(oauthStates.stateId, stateId));
}

export async function readSessionRecord(
  db: D1Database,
  sessionId: string,
): Promise<SessionRecord | null> {
  const database = registryDb(db);
  const [row] = await database
    .select({
      session_id: sessions.sessionId,
      user_id: sessions.userId,
      github_login: sessions.githubLogin,
      created_at: sessions.createdAt,
      expires_at: sessions.expiresAt,
    })
    .from(sessions)
    .where(eq(sessions.sessionId, sessionId))
    .limit(1);

  return row ?? null;
}

export async function writeSessionRecord(
  db: D1Database,
  record: SessionRecord,
): Promise<void> {
  const database = registryDb(db);
  await database
    .insert(sessions)
    .values({
      sessionId: record.session_id,
      userId: record.user_id,
      githubLogin: record.github_login,
      createdAt: record.created_at,
      expiresAt: record.expires_at,
    })
    .onConflictDoUpdate({
      target: sessions.sessionId,
      set: {
        userId: record.user_id,
        githubLogin: record.github_login,
        createdAt: record.created_at,
        expiresAt: record.expires_at,
      },
    });
}

export async function deleteSessionRecord(db: D1Database, sessionId: string): Promise<void> {
  const database = registryDb(db);
  await database.delete(sessions).where(eq(sessions.sessionId, sessionId));
}

export async function readSessionHandoffRecord(
  db: D1Database,
  handoffId: string,
): Promise<SessionHandoffRecord | null> {
  const database = registryDb(db);
  const [row] = await database
    .select({
      handoff_id: sessionHandoffs.handoffId,
      session_id: sessionHandoffs.sessionId,
      return_to: sessionHandoffs.returnTo,
      created_at: sessionHandoffs.createdAt,
      expires_at: sessionHandoffs.expiresAt,
    })
    .from(sessionHandoffs)
    .where(eq(sessionHandoffs.handoffId, handoffId))
    .limit(1);

  return row ?? null;
}

export async function writeSessionHandoffRecord(
  db: D1Database,
  record: SessionHandoffRecord,
): Promise<void> {
  const database = registryDb(db);
  await database
    .insert(sessionHandoffs)
    .values({
      handoffId: record.handoff_id,
      sessionId: record.session_id,
      returnTo: record.return_to,
      createdAt: record.created_at,
      expiresAt: record.expires_at,
    })
    .onConflictDoUpdate({
      target: sessionHandoffs.handoffId,
      set: {
        sessionId: record.session_id,
        returnTo: record.return_to,
        createdAt: record.created_at,
        expiresAt: record.expires_at,
      },
    });
}

export async function deleteSessionHandoffRecord(db: D1Database, handoffId: string): Promise<void> {
  const database = registryDb(db);
  await database.delete(sessionHandoffs).where(eq(sessionHandoffs.handoffId, handoffId));
}

export async function readApiTokenRecord(
  db: D1Database,
  userId: string,
  tokenId: string,
): Promise<ApiTokenRecord | null> {
  const database = registryDb(db);
  const [row] = await database
    .select({
      token_id: apiTokens.tokenId,
      user_id: apiTokens.userId,
      github_login: apiTokens.githubLogin,
      name: apiTokens.name,
      secret_hash: apiTokens.secretHash,
      capabilities_json: apiTokens.capabilitiesJson,
      created_at: apiTokens.createdAt,
      last_used_at: apiTokens.lastUsedAt,
      revoked_at: apiTokens.revokedAt,
    })
    .from(apiTokens)
    .where(and(eq(apiTokens.userId, userId), eq(apiTokens.tokenId, tokenId)))
    .limit(1);

  return row ? parseApiTokenRecord(row) : null;
}

export async function writeApiTokenRecord(
  db: D1Database,
  record: ApiTokenRecord,
): Promise<void> {
  const database = registryDb(db);
  await database
    .insert(apiTokens)
    .values({
      tokenId: record.token_id,
      userId: record.user_id,
      githubLogin: record.github_login,
      name: record.name,
      secretHash: record.secret_hash,
      capabilitiesJson: JSON.stringify(record.capabilities),
      createdAt: record.created_at,
      lastUsedAt: record.last_used_at ?? null,
      revokedAt: record.revoked_at ?? null,
    })
    .onConflictDoUpdate({
      target: apiTokens.tokenId,
      set: {
        userId: record.user_id,
        githubLogin: record.github_login,
        name: record.name,
        secretHash: record.secret_hash,
        capabilitiesJson: JSON.stringify(record.capabilities),
        createdAt: record.created_at,
        lastUsedAt: record.last_used_at ?? null,
        revokedAt: record.revoked_at ?? null,
      },
    });
}

export async function listApiTokenRecords(
  db: D1Database,
  userId: string,
): Promise<ApiTokenRecord[]> {
  const database = registryDb(db);
  const rows = await database
    .select({
      token_id: apiTokens.tokenId,
      user_id: apiTokens.userId,
      github_login: apiTokens.githubLogin,
      name: apiTokens.name,
      secret_hash: apiTokens.secretHash,
      capabilities_json: apiTokens.capabilitiesJson,
      created_at: apiTokens.createdAt,
      last_used_at: apiTokens.lastUsedAt,
      revoked_at: apiTokens.revokedAt,
    })
    .from(apiTokens)
    .where(eq(apiTokens.userId, userId))
    .orderBy(desc(apiTokens.createdAt));

  return rows.map(parseApiTokenRecord);
}

export async function readApiTokenLookupRecord(
  db: D1Database,
  tokenHash: string,
): Promise<ApiTokenLookupRecord | null> {
  const database = registryDb(db);
  const [row] = await database
    .select({
      token_id: apiTokenLookups.tokenId,
      user_id: apiTokenLookups.userId,
      github_login: apiTokenLookups.githubLogin,
      capabilities_json: apiTokenLookups.capabilitiesJson,
      revoked_at: apiTokenLookups.revokedAt,
    })
    .from(apiTokenLookups)
    .where(eq(apiTokenLookups.secretHash, tokenHash))
    .limit(1);

  return row ? parseApiTokenLookupRecord(row) : null;
}

export async function writeApiTokenLookupRecord(
  db: D1Database,
  tokenHash: string,
  record: ApiTokenLookupRecord,
): Promise<void> {
  const database = registryDb(db);
  await database
    .insert(apiTokenLookups)
    .values({
      secretHash: tokenHash,
      tokenId: record.token_id,
      userId: record.user_id,
      githubLogin: record.github_login,
      capabilitiesJson: JSON.stringify(record.capabilities),
      revokedAt: record.revoked_at ?? null,
    })
    .onConflictDoUpdate({
      target: apiTokenLookups.secretHash,
      set: {
        tokenId: record.token_id,
        userId: record.user_id,
        githubLogin: record.github_login,
        capabilitiesJson: JSON.stringify(record.capabilities),
        revokedAt: record.revoked_at ?? null,
      },
    });
}

export async function readPackageClaim(
  db: D1Database,
  packageName: string,
): Promise<PackageClaimRecord | null> {
  const database = registryDb(db);
  const [row] = await database
    .select({
      package_name: packageClaims.packageName,
      package_locator: packageClaims.packageLocator,
      source_url: packageClaims.sourceUrl,
      package_subdir: packageClaims.packageSubdir,
      owner_user_id: packageClaims.ownerUserId,
      owner_github_login: packageClaims.ownerGithubLogin,
      claimed_at: packageClaims.claimedAt,
      updated_at: packageClaims.updatedAt,
    })
    .from(packageClaims)
    .where(eq(packageClaims.packageName, packageName))
    .limit(1);

  return row ? parsePackageClaimRecord(row) : null;
}

export async function writePackageClaim(
  db: D1Database,
  record: PackageClaimRecord,
): Promise<void> {
  const database = registryDb(db);
  await database
    .insert(packageClaims)
    .values({
      packageName: record.package_name,
      packageLocator: record.package_locator,
      sourceUrl: record.source_url,
      packageSubdir: record.package_subdir,
      ownerUserId: record.owner_user_id ?? null,
      ownerGithubLogin: record.owner_github_login ?? null,
      claimedAt: record.claimed_at,
      updatedAt: record.updated_at,
    })
    .onConflictDoUpdate({
      target: packageClaims.packageName,
      set: {
        packageLocator: record.package_locator,
        sourceUrl: record.source_url,
        packageSubdir: record.package_subdir,
        ownerUserId: record.owner_user_id ?? null,
        ownerGithubLogin: record.owner_github_login ?? null,
        claimedAt: record.claimed_at,
        updatedAt: record.updated_at,
      },
    });
}

export async function readPublishedRelease(
  db: D1Database,
  packageName: string,
  version: string,
): Promise<PublishedReleaseRecord | null> {
  const database = registryDb(db);
  const [row] = await database
    .select({
      package_name: publishedReleases.packageName,
      package_version: publishedReleases.packageVersion,
      package_locator: publishedReleases.packageLocator,
      source_url: publishedReleases.sourceUrl,
      package_subdir: publishedReleases.packageSubdir,
      artifact_sha256: publishedReleases.artifactSha256,
      package_description: publishedReleases.packageDescription,
      package_license: publishedReleases.packageLicense,
      package_homepage: publishedReleases.packageHomepage,
      package_repository: publishedReleases.packageRepository,
      package_root_module: publishedReleases.packageRootModule,
      package_categories_json: publishedReleases.packageCategoriesJson,
      package_keywords_json: publishedReleases.packageKeywordsJson,
      dependencies_json: publishedReleases.dependenciesJson,
      source_archive_key: publishedReleases.sourceArchiveKey,
      manifest_key: publishedReleases.manifestKey,
      published_at: publishedReleases.publishedAt,
      yanked_at: publishedReleases.yankedAt,
      yanked_by_github_login: publishedReleases.yankedByGithubLogin,
    })
    .from(publishedReleases)
    .where(
      and(
        eq(publishedReleases.packageName, packageName),
        eq(publishedReleases.packageVersion, version),
      ),
    )
    .limit(1);

  return row ? parsePublishedReleaseRecord(row) : null;
}

export async function hasPublishedRelease(db: D1Database, packageName: string): Promise<boolean> {
  const database = registryDb(db);
  const [row] = await database
    .select({ package_name: publishedReleases.packageName })
    .from(publishedReleases)
    .where(eq(publishedReleases.packageName, packageName))
    .limit(1);

  return row !== undefined;
}

export async function writePublishedRelease(
  db: D1Database,
  record: PublishedReleaseRecord,
): Promise<void> {
  const database = registryDb(db);
  await database
    .insert(publishedReleases)
    .values({
      packageName: record.package_name,
      packageVersion: record.package_version,
      packageLocator: record.package_locator,
      sourceUrl: record.source_url,
      packageSubdir: record.package_subdir,
      artifactSha256: record.artifact_sha256,
      packageDescription: record.package_description ?? null,
      packageLicense: record.package_license ?? null,
      packageHomepage: record.package_homepage ?? null,
      packageRepository: record.package_repository ?? null,
      packageRootModule: record.package_root_module ?? null,
      packageCategoriesJson: JSON.stringify(record.package_categories ?? []),
      packageKeywordsJson: JSON.stringify(record.package_keywords ?? []),
      dependenciesJson: JSON.stringify(record.dependencies),
      sourceArchiveKey: record.source_archive_key,
      manifestKey: record.manifest_key,
      publishedAt: record.published_at,
      yankedAt: record.yanked_at ?? null,
      yankedByGithubLogin: record.yanked_by_github_login ?? null,
    })
    .onConflictDoUpdate({
      target: [publishedReleases.packageName, publishedReleases.packageVersion],
      set: {
        packageLocator: record.package_locator,
        sourceUrl: record.source_url,
        packageSubdir: record.package_subdir,
        artifactSha256: record.artifact_sha256,
        packageDescription: record.package_description ?? null,
        packageLicense: record.package_license ?? null,
        packageHomepage: record.package_homepage ?? null,
        packageRepository: record.package_repository ?? null,
        packageRootModule: record.package_root_module ?? null,
        packageCategoriesJson: JSON.stringify(record.package_categories ?? []),
        packageKeywordsJson: JSON.stringify(record.package_keywords ?? []),
        dependenciesJson: JSON.stringify(record.dependencies),
        sourceArchiveKey: record.source_archive_key,
        manifestKey: record.manifest_key,
        publishedAt: record.published_at,
        yankedAt: record.yanked_at ?? null,
        yankedByGithubLogin: record.yanked_by_github_login ?? null,
      },
    });
}

export async function yankPublishedRelease(
  db: D1Database,
  packageName: string,
  version: string,
  actorGithubLogin: string,
  yankedAt: string,
): Promise<PublishedReleaseRecord | null> {
  const database = registryDb(db);
  const release = await readPublishedRelease(db, packageName, version);
  if (release === null) {
    return null;
  }

  if (release.yanked_at !== undefined) {
    return release;
  }

  await database
    .update(publishedReleases)
    .set({
      yankedAt,
      yankedByGithubLogin: actorGithubLogin,
    })
    .where(
      and(
        eq(publishedReleases.packageName, packageName),
        eq(publishedReleases.packageVersion, version),
      ),
    );

  return await readPublishedRelease(db, packageName, version);
}

export async function writeRegistryEvent(
  db: D1Database,
  record: RegistryEventRecord,
): Promise<void> {
  const database = registryDb(db);
  await database.insert(registryEvents).values({
    eventId: record.event_id,
    eventType: record.event_type,
    packageName: record.package_name ?? null,
    packageVersion: record.package_version ?? null,
    packageLocator: record.package_locator ?? null,
    payloadJson: JSON.stringify(record.payload),
    createdAt: record.created_at,
  });
}

export async function listRegistryEvents(
  db: D1Database,
  limit = 100,
  after?: string,
): Promise<RegistryEventRecord[]> {
  const database = registryDb(db);
  const rows = after === undefined
    ? await database
        .select({
          event_id: registryEvents.eventId,
          event_type: registryEvents.eventType,
          package_name: registryEvents.packageName,
          package_version: registryEvents.packageVersion,
          package_locator: registryEvents.packageLocator,
          payload_json: registryEvents.payloadJson,
          created_at: registryEvents.createdAt,
        })
        .from(registryEvents)
        .orderBy(desc(registryEvents.eventId))
        .limit(limit)
    : await database
        .select({
          event_id: registryEvents.eventId,
          event_type: registryEvents.eventType,
          package_name: registryEvents.packageName,
          package_version: registryEvents.packageVersion,
          package_locator: registryEvents.packageLocator,
          payload_json: registryEvents.payloadJson,
          created_at: registryEvents.createdAt,
        })
        .from(registryEvents)
        .where(gt(registryEvents.eventId, after))
        .orderBy(asc(registryEvents.eventId))
        .limit(limit);

  return rows.map((row) => parseRegistryEventRecord(row as RegistryEventRow));
}

export async function listPackageRegistryEvents(
  db: D1Database,
  packageName: string,
  packageVersion?: string,
  limit = 50,
): Promise<RegistryEventRecord[]> {
  const database = registryDb(db);

  const rows = packageVersion === undefined
    ? await database
        .select({
          event_id: registryEvents.eventId,
          event_type: registryEvents.eventType,
          package_name: registryEvents.packageName,
          package_version: registryEvents.packageVersion,
          package_locator: registryEvents.packageLocator,
          payload_json: registryEvents.payloadJson,
          created_at: registryEvents.createdAt,
        })
        .from(registryEvents)
        .where(eq(registryEvents.packageName, packageName))
        .orderBy(desc(registryEvents.eventId))
        .limit(limit)
    : await database
        .select({
          event_id: registryEvents.eventId,
          event_type: registryEvents.eventType,
          package_name: registryEvents.packageName,
          package_version: registryEvents.packageVersion,
          package_locator: registryEvents.packageLocator,
          payload_json: registryEvents.payloadJson,
          created_at: registryEvents.createdAt,
        })
        .from(registryEvents)
        .where(
          and(
            eq(registryEvents.packageName, packageName),
            eq(registryEvents.packageVersion, packageVersion),
          ),
        )
        .orderBy(desc(registryEvents.eventId))
        .limit(limit);

  return rows.map((row) => parseRegistryEventRecord(row as RegistryEventRow));
}

export async function readPackageOverviewDocument(
  db: D1Database,
  packageName: string,
): Promise<PackageOverviewDocument | null> {
  const packages = await listPackageSnapshots(db);
  const snapshot = packages.find((candidate) => candidate.package_name === packageName);
  if (snapshot === undefined) {
    return null;
  }

  const dependentMap = buildPackageDependentMap(packages);

  return {
    schema_version: 1,
    package_name: snapshot.package_name,
    latest_version: snapshot.latest_version,
    updated_at: snapshot.updated_at,
    published_at: snapshot.published_at,
    description: snapshot.description ?? undefined,
    license: snapshot.license ?? undefined,
    homepage: snapshot.homepage ?? undefined,
    repository: snapshot.repository ?? undefined,
    root_module: snapshot.root_module ?? undefined,
    canonical_locator: snapshot.canonical_locator,
    repo_url: snapshot.repo_url,
    subdir: snapshot.subdir,
    source_key: snapshot.source_key,
    manifest_key: snapshot.manifest_key,
    artifact_sha256: snapshot.artifact_sha256,
    owner_github_login: snapshot.owner_github_login ?? snapshot.repo_owner,
    owner_github_avatar_url: await resolveOwnerAvatarUrl(
      db,
      {
        owner_user_id: snapshot.owner_user_id,
        owner_github_login: snapshot.owner_github_login,
      },
      snapshot.owner_github_login ?? snapshot.repo_owner,
    ),
    release_count: snapshot.release_count,
    dependency_count: snapshot.dependencies.length,
    dependent_count: (dependentMap.get(packageName) ?? []).length,
    download_count: await readPackageDownloadCount(db, packageName),
    categories: snapshot.categories,
    keywords: snapshot.keywords,
    yanked: snapshot.yanked,
    yanked_at: snapshot.yanked_at ?? undefined,
    yanked_by_github_login: snapshot.yanked_by_github_login ?? undefined,
  };
}

export async function readPackageRelationsDocument(
  db: D1Database,
  packageName: string,
): Promise<PackageRelationsDocument | null> {
  const packages = await listPackageSnapshots(db);
  const snapshot = packages.find((candidate) => candidate.package_name === packageName);
  if (snapshot === undefined) {
    return null;
  }

  const dependentMap = buildPackageDependentMap(packages);
  return {
    schema_version: 1,
    package_name: snapshot.package_name,
    updated_at: snapshot.updated_at,
    dependencies: snapshot.dependencies,
    dependents: [...(dependentMap.get(packageName) ?? [])].sort((left, right) =>
      left.package_name.localeCompare(right.package_name)
    ),
  };
}

export async function readPackageDownloadsDocument(
  db: D1Database,
  packageName: string,
  windowDays = 30,
): Promise<PackageDownloadsDocument | null> {
  const database = registryDb(db);
  const releaseRows = await database
    .select({
      package_version: publishedReleases.packageVersion,
      published_at: publishedReleases.publishedAt,
      artifact_sha256: publishedReleases.artifactSha256,
      yanked_at: publishedReleases.yankedAt,
    })
    .from(publishedReleases)
    .where(eq(publishedReleases.packageName, packageName))
    .orderBy(desc(publishedReleases.publishedAt));

  if (releaseRows.length === 0) {
    return null;
  }

  const latestVersion = [...releaseRows]
    .sort((left, right) => compareVersionRecordsDesc(left, right))
    .find((release) => release.yanked_at === null || release.yanked_at === undefined)?.package_version
    ?? releaseRows[0]?.package_version
    ?? "";
  const [totalRow] = await database
    .select({ count: sql<number>`count(*)` })
    .from(packageDownloads)
    .where(and(eq(packageDownloads.packageName, packageName), countedRiotAgentSql(packageDownloads.riotAgent)));

  const versionCountRows = await database
    .select({
      version: packageDownloads.packageVersion,
      count: sql<number>`count(*)`,
    })
    .from(packageDownloads)
    .where(and(eq(packageDownloads.packageName, packageName), countedRiotAgentSql(packageDownloads.riotAgent)))
    .groupBy(packageDownloads.packageVersion);
  const versionCounts = new Map<string, number>();
  for (const row of versionCountRows) {
    versionCounts.set(row.version, toCount(row.count));
  }

  const startDate = startOfUtcDay(addUtcDays(new Date(), -(windowDays - 1)));
  const startIso = startDate.toISOString();
  const bucket = sql<string>`substr(${packageDownloads.downloadedAt}, 1, 10)`;

  const versionBucketRows = await database
    .select({
      date: bucket,
      version: packageDownloads.packageVersion,
      count: sql<number>`count(*)`,
    })
    .from(packageDownloads)
    .where(
      and(
        eq(packageDownloads.packageName, packageName),
        gte(packageDownloads.downloadedAt, startIso),
        countedRiotAgentSql(packageDownloads.riotAgent),
      ),
    )
    .groupBy(bucket, packageDownloads.packageVersion)
    .orderBy(asc(bucket), asc(packageDownloads.packageVersion));
  const versionDailyCounts = new Map<string, Map<string, number>>();
  for (const row of versionBucketRows) {
    const day = row.date;
    const perDay = versionDailyCounts.get(day) ?? new Map<string, number>();
    perDay.set(row.version, toCount(row.count));
    versionDailyCounts.set(day, perDay);
  }

  const dailyRows = await database
    .select({
      date: bucket,
      count: sql<number>`count(*)`,
    })
    .from(packageDownloads)
    .where(
      and(
        eq(packageDownloads.packageName, packageName),
        gte(packageDownloads.downloadedAt, startIso),
        countedRiotAgentSql(packageDownloads.riotAgent),
      ),
    )
    .groupBy(bucket)
    .orderBy(asc(bucket));
  const dailyCounts = toDateCountMap(dailyRows);
  const sortedReleases = [...releaseRows].sort((left, right) => compareVersionRecordsDesc(left, right));
  const visibleVersions = sortedReleases.slice(0, 5).map((release) => release.package_version);
  const visibleVersionSet = new Set(visibleVersions);
  const dayKeys = [...Array(windowDays)].map((_, index) => addUtcDays(startDate, index).toISOString().slice(0, 10));
  const stackedDownloads = visibleVersions.map((version) => ({
    key: version,
    label: version,
    is_latest: version === latestVersion,
    is_other: false,
    total_downloads: versionCounts.get(version) ?? 0,
    daily_downloads: dayKeys.map((day) => ({
      date: day,
      download_count: versionDailyCounts.get(day)?.get(version) ?? 0,
    })),
  }));
  const otherDailyDownloads = dayKeys.map((day) => {
    const perDay = versionDailyCounts.get(day);
    let count = 0;
    if (perDay !== undefined) {
      for (const [version, value] of perDay.entries()) {
        if (!visibleVersionSet.has(version)) {
          count += value;
        }
      }
    }

    return {
      date: day,
      download_count: count,
    };
  });
  const otherTotalDownloads = otherDailyDownloads.reduce((total, point) => total + point.download_count, 0);
  if (otherTotalDownloads > 0) {
    stackedDownloads.push({
      key: "other",
      label: "Other",
      is_latest: false,
      is_other: true,
      total_downloads: otherTotalDownloads,
      daily_downloads: otherDailyDownloads,
    });
  }

  return {
    schema_version: 1,
    package_name: packageName,
    latest_version: latestVersion,
    generated_at: new Date().toISOString(),
    window_days: windowDays,
    total_downloads: toCount(totalRow?.count),
    daily_downloads: [...Array(windowDays)].map((_, index) => {
      const day = addUtcDays(startDate, index).toISOString().slice(0, 10);
      return {
        date: day,
        download_count: dailyCounts.get(day) ?? 0,
      };
    }),
    stacked_downloads: stackedDownloads,
    version_downloads: [...sortedReleases]
      .map((release) => ({
        version: release.package_version,
        published_at: release.published_at,
        download_count: versionCounts.get(release.package_version) ?? 0,
        is_latest: release.package_version === latestVersion,
      })),
  };
}

export async function readRecentPackagesDocument(
  db: D1Database,
): Promise<RecentPackagesDocument | null> {
  const packages = await listPackageSnapshots(db);
  if (packages.length === 0) {
    return null;
  }

  const avatarUrls = await resolveOwnerAvatarUrls(db, packages);

  return {
    schema_version: 1,
    generated_at: new Date().toISOString(),
    packages: packages
      .slice(0, 12)
      .map((snapshot) => toWebPackageListItem(snapshot, avatarUrls)),
  };
}

export async function readPopularPackagesDocument(
  db: D1Database,
): Promise<PopularPackagesDocument | null> {
  const packages = await listPackageSnapshots(db);
  if (packages.length === 0) {
    return null;
  }

  const avatarUrls = await resolveOwnerAvatarUrls(db, packages);
  const dependentMap = buildPackageDependentMap(packages);

  return {
    schema_version: 1,
    generated_at: new Date().toISOString(),
    packages: packages
      .map((snapshot) => ({
        ...toWebPackageListItem(snapshot, avatarUrls),
        dependent_count: (dependentMap.get(snapshot.package_name) ?? []).length,
        release_count: snapshot.release_count,
      }))
      .sort((left, right) => {
        if (right.dependent_count !== left.dependent_count) {
          return right.dependent_count - left.dependent_count;
        }

        if (right.release_count !== left.release_count) {
          return right.release_count - left.release_count;
        }

        return right.updated_at.localeCompare(left.updated_at);
      })
      .slice(0, 12),
  };
}

export async function readCategoriesIndexDocument(
  db: D1Database,
): Promise<CategoriesIndexDocument | null> {
  const packages = await listPackageSnapshots(db);
  if (packages.length === 0) {
    return null;
  }

  const packagesByCategory = new Map<string, Set<string>>();
  for (const snapshot of packages) {
    for (const category of snapshot.categories) {
      const normalized = category.trim();
      if (normalized.length === 0) {
        continue;
      }

      const packageNames = packagesByCategory.get(normalized) ?? new Set<string>();
      packageNames.add(snapshot.package_name);
      packagesByCategory.set(normalized, packageNames);
    }
  }

  const categories: CategorySummary[] = [...packagesByCategory.entries()]
    .map(([name, packageNames]) => ({
      name,
      slug: toSlug(name),
      package_count: packageNames.size,
      packages: [...packageNames].sort(),
    }))
    .sort((left, right) => {
      if (right.package_count !== left.package_count) {
        return right.package_count - left.package_count;
      }

      return left.name.localeCompare(right.name);
    });

  return {
    schema_version: 1,
    generated_at: new Date().toISOString(),
    categories,
  };
}

export async function readOwnerPackagesDocument(
  db: D1Database,
  ownerGithubLogin: string,
): Promise<OwnerPackagesDocument | null> {
  const packages = await listPackageSnapshots(db);
  const ownerPackages = packages.filter((snapshot) =>
    (snapshot.owner_github_login ?? snapshot.repo_owner).toLowerCase() === ownerGithubLogin.toLowerCase()
  );
  if (ownerPackages.length === 0) {
    return null;
  }

  const avatarUrls = await resolveOwnerAvatarUrls(db, ownerPackages);
  const normalizedLogin = ownerGithubLogin.toLowerCase();

  const packageItems = ownerPackages.map((snapshot) => toWebPackageListItem(snapshot, avatarUrls));
  return {
    schema_version: 1,
    generated_at: new Date().toISOString(),
    owner_github_login: ownerPackages[0]?.owner_github_login ?? ownerPackages[0]?.repo_owner ?? ownerGithubLogin,
    owner_github_avatar_url: avatarUrls.get(normalizedLogin),
    package_count: packageItems.length,
    latest_update_at: packageItems[0]?.updated_at,
    packages: packageItems,
  };
}

export async function readRegistryStatsSummaryDocument(
  db: D1Database,
): Promise<RegistryStatsSummaryDocument> {
  const database = registryDb(db);
  const [packageDownloadsRow] = await database
    .select({ count: sql<number>`count(*)` })
    .from(packageDownloads)
    .where(countedRiotAgentSql(packageDownloads.riotAgent));
  const [riotDownloadsRow] = await database
    .select({ count: sql<number>`count(*)` })
    .from(binaryDownloads)
    .where(and(eq(binaryDownloads.binaryName, "riot"), countedRiotAgentSql(binaryDownloads.riotAgent)));
  const [ocamlDownloadsRow] = await database
    .select({ count: sql<number>`count(*)` })
    .from(binaryDownloads)
    .where(and(eq(binaryDownloads.binaryName, "ocaml"), countedRiotAgentSql(binaryDownloads.riotAgent)));
  const [packagesRow] = await database
    .select({ count: sql<number>`count(*)` })
    .from(packages);
  const [versionsRow] = await database
    .select({ count: sql<number>`count(*)` })
    .from(publishedReleases);
  const [usersRow] = await database
    .select({ count: sql<number>`count(*)` })
    .from(users);

  return {
    schema_version: 1,
    generated_at: new Date().toISOString(),
    total_package_downloads: toCount(packageDownloadsRow?.count),
    total_riot_downloads: toCount(riotDownloadsRow?.count),
    total_ocaml_downloads: toCount(ocamlDownloadsRow?.count),
    total_packages: toCount(packagesRow?.count),
    total_versions: toCount(versionsRow?.count),
    total_users: toCount(usersRow?.count),
  };
}

export async function readRegistryStatsDashboardDocument(
  db: D1Database,
  window: RegistryStatsWindowKey = "30d",
): Promise<RegistryStatsDashboardDocument> {
  const database = registryDb(db);
  const summary = await readRegistryStatsSummaryDocument(db);
  const [indexReadsRow] = await database
    .select({ count: sql<number>`count(*)` })
    .from(indexReads)
    .where(countedRiotAgentSql(indexReads.riotAgent));
  const windowConfig = await resolveStatsWindow(db, window);
  const activity = await listRegistryStatsActivity(db, windowConfig);

  return {
    schema_version: 1,
    generated_at: summary.generated_at,
    window,
    window_label: windowConfig.label,
    window_days: windowConfig.windowDays,
    available_windows: STATS_WINDOW_OPTIONS,
    summary: {
      ...summary,
      total_index_reads: toCount(indexReadsRow?.count),
      mean_package_downloads_per_package: summary.total_packages === 0
        ? 0
        : summary.total_package_downloads / summary.total_packages,
    },
    daily_activity: activity,
    metrics: buildRegistryStatsMetricSeries(activity),
    top_packages: await listTopDownloadedPackages(db, 10),
    latest_releases: await listLatestPublishedReleases(db, 10),
  };
}

async function listPackageSnapshots(db: D1Database): Promise<PackageSnapshot[]> {
  const database = registryDb(db);
  const rows = await database
    .select({
      package_name: publishedReleases.packageName,
      package_version: publishedReleases.packageVersion,
      package_locator: publishedReleases.packageLocator,
      source_url: publishedReleases.sourceUrl,
      package_subdir: publishedReleases.packageSubdir,
      artifact_sha256: publishedReleases.artifactSha256,
      package_description: publishedReleases.packageDescription,
      package_license: publishedReleases.packageLicense,
      package_homepage: publishedReleases.packageHomepage,
      package_repository: publishedReleases.packageRepository,
      package_root_module: publishedReleases.packageRootModule,
      package_categories_json: publishedReleases.packageCategoriesJson,
      package_keywords_json: publishedReleases.packageKeywordsJson,
      dependencies_json: publishedReleases.dependenciesJson,
      source_archive_key: publishedReleases.sourceArchiveKey,
      manifest_key: publishedReleases.manifestKey,
      published_at: publishedReleases.publishedAt,
      yanked_at: publishedReleases.yankedAt,
      yanked_by_github_login: publishedReleases.yankedByGithubLogin,
    })
    .from(publishedReleases)
    .orderBy(asc(publishedReleases.packageName), desc(publishedReleases.publishedAt));

  const claims = await listPackageClaims(db);
  const grouped = new Map<string, PublishedReleaseRecord[]>();

  for (const row of rows) {
    const release = parsePublishedReleaseRecord(row as PublishedReleaseRow);
    const packageReleases = grouped.get(release.package_name) ?? [];
    packageReleases.push(release);
    grouped.set(release.package_name, packageReleases);
  }

  const snapshots = [...grouped.entries()].map(([packageName, releases]) => {
    releases.sort(compareReleaseVersionsDesc);
    const latestRelease = releases.find((release) => !isYankedRelease(release)) ?? releases[0];
    if (latestRelease === undefined) {
      throw new Error(`Package ${packageName} has no releases.`);
    }

    const claim = claims.get(packageName);
    const ownerGithubLogin = claim?.owner_github_login ?? parseOwnerFromLocator(latestRelease.package_locator);
    const releaseSummaries = releases.map((release) => ({
      version: release.package_version,
      published_at: release.published_at,
      yanked: isYankedRelease(release),
      yanked_at: release.yanked_at,
      yanked_by_github_login: release.yanked_by_github_login,
      package_path: `/p/${packageName}/${release.package_version}`,
    } satisfies WebPackageReleaseListItem));
    return {
      package_name: packageName,
      latest_version: latestRelease.package_version,
      description: latestRelease.package_description,
      license: latestRelease.package_license,
      homepage: latestRelease.package_homepage,
      repository: latestRelease.package_repository,
      root_module: latestRelease.package_root_module,
      canonical_locator: latestRelease.package_locator,
      repo_url: latestRelease.source_url,
      repo_owner: parseOwnerFromLocator(latestRelease.package_locator),
      subdir: latestRelease.package_subdir,
      release_count: releases.length,
      updated_at: latestRelease.published_at,
      published_at: latestRelease.published_at,
      source_key: latestRelease.source_archive_key,
      manifest_key: latestRelease.manifest_key,
      artifact_sha256: latestRelease.artifact_sha256,
      owner_user_id: claim?.owner_user_id,
      owner_github_login: ownerGithubLogin,
      categories: latestRelease.package_categories ?? [],
      keywords: latestRelease.package_keywords ?? [],
      dependencies: normalizeDependencies(JSON.stringify(latestRelease.dependencies)),
      releases: releaseSummaries,
      yanked: isYankedRelease(latestRelease),
      yanked_at: latestRelease.yanked_at,
      yanked_by_github_login: latestRelease.yanked_by_github_login,
      yanked_release_count: releases.filter((release) => isYankedRelease(release)).length,
    } satisfies PackageSnapshot;
  });

  snapshots.sort((left, right) => {
    const timestamp = right.updated_at.localeCompare(left.updated_at);
    if (timestamp !== 0) {
      return timestamp;
    }

    return left.package_name.localeCompare(right.package_name);
  });

  return snapshots;
}

async function readPackageDownloadCount(db: D1Database, packageName: string): Promise<number> {
  const database = registryDb(db);
  const [row] = await database
    .select({ count: sql<number>`count(*)` })
    .from(packageDownloads)
    .where(and(eq(packageDownloads.packageName, packageName), countedRiotAgentSql(packageDownloads.riotAgent)));

  return toCount(row?.count);
}

async function listRegistryStatsActivity(
  db: D1Database,
  window: ResolvedStatsWindow,
): Promise<RegistryStatsActivityPoint[]> {
  const database = registryDb(db);
  const startIso = window.start.toISOString();

  const packageBucket = statsBucketSql(packageDownloads.downloadedAt, window.granularity);
  const binaryBucket = statsBucketSql(binaryDownloads.downloadedAt, window.granularity);
  const indexBucket = statsBucketSql(indexReads.readAt, window.granularity);
  const releaseBucket = statsBucketSql(publishedReleases.publishedAt, window.granularity);

  const [packageRows, riotRows, ocamlRows, indexRows, releaseRows] = await Promise.all([
    database
      .select({
        date: packageBucket,
        count: sql<number>`count(*)`,
      })
      .from(packageDownloads)
      .where(and(gte(packageDownloads.downloadedAt, startIso), countedRiotAgentSql(packageDownloads.riotAgent)))
      .groupBy(packageBucket)
      .orderBy(asc(packageBucket)),
    database
      .select({
        date: binaryBucket,
        count: sql<number>`count(*)`,
      })
      .from(binaryDownloads)
      .where(
        and(
          gte(binaryDownloads.downloadedAt, startIso),
          eq(binaryDownloads.binaryName, "riot"),
          countedRiotAgentSql(binaryDownloads.riotAgent),
        ),
      )
      .groupBy(binaryBucket)
      .orderBy(asc(binaryBucket)),
    database
      .select({
        date: binaryBucket,
        count: sql<number>`count(*)`,
      })
      .from(binaryDownloads)
      .where(
        and(
          gte(binaryDownloads.downloadedAt, startIso),
          eq(binaryDownloads.binaryName, "ocaml"),
          countedRiotAgentSql(binaryDownloads.riotAgent),
        ),
      )
      .groupBy(binaryBucket)
      .orderBy(asc(binaryBucket)),
    database
      .select({
        date: indexBucket,
        count: sql<number>`count(*)`,
      })
      .from(indexReads)
      .where(and(gte(indexReads.readAt, startIso), countedRiotAgentSql(indexReads.riotAgent)))
      .groupBy(indexBucket)
      .orderBy(asc(indexBucket)),
    database
      .select({
        date: releaseBucket,
        count: sql<number>`count(*)`,
      })
      .from(publishedReleases)
      .where(gte(publishedReleases.publishedAt, startIso))
      .groupBy(releaseBucket)
      .orderBy(asc(releaseBucket)),
  ]);

  const packageCounts = toDateCountMap(packageRows);
  const riotCounts = toDateCountMap(riotRows);
  const ocamlCounts = toDateCountMap(ocamlRows);
  const indexCounts = toDateCountMap(indexRows);
  const releaseCounts = toDateCountMap(releaseRows);

  return window.buckets.map((bucket) => {
    return {
      date: bucket,
      package_downloads: packageCounts.get(bucket) ?? 0,
      riot_downloads: riotCounts.get(bucket) ?? 0,
      ocaml_downloads: ocamlCounts.get(bucket) ?? 0,
      index_reads: indexCounts.get(bucket) ?? 0,
      releases_published: releaseCounts.get(bucket) ?? 0,
    } satisfies RegistryStatsActivityPoint;
  });
}

async function listTopDownloadedPackages(
  db: D1Database,
  limit: number,
): Promise<RegistryStatsDashboardDocument["top_packages"]> {
  const database = registryDb(db);
  const totalDownloads = sql<number>`count(*)`;

  const rows = await database
    .select({
      package_name: packageDownloads.packageName,
      latest_version: sql<string>`coalesce(${packages.latestVersion}, '')`,
      description: packages.description,
      download_count: totalDownloads,
    })
    .from(packageDownloads)
    .leftJoin(packages, eq(packageDownloads.packageName, packages.packageName))
    .where(countedRiotAgentSql(packageDownloads.riotAgent))
    .groupBy(packageDownloads.packageName, packages.latestVersion, packages.description)
    .orderBy(desc(totalDownloads), asc(packageDownloads.packageName))
    .limit(limit);

  return rows.map((row) => ({
    package_name: row.package_name,
    latest_version: row.latest_version,
    description: row.description ?? undefined,
    package_path: `/p/${row.package_name}`,
    download_count: toCount(row.download_count),
  }));
}

async function listLatestPublishedReleases(
  db: D1Database,
  limit: number,
): Promise<RegistryStatsDashboardDocument["latest_releases"]> {
  const database = registryDb(db);
  const rows = await database
    .select({
      package_name: publishedReleases.packageName,
      package_version: publishedReleases.packageVersion,
      published_at: publishedReleases.publishedAt,
    })
    .from(publishedReleases)
    .orderBy(desc(publishedReleases.publishedAt), asc(publishedReleases.packageName))
    .limit(limit);

  return rows.map((row) => ({
    package_name: row.package_name,
    package_version: row.package_version,
    package_path: `/p/${row.package_name}/${row.package_version}`,
    published_at: row.published_at,
  }));
}

async function listPackageClaims(db: D1Database): Promise<Map<string, PackageClaimRecord>> {
  const database = registryDb(db);
  const rows = await database
    .select({
      package_name: packageClaims.packageName,
      package_locator: packageClaims.packageLocator,
      source_url: packageClaims.sourceUrl,
      package_subdir: packageClaims.packageSubdir,
      owner_user_id: packageClaims.ownerUserId,
      owner_github_login: packageClaims.ownerGithubLogin,
      claimed_at: packageClaims.claimedAt,
      updated_at: packageClaims.updatedAt,
    })
    .from(packageClaims);

  const claims = new Map<string, PackageClaimRecord>();
  for (const row of rows) {
    claims.set(row.package_name, parsePackageClaimRecord(row as PackageClaimRow));
  }

  return claims;
}

function buildPackageDependentMap(
  packages: PackageSnapshot[],
): Map<string, PackageRelationDependent[]> {
  const dependents = new Map<string, PackageRelationDependent[]>();
  for (const snapshot of packages) {
    for (const dependency of snapshot.dependencies) {
      const existing = dependents.get(dependency.package_name) ?? [];
      existing.push({
        package_name: snapshot.package_name,
        latest_version: snapshot.latest_version,
        requirement: dependency.requirement,
      });
      dependents.set(dependency.package_name, existing);
    }
  }

  return dependents;
}

function toDateCountMap(rows: Array<{ date: string; count: number }>): Map<string, number> {
  const values = new Map<string, number>();
  for (const row of rows) {
    values.set(row.date, toCount(row.count));
  }

  return values;
}

const STATS_WINDOW_OPTIONS: RegistryStatsWindowOption[] = [
  { key: "all", label: "All time" },
  { key: "year", label: "This year" },
  { key: "30d", label: "Last 30 days" },
  { key: "7d", label: "This week" },
];

const STATS_METRICS: Array<{
  key: RegistryStatsMetricKey;
  label: string;
  color: string;
}> = [
  { key: "package_downloads", label: "Package installs", color: "var(--chart-3)" },
  { key: "riot_downloads", label: "Riot installs", color: "var(--chart-1)" },
  { key: "ocaml_downloads", label: "OCaml installs", color: "var(--chart-2)" },
  { key: "index_reads", label: "Index refreshes", color: "var(--chart-5)" },
  { key: "releases_published", label: "Releases published", color: "var(--chart-4)" },
];

type StatsGranularity = "hour" | "day";

interface ResolvedStatsWindow {
  key: RegistryStatsWindowKey;
  label: string;
  start: Date;
  windowDays: number;
  granularity: StatsGranularity;
  buckets: string[];
}

async function resolveStatsWindow(
  db: D1Database,
  window: RegistryStatsWindowKey,
): Promise<ResolvedStatsWindow> {
  const now = new Date();
  const todayStart = startOfUtcDay(now);
  const label = STATS_WINDOW_OPTIONS.find((option) => option.key === window)?.label ?? "Last 30 days";

  switch (window) {
    case "7d": {
      const start = startOfUtcDay(addUtcDays(todayStart, -6));
      return {
        key: window,
        label,
        start,
        windowDays: 7,
        granularity: "day",
        buckets: buildDailyBuckets(start, todayStart),
      };
    }
    case "year": {
      const start = new Date(Date.UTC(now.getUTCFullYear(), 0, 1));
      return {
        key: window,
        label,
        start,
        windowDays: diffUtcDays(start, todayStart) + 1,
        granularity: "day",
        buckets: buildDailyBuckets(start, todayStart),
      };
    }
    case "all": {
      const earliest = await readEarliestStatsTimestamp(db);
      const start = startOfUtcDay(earliest ?? todayStart);
      return {
        key: window,
        label,
        start,
        windowDays: diffUtcDays(start, todayStart) + 1,
        granularity: "day",
        buckets: buildDailyBuckets(start, todayStart),
      };
    }
    case "30d":
    default: {
      const start = startOfUtcDay(addUtcDays(todayStart, -29));
      return {
        key: "30d",
        label,
        start,
        windowDays: 30,
        granularity: "day",
        buckets: buildDailyBuckets(start, todayStart),
      };
    }
  }
}

async function readEarliestStatsTimestamp(db: D1Database): Promise<Date | null> {
  const database = registryDb(db);
  const [packageRow, binaryRow, indexRow, releaseRow] = await Promise.all([
    database.select({ value: sql<string | null>`min(${packageDownloads.downloadedAt})` })
      .from(packageDownloads)
      .where(countedRiotAgentSql(packageDownloads.riotAgent)),
    database.select({ value: sql<string | null>`min(${binaryDownloads.downloadedAt})` })
      .from(binaryDownloads)
      .where(countedRiotAgentSql(binaryDownloads.riotAgent)),
    database.select({ value: sql<string | null>`min(${indexReads.readAt})` })
      .from(indexReads)
      .where(countedRiotAgentSql(indexReads.riotAgent)),
    database.select({ value: sql<string | null>`min(${publishedReleases.publishedAt})` }).from(publishedReleases),
  ]);

  const values = [packageRow[0]?.value, binaryRow[0]?.value, indexRow[0]?.value, releaseRow[0]?.value]
    .filter((value): value is string => typeof value === "string" && value.length > 0)
    .sort();

  if (values.length === 0) {
    return null;
  }

  const earliest = values[0];
  if (earliest === undefined) {
    return null;
  }

  return new Date(earliest);
}

function buildDailyBuckets(start: Date, end: Date): string[] {
  const days = diffUtcDays(start, end) + 1;
  return [...Array(days)].map((_, index) => addUtcDays(start, index).toISOString().slice(0, 10));
}

function diffUtcDays(start: Date, end: Date): number {
  return Math.floor((startOfUtcDay(end).getTime() - startOfUtcDay(start).getTime()) / 86_400_000);
}

function statsBucketSql(column: unknown, granularity: StatsGranularity) {
  if (granularity === "hour") {
    return sql<string>`substr(${column}, 1, 13) || ':00:00Z'`;
  }

  return sql<string>`substr(${column}, 1, 10)`;
}

function buildRegistryStatsMetricSeries(
  points: RegistryStatsActivityPoint[],
): RegistryStatsMetricSeries[] {
  return STATS_METRICS.map((metric) => ({
    key: metric.key,
    label: metric.label,
    color: metric.color,
    total: points.reduce((sum, point) => sum + point[metric.key], 0),
    points,
  }));
}

function startOfUtcDay(value: Date): Date {
  return new Date(Date.UTC(value.getUTCFullYear(), value.getUTCMonth(), value.getUTCDate()));
}

function addUtcDays(value: Date, days: number): Date {
  const copy = new Date(value);
  copy.setUTCDate(copy.getUTCDate() + days);
  return copy;
}

async function resolveOwnerAvatarUrls(
  db: D1Database,
  rows: Array<{
    repo_owner: string;
    owner_user_id?: string | null;
    owner_github_login?: string | null;
  }>,
): Promise<Map<string, string | undefined>> {
  const avatarUrls = new Map<string, string | undefined>();

  for (const row of rows) {
    const ownerGithubLogin = row.owner_github_login ?? row.repo_owner;
    const normalizedLogin = ownerGithubLogin.toLowerCase();
    if (avatarUrls.has(normalizedLogin)) {
      continue;
    }

    avatarUrls.set(
      normalizedLogin,
      await resolveOwnerAvatarUrl(
        db,
        {
          owner_user_id: row.owner_user_id,
          owner_github_login: row.owner_github_login,
        },
        ownerGithubLogin,
      ),
    );
  }

  return avatarUrls;
}

async function resolveOwnerAvatarUrl(
  db: D1Database,
  owner: {
    owner_user_id?: string | null;
    owner_github_login?: string | null;
  },
  fallbackGithubLogin: string,
): Promise<string | undefined> {
  if (owner.owner_user_id !== undefined && owner.owner_user_id !== null) {
    const user = await readUserRecord(db, owner.owner_user_id);
    if (user?.github_avatar_url !== undefined) {
      return user.github_avatar_url;
    }
  }

  const githubLogin = owner.owner_github_login ?? fallbackGithubLogin;
  const loginRecord = await readUserLoginRecord(db, githubLogin);
  if (loginRecord === null) {
    return undefined;
  }

  const user = await readUserRecord(db, loginRecord.user_id);
  return user?.github_avatar_url;
}

function toWebPackageListItem(
  row: PackageSnapshot,
  avatarUrls: Map<string, string | undefined>,
): WebPackageListItem {
  const ownerGithubLogin = row.owner_github_login ?? row.repo_owner;
  const normalizedOwner = ownerGithubLogin.toLowerCase();

  return {
    package_name: row.package_name,
    latest_version: row.latest_version,
    description: row.description ?? undefined,
    license: row.license ?? undefined,
    owner_github_login: ownerGithubLogin,
    owner_github_avatar_url: avatarUrls.get(normalizedOwner),
    categories: row.categories,
    updated_at: row.updated_at,
    repo_url: row.repo_url,
    repository: row.repository ?? undefined,
    subdir: row.subdir,
    release_count: row.release_count,
    yanked_release_count: row.yanked_release_count,
    package_path: `/p/${row.package_name}`,
    releases: row.releases,
  };
}

function isYankedRelease(release: PublishedReleaseRecord): boolean {
  return release.yanked_at !== undefined && release.yanked_at !== null;
}

function toCount(value: unknown): number {
  if (typeof value === "number") {
    return value;
  }

  if (typeof value === "string") {
    const parsed = Number.parseInt(value, 10);
    return Number.isFinite(parsed) ? parsed : 0;
  }

  return 0;
}

function countedRiotAgentSql(column: unknown) {
  const exclusions = INTERNAL_RIOT_AGENT_PREFIXES.map((prefix) => sql`${column} NOT LIKE ${`${prefix}%`}`);
  if (exclusions.length === 0) {
    return sql`1 = 1`;
  }

  return sql`(${column} IS NOT NULL AND ${sql.join(exclusions, sql` AND `)})`;
}

function normalizeDependencies(dependenciesJson: string): PackageRelationDependency[] {
  return parseJsonArray<Record<string, unknown>>(dependenciesJson)
    .map((dependency) => {
      const packageName =
        typeof dependency.package === "string"
          ? dependency.package
          : typeof dependency.name === "string"
            ? dependency.name
            : null;

      if (packageName === null || packageName.length === 0) {
        return null;
      }

      const requirement =
        typeof dependency.version === "string"
          ? dependency.version
          : typeof dependency.requirement === "string"
            ? dependency.requirement
            : typeof dependency.raw === "string"
              ? dependency.raw
              : "unspecified";

      return {
        package_name: packageName,
        requirement,
      };
    })
    .filter((dependency): dependency is PackageRelationDependency => dependency !== null);
}

function compareReleaseVersionsDesc(
  left: PublishedReleaseRecord,
  right: PublishedReleaseRecord,
): number {
  const leftVersion = semver.valid(left.package_version);
  const rightVersion = semver.valid(right.package_version);

  if (leftVersion !== null && rightVersion !== null) {
    const semverResult = semver.rcompare(leftVersion, rightVersion);
    if (semverResult !== 0) {
      return semverResult;
    }
  } else if (leftVersion !== null || rightVersion !== null) {
    return leftVersion !== null ? -1 : 1;
  }

  const publishedAtResult = right.published_at.localeCompare(left.published_at);
  if (publishedAtResult !== 0) {
    return publishedAtResult;
  }

  const versionResult = right.package_version.localeCompare(left.package_version);
  if (versionResult !== 0) {
    return versionResult;
  }

  return right.artifact_sha256.localeCompare(left.artifact_sha256);
}

function compareVersionRecordsDesc(
  left: { package_version: string; published_at: string; artifact_sha256: string },
  right: { package_version: string; published_at: string; artifact_sha256: string },
): number {
  const leftVersion = semver.valid(left.package_version);
  const rightVersion = semver.valid(right.package_version);

  if (leftVersion !== null && rightVersion !== null) {
    const semverResult = semver.rcompare(leftVersion, rightVersion);
    if (semverResult !== 0) {
      return semverResult;
    }
  } else if (leftVersion !== null || rightVersion !== null) {
    return leftVersion !== null ? -1 : 1;
  }

  const publishedAtResult = right.published_at.localeCompare(left.published_at);
  if (publishedAtResult !== 0) {
    return publishedAtResult;
  }

  const versionResult = right.package_version.localeCompare(left.package_version);
  if (versionResult !== 0) {
    return versionResult;
  }

  return right.artifact_sha256.localeCompare(left.artifact_sha256);
}

function parseOwnerFromLocator(locator: string): string {
  const parts = locator.split("/");
  return parts[1] ?? "unknown";
}

function toSlug(value: string): string {
  return value
    .toLowerCase()
    .trim()
    .replace(/[^a-z0-9]+/g, "-")
    .replace(/^-+|-+$/g, "");
}

interface PackageSnapshot {
  package_name: string;
  latest_version: string;
  description?: string | null;
  license?: string | null;
  homepage?: string | null;
  repository?: string | null;
  root_module?: string | null;
  canonical_locator: string;
  subdir: string;
  release_count: number;
  updated_at: string;
  published_at: string;
  repo_url: string;
  repo_owner: string;
  source_key: string;
  manifest_key: string;
  artifact_sha256: string;
  owner_user_id?: string | null;
  owner_github_login?: string | null;
  categories: string[];
  keywords: string[];
  dependencies: PackageRelationDependency[];
  releases: WebPackageReleaseListItem[];
  yanked: boolean;
  yanked_at?: string | null;
  yanked_by_github_login?: string | null;
  yanked_release_count: number;
}

interface ApiTokenRow {
  token_id: string;
  user_id: string;
  github_login: string;
  name: string;
  secret_hash: string;
  capabilities_json: string;
  created_at: string;
  last_used_at?: string | null;
  revoked_at?: string | null;
}

interface UserRow {
  user_id: string;
  github_id: number;
  github_login: string;
  github_name?: string | null;
  github_avatar_url?: string | null;
  github_email?: string | null;
  github_email_verified?: number | boolean | null;
  created_at: string;
  updated_at: string;
}

interface PackageClaimRow {
  package_name: string;
  package_locator: string;
  source_url: string;
  package_subdir: string;
  owner_user_id?: string | null;
  owner_github_login?: string | null;
  claimed_at: string;
  updated_at: string;
}

interface ApiTokenLookupRow {
  token_id: string;
  user_id: string;
  github_login: string;
  capabilities_json: string;
  revoked_at?: string | null;
}

interface PublishedReleaseRow {
  package_name: string;
  package_version: string;
  package_locator: string;
  source_url: string;
  package_subdir: string;
  artifact_sha256: string;
  package_description?: string | null;
  package_license?: string | null;
  package_homepage?: string | null;
  package_repository?: string | null;
  package_root_module?: string | null;
  package_categories_json: string;
  package_keywords_json: string;
  dependencies_json: string;
  source_archive_key: string;
  manifest_key: string;
  published_at: string;
  yanked_at?: string | null;
  yanked_by_github_login?: string | null;
}

interface RegistryEventRow {
  event_id: string;
  event_type: RegistryEventRecord["event_type"];
  package_name?: string | null;
  package_version?: string | null;
  package_locator?: string | null;
  payload_json: string;
  created_at: string;
}

function parseApiTokenRecord(row: ApiTokenRow): ApiTokenRecord {
  return {
    token_id: row.token_id,
    user_id: row.user_id,
    github_login: row.github_login,
    name: row.name,
    secret_hash: row.secret_hash,
    capabilities: parseJsonArray<ApiTokenCapability>(row.capabilities_json),
    created_at: row.created_at,
    last_used_at: row.last_used_at ?? undefined,
    revoked_at: row.revoked_at ?? undefined,
  };
}

function parseUserRecord(row: UserRow): UserRecord {
  return {
    user_id: row.user_id,
    github_id: row.github_id,
    github_login: row.github_login,
    github_name: row.github_name ?? undefined,
    github_avatar_url: row.github_avatar_url ?? undefined,
    github_email: row.github_email ?? undefined,
    github_email_verified: row.github_email_verified === 1 || row.github_email_verified === true,
    created_at: row.created_at,
    updated_at: row.updated_at,
  };
}

function parsePackageClaimRecord(row: PackageClaimRow): PackageClaimRecord {
  return {
    package_name: row.package_name,
    package_locator: row.package_locator,
    source_url: row.source_url,
    package_subdir: row.package_subdir,
    owner_user_id: row.owner_user_id ?? undefined,
    owner_github_login: row.owner_github_login ?? undefined,
    claimed_at: row.claimed_at,
    updated_at: row.updated_at,
  };
}

function parseApiTokenLookupRecord(row: ApiTokenLookupRow): ApiTokenLookupRecord {
  return {
    token_id: row.token_id,
    user_id: row.user_id,
    github_login: row.github_login,
    capabilities: parseJsonArray<ApiTokenCapability>(row.capabilities_json),
    revoked_at: row.revoked_at ?? undefined,
  };
}

function parsePublishedReleaseRecord(row: PublishedReleaseRow): PublishedReleaseRecord {
  return {
    package_name: row.package_name,
    package_version: row.package_version,
    package_locator: row.package_locator,
    source_url: row.source_url,
    package_subdir: row.package_subdir,
    artifact_sha256: row.artifact_sha256,
    package_description: row.package_description ?? undefined,
    package_license: row.package_license ?? undefined,
    package_homepage: row.package_homepage ?? undefined,
    package_repository: row.package_repository ?? undefined,
    package_root_module: row.package_root_module ?? undefined,
    package_categories: parseJsonArray(row.package_categories_json),
    package_keywords: parseJsonArray(row.package_keywords_json),
    dependencies: parseJsonArray<Record<string, unknown>>(row.dependencies_json),
    source_archive_key: row.source_archive_key,
    manifest_key: row.manifest_key,
    published_at: row.published_at,
    yanked_at: row.yanked_at ?? undefined,
    yanked_by_github_login: row.yanked_by_github_login ?? undefined,
  };
}

function parseRegistryEventRecord(row: RegistryEventRow): RegistryEventRecord {
  return {
    event_id: row.event_id,
    event_type: row.event_type,
    package_name: row.package_name ?? undefined,
    package_version: row.package_version ?? undefined,
    package_locator: row.package_locator ?? undefined,
    payload: parseJsonObject(row.payload_json),
    created_at: row.created_at,
  };
}

function parseJsonArray<T = string>(value: string): T[] {
  try {
    const parsed = JSON.parse(value) as unknown;
    return Array.isArray(parsed) ? (parsed as T[]) : [];
  } catch {
    return [];
  }
}

function parseJsonObject(value: string): Record<string, unknown> {
  try {
    const parsed = JSON.parse(value) as unknown;
    return parsed !== null && typeof parsed === "object" && !Array.isArray(parsed)
      ? (parsed as Record<string, unknown>)
      : {};
  } catch {
    return {};
  }
}
