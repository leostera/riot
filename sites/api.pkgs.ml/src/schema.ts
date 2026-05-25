import { index, integer, primaryKey, sqliteTable, text, uniqueIndex } from "drizzle-orm/sqlite-core";

export const users = sqliteTable("users", {
  userId: text("user_id").primaryKey(),
  githubId: integer("github_id").notNull().unique(),
  githubLogin: text("github_login").notNull().unique(),
  githubLoginLower: text("github_login_lower").notNull().unique(),
  githubName: text("github_name"),
  githubAvatarUrl: text("github_avatar_url"),
  githubEmail: text("github_email"),
  githubEmailVerified: integer("github_email_verified", { mode: "boolean" }).notNull().default(false),
  createdAt: text("created_at").notNull(),
  updatedAt: text("updated_at").notNull(),
});

export const userLogins = sqliteTable("user_logins", {
  githubLoginLower: text("github_login_lower").primaryKey(),
  githubLogin: text("github_login").notNull(),
  userId: text("user_id").notNull(),
  updatedAt: text("updated_at").notNull(),
});

export const oauthStates = sqliteTable("oauth_states", {
  stateId: text("state_id").primaryKey(),
  returnTo: text("return_to").notNull(),
  createdAt: text("created_at").notNull(),
});

export const sessions = sqliteTable(
  "sessions",
  {
    sessionId: text("session_id").primaryKey(),
    userId: text("user_id").notNull(),
    githubLogin: text("github_login").notNull(),
    createdAt: text("created_at").notNull(),
    expiresAt: text("expires_at").notNull(),
  },
  (table) => ({
    userIdIdx: index("idx_sessions_user_id").on(table.userId),
  }),
);

export const sessionHandoffs = sqliteTable(
  "session_handoffs",
  {
    handoffId: text("handoff_id").primaryKey(),
    sessionId: text("session_id").notNull(),
    returnTo: text("return_to").notNull(),
    createdAt: text("created_at").notNull(),
    expiresAt: text("expires_at").notNull(),
  },
  (table) => ({
    sessionIdx: index("idx_session_handoffs_session_id").on(table.sessionId),
    expiresIdx: index("idx_session_handoffs_expires_at").on(table.expiresAt),
  }),
);

export const apiTokens = sqliteTable(
  "api_tokens",
  {
    tokenId: text("token_id").primaryKey(),
    userId: text("user_id").notNull(),
    githubLogin: text("github_login").notNull(),
    name: text("name").notNull(),
    secretHash: text("secret_hash").notNull().unique(),
    capabilitiesJson: text("capabilities_json").notNull(),
    createdAt: text("created_at").notNull(),
    lastUsedAt: text("last_used_at"),
    revokedAt: text("revoked_at"),
  },
  (table) => ({
    userIdIdx: index("idx_api_tokens_user_id").on(table.userId),
    secretHashIdx: index("idx_api_tokens_secret_hash").on(table.secretHash),
  }),
);

export const apiTokenLookups = sqliteTable("api_token_lookups", {
  secretHash: text("secret_hash").primaryKey(),
  tokenId: text("token_id").notNull(),
  userId: text("user_id").notNull(),
  githubLogin: text("github_login").notNull(),
  capabilitiesJson: text("capabilities_json").notNull(),
  revokedAt: text("revoked_at"),
});

export const packageClaims = sqliteTable(
  "package_claims",
  {
    packageName: text("package_name").primaryKey(),
    packageLocator: text("package_locator").notNull(),
    sourceUrl: text("source_url").notNull(),
    packageSubdir: text("package_subdir").notNull(),
    ownerUserId: text("owner_user_id"),
    ownerGithubLogin: text("owner_github_login"),
    claimedAt: text("claimed_at").notNull(),
    updatedAt: text("updated_at").notNull(),
  },
  (table) => ({
    ownerLoginIdx: index("idx_claims_owner_login").on(table.ownerGithubLogin),
  }),
);

export const publishedReleases = sqliteTable(
  "published_releases",
  {
    packageName: text("package_name").notNull(),
    packageVersion: text("package_version").notNull(),
    packageLocator: text("package_locator").notNull(),
    sourceUrl: text("source_url").notNull(),
    packageSubdir: text("package_subdir").notNull(),
    artifactSha256: text("artifact_sha256").notNull(),
    packageDescription: text("package_description"),
    packageLicense: text("package_license"),
    packageHomepage: text("package_homepage"),
    packageRepository: text("package_repository"),
    packageRootModule: text("package_root_module"),
    packageCategoriesJson: text("package_categories_json").notNull(),
    packageKeywordsJson: text("package_keywords_json").notNull(),
    dependenciesJson: text("dependencies_json").notNull(),
    sourceArchiveKey: text("source_archive_key").notNull(),
    manifestKey: text("manifest_key").notNull(),
    publishedAt: text("published_at").notNull(),
    yankedAt: text("yanked_at"),
    yankedByGithubLogin: text("yanked_by_github_login"),
  },
  (table) => ({
    pk: primaryKey({ columns: [table.packageName, table.packageVersion] }),
    packageNameIdx: index("idx_releases_package_name").on(table.packageName),
  }),
);

export const registryEvents = sqliteTable(
  "registry_events",
  {
    sequenceId: integer("sequence_id").primaryKey({ autoIncrement: true }),
    eventId: text("event_id").notNull(),
    eventType: text("event_type").notNull(),
    packageName: text("package_name"),
    packageVersion: text("package_version"),
    packageLocator: text("package_locator"),
    payloadJson: text("payload_json").notNull(),
    createdAt: text("created_at").notNull(),
  },
  (table) => ({
    eventIdUnique: uniqueIndex("registry_events_event_id_unique").on(table.eventId),
    sequenceIdx: index("idx_registry_events_sequence_id").on(table.sequenceId),
    createdAtIdx: index("idx_registry_events_created_at").on(table.createdAt),
    packageIdx: index("idx_registry_events_package").on(
      table.packageName,
      table.packageVersion,
      table.createdAt,
    ),
  }),
);

export const indexReads = sqliteTable(
  "index_reads",
  {
    readId: text("read_id").primaryKey(),
    documentKey: text("document_key").notNull(),
    packageName: text("package_name"),
    riotAgent: text("riot_agent").notNull(),
    readAt: text("read_at").notNull(),
  },
  (table) => ({
    documentIdx: index("idx_index_reads_document").on(table.documentKey, table.readAt),
    packageIdx: index("idx_index_reads_package").on(table.packageName, table.readAt),
    readAtIdx: index("idx_index_reads_read_at").on(table.readAt),
  }),
);

export const packageDownloads = sqliteTable(
  "package_downloads",
  {
    downloadId: text("download_id").primaryKey(),
    packageName: text("package_name").notNull(),
    packageVersion: text("package_version").notNull(),
    artifactSha256: text("artifact_sha256").notNull(),
    sourceArchiveKey: text("source_archive_key").notNull(),
    riotAgent: text("riot_agent").notNull(),
    downloadedAt: text("downloaded_at").notNull(),
  },
  (table) => ({
    packageIdx: index("idx_package_downloads_package").on(
      table.packageName,
      table.packageVersion,
      table.downloadedAt,
    ),
    artifactIdx: index("idx_package_downloads_artifact").on(table.artifactSha256),
    downloadedAtIdx: index("idx_package_downloads_downloaded_at").on(table.downloadedAt),
  }),
);

export const binaryDownloads = sqliteTable(
  "binary_downloads",
  {
    downloadId: text("download_id").primaryKey(),
    binaryName: text("binary_name").notNull(),
    objectKey: text("object_key").notNull(),
    riotAgent: text("riot_agent").notNull(),
    downloadedAt: text("downloaded_at").notNull(),
  },
  (table) => ({
    binaryIdx: index("idx_binary_downloads_binary").on(table.binaryName, table.downloadedAt),
    objectIdx: index("idx_binary_downloads_object").on(table.objectKey),
    downloadedAtIdx: index("idx_binary_downloads_downloaded_at").on(table.downloadedAt),
  }),
);

export const packageReleasesToProcess = sqliteTable(
  "package_releases_to_process",
  {
    releaseId: text("release_id").primaryKey(),
    packageName: text("package_name").notNull(),
    packageVersion: text("package_version").notNull(),
    artifactSha256: text("artifact_sha256").notNull(),
    sourceArchiveKey: text("source_archive_key").notNull(),
    status: text("status").notNull(),
    attemptCount: integer("attempt_count").notNull().default(0),
    nextAttemptAt: text("next_attempt_at").notNull(),
    createdAt: text("created_at").notNull(),
    updatedAt: text("updated_at").notNull(),
    lastAttemptedAt: text("last_attempted_at"),
    leaseExpiresAt: text("lease_expires_at"),
    finishedAt: text("finished_at"),
    statusMessage: text("status_message"),
    payloadJson: text("payload_json").notNull(),
  },
  (table) => ({
    identityUnique: uniqueIndex("package_releases_to_process_identity_unique").on(
      table.packageName,
      table.packageVersion,
      table.artifactSha256,
    ),
    statusIdx: index("idx_package_releases_to_process_status").on(
      table.status,
      table.nextAttemptAt,
      table.updatedAt,
    ),
    packageIdx: index("idx_package_releases_to_process_package").on(
      table.packageName,
      table.packageVersion,
      table.updatedAt,
    ),
  }),
);

export const packagePipelineRuns = sqliteTable(
  "package_pipeline_runs",
  {
    runId: text("run_id").primaryKey(),
    runKind: text("run_kind").notNull(),
    packageName: text("package_name").notNull(),
    packageVersion: text("package_version").notNull(),
    artifactSha256: text("artifact_sha256").notNull(),
    sourceArchiveKey: text("source_archive_key").notNull(),
    runnerKind: text("runner_kind").notNull(),
    status: text("status").notNull(),
    outputPrefix: text("output_prefix").notNull(),
    requestKey: text("request_key").notNull(),
    createdAt: text("created_at").notNull(),
    updatedAt: text("updated_at").notNull(),
    startedAt: text("started_at"),
    finishedAt: text("finished_at"),
    statusMessage: text("status_message"),
    metadataJson: text("metadata_json").notNull(),
  },
  (table) => ({
    identityUnique: uniqueIndex("package_pipeline_runs_identity_unique").on(
      table.packageName,
      table.packageVersion,
      table.artifactSha256,
      table.runKind,
    ),
    packageIdx: index("idx_package_pipeline_runs_package").on(
      table.packageName,
      table.packageVersion,
      table.runKind,
      table.createdAt,
    ),
    statusIdx: index("idx_package_pipeline_runs_status").on(table.status, table.updatedAt),
  }),
);

export const packages = sqliteTable("packages", {
  packageName: text("package_name").primaryKey(),
  normalizedName: text("normalized_name").notNull(),
  latestVersion: text("latest_version").notNull(),
  description: text("description"),
  license: text("license"),
  homepage: text("homepage"),
  repository: text("repository"),
  rootModule: text("root_module"),
  canonicalLocator: text("canonical_locator").notNull(),
  repoUrl: text("repo_url").notNull(),
  repoOwner: text("repo_owner").notNull(),
  repoName: text("repo_name").notNull(),
  subdir: text("subdir").notNull(),
  releaseCount: integer("release_count").notNull(),
  updatedAt: text("updated_at").notNull(),
});
