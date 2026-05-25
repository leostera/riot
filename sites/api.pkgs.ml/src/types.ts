export interface Env {
  ML_PKGS_CDN: R2Bucket;
  ML_PKGS_BACKUPS?: R2Bucket;
  SEARCH_DB: D1Database;
  PACKAGE_PUBLISHED_QUEUE: Queue<PackagePublishedEvent>;
  PACKAGE_INDEXED_QUEUE: Queue<PackageIndexedEvent>;
  PUBLICATION_COORDINATOR: DurableObjectNamespace;
  REGISTRY_D1_BACKUP?: Workflow<{ accountId: string; databaseId: string; bucketPrefix?: string }>;
  CDN_BASE_URL?: string;
  INDEX_BASE_URL?: string;
  INDEX_BASE_PATH?: string;
  INDEX_ROUTE_PATH?: string;
  VIEWS_BASE_PATH?: string;
  GITHUB_API_BASE_URL?: string;
  GITHUB_OAUTH_CLIENT_ID?: string;
  GITHUB_OAUTH_CLIENT_SECRET?: string;
  ROOT_AUTH_TOKEN?: string;
  AUTH_COOKIE_DOMAIN?: string;
  PKGS_WEB_BASE_URL?: string;
  PLAY_WEB_BASE_URL?: string;
  D1_REST_API_TOKEN?: string;
  D1_BACKUP_ACCOUNT_ID?: string;
  D1_BACKUP_DATABASE_ID?: string;
  D1_BACKUP_BUCKET_PREFIX?: string;
  D1_BACKUP_ENABLED?: string;
}

export interface RegistryConfig {
  cdnBaseUrl: string;
  indexBaseUrl: string;
  indexBasePath: string;
  indexRoutePath: string;
  viewsBasePath: string;
  authCookieDomain: string;
  pkgsWebBaseUrl: string;
  playWebBaseUrl: string;
}

export type IndexConfig = RegistryConfig;

export interface PackagePublicationManifest {
  package_locator: string;
  source_url: string;
  package_subdir: string;
  artifact_sha256: string;
  package_name: string;
  package_version: string;
  package_public: boolean;
  package_description?: string;
  package_license?: string;
  package_homepage?: string;
  package_repository?: string;
  package_root_module?: string;
  package_categories?: string[];
  package_keywords?: string[];
  dependencies: Array<Record<string, unknown>>;
  source_archive_key: string;
  manifest_key: string;
  materialized_at: string;
}

export interface PackageClaimRecord {
  package_name: string;
  package_locator: string;
  source_url: string;
  package_subdir: string;
  owner_user_id?: string;
  owner_github_login?: string;
  claimed_at: string;
  updated_at: string;
}

export interface UserRecord {
  user_id: string;
  github_id: number;
  github_login: string;
  github_name?: string;
  github_avatar_url?: string;
  github_email?: string;
  github_email_verified?: boolean;
  created_at: string;
  updated_at: string;
}

export interface UserLoginRecord {
  github_login: string;
  user_id: string;
  updated_at: string;
}

export interface OAuthStateRecord {
  state_id: string;
  return_to: string;
  created_at: string;
}

export interface SessionRecord {
  session_id: string;
  user_id: string;
  github_login: string;
  created_at: string;
  expires_at: string;
}

export interface SessionHandoffRecord {
  handoff_id: string;
  session_id: string;
  return_to: string;
  created_at: string;
  expires_at: string;
}

export type ApiTokenCapability = "publish";

export interface ApiTokenRecord {
  token_id: string;
  user_id: string;
  github_login: string;
  name: string;
  secret_hash: string;
  capabilities: ApiTokenCapability[];
  created_at: string;
  last_used_at?: string;
  revoked_at?: string;
}

export interface ApiTokenLookupRecord {
  token_id: string;
  user_id: string;
  github_login: string;
  capabilities: ApiTokenCapability[];
  revoked_at?: string;
}

export type RegistryEventType =
  | "package.submitted"
  | "package.verified"
  | "package.indexed"
  | "package.searchable"
  | "package.published"
  | "package.yanked"
  | "package.processing.queued"
  | "package.processing.started"
  | "package.processing.requeued"
  | "package.processing.blocked"
  | "package.processing.finished"
  | "package.docs.staged"
  | "package.docs.generated"
  | "package.docs.failed"
  | "package.build.staged"
  | "package.build.verified"
  | "package.build.failed";

export interface RegistryEventRecord {
  event_id: string;
  event_type: RegistryEventType;
  package_name?: string;
  package_version?: string;
  package_locator?: string;
  payload: Record<string, unknown>;
  created_at: string;
}

export interface IndexReadRecord {
  read_id: string;
  document_key: string;
  package_name?: string;
  riot_agent: string;
  read_at: string;
}

export interface PackageDownloadRecord {
  download_id: string;
  package_name: string;
  package_version: string;
  artifact_sha256: string;
  source_archive_key: string;
  riot_agent: string;
  downloaded_at: string;
}

export type BinaryDownloadName = "riot" | "ocaml";

export interface BinaryDownloadRecord {
  download_id: string;
  binary_name: BinaryDownloadName;
  object_key: string;
  riot_agent: string;
  downloaded_at: string;
}

export type PackageReleaseToProcessStatus = "pending" | "processing" | "blocked" | "finished";

export interface PackageReleaseToProcessRecord {
  release_id: string;
  package_name: string;
  package_version: string;
  artifact_sha256: string;
  source_archive_key: string;
  status: PackageReleaseToProcessStatus;
  attempt_count: number;
  next_attempt_at: string;
  created_at: string;
  updated_at: string;
  last_attempted_at?: string;
  lease_expires_at?: string;
  finished_at?: string;
  status_message?: string;
  payload: PackagePublishedEvent;
}

export type PackagePipelineRunKind = "docs" | "build" | "test" | "fmt" | "fix" | "bench";

export type PackagePipelineRunStatus =
  | "staged"
  | "running"
  | "succeeded"
  | "failed"
  | "blocked";

export type PackagePipelineRunnerKind = "cloudflare-container" | "cloudflare-sandbox";

export type PackagePipelineStepKind =
  | "download"
  | "unpack"
  | "install-riot"
  | "upgrade-riot"
  | "generate-docs"
  | "build-package"
  | "upload"
  | "upload-report";

export interface PackagePipelineRunRecord {
  run_id: string;
  run_kind: PackagePipelineRunKind;
  package_name: string;
  package_version: string;
  artifact_sha256: string;
  source_archive_key: string;
  runner_kind: PackagePipelineRunnerKind;
  status: PackagePipelineRunStatus;
  output_prefix: string;
  request_key: string;
  created_at: string;
  updated_at: string;
  started_at?: string;
  finished_at?: string;
  status_message?: string;
  metadata: Record<string, unknown>;
}

export interface PackagePipelineRequestStep {
  kind: PackagePipelineStepKind;
  detail: string;
}

export interface PackagePipelineRequestRunner {
  kind: "cloudflare-container" | "cloudflare-sandbox";
  status: "pending_runner";
  notes: string[];
}

interface PackagePipelineRequestBase {
  run_id: string;
  run_kind: PackagePipelineRunKind;
  package_name: string;
  package_version: string;
  artifact_sha256: string;
  source_archive_key: string;
  source_archive_url: string;
  output_prefix: string;
  riot_install_url: string;
  riot_release_metadata_url: string;
  command: string[];
  runner: PackagePipelineRequestRunner;
  steps: PackagePipelineRequestStep[];
  created_at: string;
}

export interface DocsBuildRequest extends PackagePipelineRequestBase {
  run_kind: "docs";
  public_docs_url: string;
  result_key: string;
  logs_key: string;
}

export interface PackageBuildRequest extends PackagePipelineRequestBase {
  run_kind: "build";
  result_key: string;
  logs_key: string;
}

export interface RegistryEventsDocument {
  limit?: number;
  after?: string;
  events: RegistryEventRecord[];
}

export interface SessionResponse {
  authenticated: boolean;
  user?: UserRecord;
}

export interface AuthenticatedActorRoot {
  kind: "root";
}

export interface AuthenticatedActorUser {
  kind: "user";
  userId: string;
  githubLogin: string;
  tokenId?: string;
}

export type AuthenticatedActor = AuthenticatedActorRoot | AuthenticatedActorUser;

export interface PublishedReleaseRecord {
  package_name: string;
  package_version: string;
  package_locator: string;
  source_url: string;
  package_subdir: string;
  artifact_sha256: string;
  package_description?: string;
  package_license?: string;
  package_homepage?: string;
  package_repository?: string;
  package_root_module?: string;
  package_categories?: string[];
  package_keywords?: string[];
  dependencies: Array<Record<string, unknown>>;
  source_archive_key: string;
  manifest_key: string;
  published_at: string;
  yanked_at?: string;
  yanked_by_github_login?: string;
}

export interface PackagePublishedEvent extends PublishedReleaseRecord {
  type: "package.published";
}

export interface PackageIndexedEvent {
  type: "package.indexed";
  package_name: string;
  package_version: string;
  package_locator: string;
  artifact_sha256: string;
  package_index_key: string;
  package_index_url: string;
  latest: string;
  indexed_at: string;
}

export interface IndexConfigDocument {
  schema_version: 1;
  kind: "sparse";
  package_path_strategy: "cargo-lowercase-v1";
  index_base_url: string;
  artifact_base_url: string;
}

export interface IndexedPackageRelease {
  version: string;
  published_at: string;
  canonical_locator: string;
  repo_url: string;
  subdir: string;
  artifact_sha256: string;
  description?: string;
  license?: string;
  homepage?: string;
  repository?: string;
  root_module?: string;
  categories?: string[];
  keywords?: string[];
  manifest_key: string;
  source_key: string;
  dependencies: Array<Record<string, unknown>>;
  yanked?: boolean;
  yanked_at?: string;
  yanked_by_github_login?: string;
}

export interface PackageIndexDocument {
  schema_version: 1;
  name: string;
  latest: string;
  updated_at: string;
  releases: IndexedPackageRelease[];
}

export interface SearchPackageRow {
  package_name: string;
  normalized_name: string;
  latest_version: string;
  description: string | null;
  license: string | null;
  homepage: string | null;
  repository: string | null;
  root_module: string | null;
  canonical_locator: string;
  repo_url: string;
  repo_owner: string;
  repo_name: string;
  subdir: string;
  release_count: number;
  updated_at: string;
}

export interface SearchResult extends SearchPackageRow {}

export interface PackageOverviewDocument {
  schema_version: 1;
  package_name: string;
  latest_version: string;
  updated_at: string;
  published_at: string;
  description?: string;
  license?: string;
  homepage?: string;
  repository?: string;
  root_module?: string;
  canonical_locator: string;
  repo_url: string;
  subdir: string;
  source_key: string;
  manifest_key: string;
  artifact_sha256: string;
  owner_github_login: string;
  owner_github_avatar_url?: string;
  release_count: number;
  dependency_count: number;
  dependent_count: number;
  download_count: number;
  categories: string[];
  keywords: string[];
  yanked: boolean;
  yanked_at?: string;
  yanked_by_github_login?: string;
}

export interface PackageExampleSummary {
  name: string;
  path: string;
}

export interface PackageExampleDocument extends PackageExampleSummary {
  source_code: string;
}

export interface PackageExamplesDocument {
  schema_version: 1;
  package_name: string;
  package_version: string;
  source_key: string;
  examples: PackageExampleDocument[];
}

export interface PackageRelationDependency {
  package_name: string;
  requirement: string;
}

export interface PackageRelationDependent {
  package_name: string;
  latest_version: string;
  requirement: string;
}

export interface PackageRelationsDocument {
  schema_version: 1;
  package_name: string;
  updated_at: string;
  dependencies: PackageRelationDependency[];
  dependents: PackageRelationDependent[];
}

export interface PackageDailyDownloadPoint {
  date: string;
  download_count: number;
}

export interface PackageStackedDownloadSeries {
  key: string;
  label: string;
  is_latest: boolean;
  is_other: boolean;
  total_downloads: number;
  daily_downloads: PackageDailyDownloadPoint[];
}

export interface PackageVersionDownloadPoint {
  version: string;
  published_at: string;
  download_count: number;
  is_latest: boolean;
}

export interface PackageDownloadsDocument {
  schema_version: 1;
  package_name: string;
  latest_version: string;
  generated_at: string;
  window_days: number;
  total_downloads: number;
  daily_downloads: PackageDailyDownloadPoint[];
  stacked_downloads: PackageStackedDownloadSeries[];
  version_downloads: PackageVersionDownloadPoint[];
}

export interface PackageReadmeDocument {
  schema_version: 1;
  package_name: string;
  package_version: string;
  source_key: string;
  readme_path: string;
  readme_markdown: string;
}

export interface WebPackageListItem {
  package_name: string;
  latest_version: string;
  description?: string;
  license?: string;
  owner_github_login: string;
  owner_github_avatar_url?: string;
  categories: string[];
  updated_at: string;
  repo_url: string;
  repository?: string;
  subdir: string;
  release_count: number;
  yanked_release_count: number;
  package_path: string;
  releases: WebPackageReleaseListItem[];
}

export interface WebPackageReleaseListItem {
  version: string;
  published_at: string;
  yanked: boolean;
  yanked_at?: string;
  yanked_by_github_login?: string;
  package_path: string;
}

export interface RecentPackagesDocument {
  schema_version: 1;
  generated_at: string;
  packages: WebPackageListItem[];
}

export interface PopularPackagesDocument {
  schema_version: 1;
  generated_at: string;
  packages: Array<WebPackageListItem & { dependent_count: number; release_count: number }>;
}

export interface CategorySummary {
  name: string;
  slug: string;
  package_count: number;
  packages: string[];
}

export interface CategoriesIndexDocument {
  schema_version: 1;
  generated_at: string;
  categories: CategorySummary[];
}

export interface OwnerPackagesDocument {
  schema_version: 1;
  generated_at: string;
  owner_github_login: string;
  owner_github_avatar_url?: string;
  package_count: number;
  latest_update_at?: string;
  packages: WebPackageListItem[];
}

export interface RegistryStatsSummaryDocument {
  schema_version: 1;
  generated_at: string;
  total_package_downloads: number;
  total_riot_downloads: number;
  total_ocaml_downloads: number;
  total_packages: number;
  total_versions: number;
  total_users: number;
}

export interface RegistryStatsActivityPoint {
  date: string;
  package_downloads: number;
  riot_downloads: number;
  ocaml_downloads: number;
  index_reads: number;
  releases_published: number;
}

export type RegistryStatsWindowKey = "all" | "year" | "30d" | "7d";

export interface RegistryStatsWindowOption {
  key: RegistryStatsWindowKey;
  label: string;
}

export type RegistryStatsMetricKey =
  | "package_downloads"
  | "riot_downloads"
  | "ocaml_downloads"
  | "index_reads"
  | "releases_published";

export interface RegistryStatsMetricSeries {
  key: RegistryStatsMetricKey;
  label: string;
  total: number;
  color: string;
  points: RegistryStatsActivityPoint[];
}

export interface StatsTopPackage {
  package_name: string;
  latest_version: string;
  description?: string;
  package_path: string;
  download_count: number;
}

export interface StatsLatestRelease {
  package_name: string;
  package_version: string;
  package_path: string;
  published_at: string;
}

export interface RegistryStatsDashboardDocument {
  schema_version: 1;
  generated_at: string;
  window: RegistryStatsWindowKey;
  window_label: string;
  window_days: number;
  available_windows: RegistryStatsWindowOption[];
  summary: RegistryStatsSummaryDocument & {
    total_index_reads: number;
    mean_package_downloads_per_package: number;
  };
  daily_activity: RegistryStatsActivityPoint[];
  metrics: RegistryStatsMetricSeries[];
  top_packages: StatsTopPackage[];
  latest_releases: StatsLatestRelease[];
}

export interface PublishedPackageRelease {
  artifactSha256: string;
  sourceKey: string;
  manifestKey: string;
  manifestCreated: boolean;
  sourceCreated: boolean;
  packageName: string;
  packageVersion: string;
  claimKey: string;
  releaseKey: string;
  claimCreated: boolean;
  releaseCreated: boolean;
  indexChanged: boolean;
}
