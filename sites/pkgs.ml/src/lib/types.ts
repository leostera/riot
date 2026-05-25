export interface SearchResult {
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

export interface SearchResponse {
  query: string;
  count: number;
  results: SearchResult[];
}

export interface RegistryServiceRootDocument {
  service: string;
  routes: Record<string, string>;
  cdn_routes: Record<string, string>;
  legacy_routes: Record<string, string>;
  cdn_base_url: string;
  index_base_url: string;
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

export interface PackageFact {
  label: string;
  value: string;
  href?: string;
  code?: boolean;
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

export interface SessionResponse {
  authenticated: boolean;
  user?: UserRecord;
}

export interface ApiTokenSummary {
  token_id: string;
  user_id: string;
  github_login: string;
  name: string;
  capabilities: string[];
  created_at: string;
  last_used_at?: string;
  revoked_at?: string;
}

export interface ApiTokensResponse {
  user: UserRecord;
  tokens: ApiTokenSummary[];
}

export interface CreateApiTokenResponse {
  plaintext_token: string;
  token: ApiTokenSummary;
}

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

export interface PopularPackageListItem extends WebPackageListItem {
  dependent_count: number;
  release_count: number;
}

export interface PopularPackagesDocument {
  schema_version: 1;
  generated_at: string;
  packages: PopularPackageListItem[];
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

export type RegistryEventType =
  | "package.submitted"
  | "package.verified"
  | "package.indexed"
  | "package.searchable"
  | "package.published"
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

export interface RegistryEventsDocument {
  limit?: number;
  after?: string;
  events: RegistryEventRecord[];
}
