import { eq, like, or, sql } from "drizzle-orm";

import { registryDb } from "./db.ts";
import { packages } from "./schema.ts";
import { buildFtsQuery, rankQueryResult } from "./search-document.ts";
import type { SearchPackageRow, SearchResult } from "./types.ts";

export async function applySearchMigrations(db: D1Database): Promise<void> {
  void db;
}

export async function upsertSearchRow(db: D1Database, row: SearchPackageRow): Promise<void> {
  const database = registryDb(db);
  await database
    .insert(packages)
    .values({
      packageName: row.package_name,
      normalizedName: row.normalized_name,
      latestVersion: row.latest_version,
      description: row.description ?? null,
      license: row.license ?? null,
      homepage: row.homepage ?? null,
      repository: row.repository ?? null,
      rootModule: row.root_module ?? null,
      canonicalLocator: row.canonical_locator,
      repoUrl: row.repo_url,
      repoOwner: row.repo_owner,
      repoName: row.repo_name,
      subdir: row.subdir,
      releaseCount: row.release_count,
      updatedAt: row.updated_at,
    })
    .onConflictDoUpdate({
      target: packages.packageName,
      set: {
        normalizedName: row.normalized_name,
        latestVersion: row.latest_version,
        description: row.description ?? null,
        license: row.license ?? null,
        homepage: row.homepage ?? null,
        repository: row.repository ?? null,
        rootModule: row.root_module ?? null,
        canonicalLocator: row.canonical_locator,
        repoUrl: row.repo_url,
        repoOwner: row.repo_owner,
        repoName: row.repo_name,
        subdir: row.subdir,
        releaseCount: row.release_count,
        updatedAt: row.updated_at,
      },
    });

  await database.run(sql`DELETE FROM package_search WHERE package_name = ${row.package_name}`);
  await database.run(sql`
    INSERT INTO package_search (
      package_name,
      description,
      repo_owner,
      repo_name,
      subdir,
      repository
    ) VALUES (
      ${row.package_name},
      ${row.description ?? ""},
      ${row.repo_owner},
      ${row.repo_name},
      ${row.subdir},
      ${row.repository ?? ""}
    )
  `);
}

export async function searchPackages(
  db: D1Database,
  query: string,
  limit: number,
  offset: number,
): Promise<SearchResult[]> {
  const database = registryDb(db);
  const normalizedQuery = query.trim();
  if (normalizedQuery.length === 0) {
    return [];
  }

  const ftsQuery = buildFtsQuery(normalizedQuery);
  const results = new Map<string, SearchResult & { fts_rank: number | null }>();

  if (ftsQuery.length > 0) {
    const ftsRows = await database.all<SearchResult & { fts_rank: number | null }>(sql`
      SELECT
        p.package_name,
        p.normalized_name,
        p.latest_version,
        p.description,
        p.license,
        p.homepage,
        p.repository,
        p.root_module,
        p.canonical_locator,
        p.repo_url,
        p.repo_owner,
        p.repo_name,
        p.subdir,
        p.release_count,
        p.updated_at,
        bm25(package_search) AS fts_rank
      FROM package_search
      JOIN packages p ON p.package_name = package_search.package_name
      WHERE package_search MATCH ${ftsQuery}
    `);

    for (const row of ftsRows) {
      results.set(row.package_name, row);
    }
  }

  const normalizedName = normalizeName(normalizedQuery);
  const directRows = await database
    .select({
      package_name: packages.packageName,
      normalized_name: packages.normalizedName,
      latest_version: packages.latestVersion,
      description: packages.description,
      license: packages.license,
      homepage: packages.homepage,
      repository: packages.repository,
      root_module: packages.rootModule,
      canonical_locator: packages.canonicalLocator,
      repo_url: packages.repoUrl,
      repo_owner: packages.repoOwner,
      repo_name: packages.repoName,
      subdir: packages.subdir,
      release_count: packages.releaseCount,
      updated_at: packages.updatedAt,
      fts_rank: sql<number | null>`NULL`,
    })
    .from(packages)
    .where(
      or(
        eq(packages.normalizedName, normalizedName),
        like(packages.normalizedName, `${normalizedName}%`),
        like(packages.normalizedName, `%${normalizedName}%`),
        eq(sql`lower(${packages.repoOwner})`, normalizedQuery.toLowerCase()),
        eq(sql`lower(${packages.repoName})`, normalizedQuery.toLowerCase()),
      ),
    );

  for (const row of directRows) {
    results.set(row.package_name, row);
  }

  return [...results.values()]
    .sort((left, right) => {
      const leftScore = rankQueryResult(left, normalizedQuery, left.fts_rank);
      const rightScore = rankQueryResult(right, normalizedQuery, right.fts_rank);

      if (leftScore !== rightScore) {
        return rightScore - leftScore;
      }

      return left.package_name.localeCompare(right.package_name);
    })
    .slice(offset, offset + limit)
    .map(({ fts_rank: _ftsRank, ...row }) => row);
}

function normalizeName(value: string): string {
  return value.toLowerCase().replace(/[\s_-]+/g, "");
}
