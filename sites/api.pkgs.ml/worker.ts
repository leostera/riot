export { PublicationCoordinator } from "./src/publication-coordinator.ts";

import { handleRequest } from "./src/routes.ts";
import { D1BackupWorkflow } from "./src/d1-backup-workflow.ts";
import type { Env } from "./src/types.ts";

function isBackupEnabled(env: Env): boolean {
  return (env.D1_BACKUP_ENABLED ?? "false").toLowerCase() === "true";
}

export default {
  async fetch(request, env, ctx): Promise<Response> {
    return await handleRequest(request, env, ctx);
  },

  async scheduled(_controller: ScheduledController, env: Env, _ctx: ExecutionContext): Promise<void> {
    if (!isBackupEnabled(env)) {
      return;
    }

    const accountId = env.D1_BACKUP_ACCOUNT_ID;
    const databaseId = env.D1_BACKUP_DATABASE_ID;
    if (!accountId || !databaseId) {
      console.error("D1 backup workflow is enabled but D1_BACKUP_ACCOUNT_ID/D1_BACKUP_DATABASE_ID are missing");
      return;
    }

    try {
      const instance = await env.REGISTRY_D1_BACKUP?.create({
        params: {
          accountId,
          databaseId,
          bucketPrefix: env.D1_BACKUP_BUCKET_PREFIX ?? "registry-database-backups",
        },
      });
      console.log(`Started D1 backup workflow: ${instance?.id}`);
    } catch (error) {
      console.error("Failed to trigger D1 backup workflow", error);
    }
  },
} satisfies ExportedHandler<Env>;

export { D1BackupWorkflow };
