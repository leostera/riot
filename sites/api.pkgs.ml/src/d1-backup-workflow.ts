import { WorkflowEntrypoint, WorkflowStep } from "cloudflare:workers";
import type { WorkflowEvent } from "cloudflare:workers";

import type { Env } from "./types.ts";

type WorkflowParams = {
  accountId: string;
  databaseId: string;
  bucketPrefix?: string;
};

type D1ExportPayload = {
  output_format: "polling";
  current_bookmark?: string;
};

type D1ExportResult = {
  at_bookmark?: string;
  filename?: string;
  signed_url?: string;
};

type D1ExportResponse = {
  success: boolean;
  errors?: Array<unknown>;
  result?: D1ExportResult;
};

export class D1BackupWorkflow extends WorkflowEntrypoint<Env, WorkflowParams> {
  override async run(
    event: Readonly<WorkflowEvent<WorkflowParams>>,
    step: WorkflowStep,
  ): Promise<void> {
    const accountId = event.payload.accountId;
    const databaseId = event.payload.databaseId;
    const bucketPrefix = event.payload.bucketPrefix ?? "registry-database-backups";

    if (!accountId || !databaseId) {
      throw new Error("Missing accountId or databaseId for D1 backup workflow");
    }

    const token = this.env.D1_REST_API_TOKEN;
    if (!token) {
      throw new Error("D1_REST_API_TOKEN is not configured for D1 backup workflow");
    }
    const backupBucket = this.env.ML_PKGS_BACKUPS;
    if (!backupBucket) {
      throw new Error("ML_PKGS_BACKUPS is not configured for D1 backup workflow");
    }

    const endpoint = `https://api.cloudflare.com/client/v4/accounts/${accountId}/d1/database/${databaseId}/export`;

    const headers = {
      Authorization: `Bearer ${token}`,
      "Content-Type": "application/json",
    };

    const atBookmark = await step.do("Start D1 backup job", async () => {
      const response = await this.callExportApi(endpoint, headers, {
        output_format: "polling",
      });
      if (!response.at_bookmark) {
        throw new Error("D1 export API did not return an in-progress bookmark");
      }

      return response.at_bookmark;
    });

    const downloadedAt = new Date();
    const backupObjectKey = await step.do(
      "Poll export status and fetch backup",
      async () => {
        let bookmark = atBookmark;
        let response: D1ExportResult | null = null;

        for (let attempt = 0; attempt < 30; attempt += 1) {
          const pollResult = await this.callExportApi(endpoint, headers, {
            output_format: "polling",
            current_bookmark: bookmark,
          });

          if (pollResult.signed_url) {
            response = pollResult;
            break;
          }

          if (!pollResult.at_bookmark) {
            throw new Error("D1 export API did not return a follow-up bookmark");
          }

          if (pollResult.at_bookmark === bookmark) {
            await step.sleep("Wait for D1 export artifact to become available", "15 seconds");
          }

          bookmark = pollResult.at_bookmark;
        }

        if (!response) {
          throw new Error("D1 export artifact was not ready before polling timeout");
        }

        const signedUrl = response.signed_url;
        if (!signedUrl) {
          throw new Error("D1 export API returned invalid signed URL payload");
        }

        const dumpResponse = await fetch(signedUrl);
        if (!dumpResponse.ok) {
          throw new Error(`Failed to fetch D1 export artifact: ${dumpResponse.status}`);
        }

        const marker = this.buildManifestKey(
          bucketPrefix,
          accountId,
          databaseId,
          downloadedAt,
          response.filename,
        );

        await backupBucket.put(marker, dumpResponse.body, {
          httpMetadata: { contentType: "application/sql" },
          customMetadata: {
            backup_type: "d1_export",
            account_id: accountId,
            database_id: databaseId,
            started_at: new Date(event.timestamp).toISOString(),
            backup_at: downloadedAt.toISOString(),
            workflow_instance_id: event.instanceId,
            bookmark: atBookmark,
          },
        });

        return marker;
      },
    );

    console.log(`Backed up D1 database ${databaseId} to ML_PKGS_BACKUPS/${backupObjectKey}`);
  }

  private buildManifestKey(
    prefix: string,
    accountId: string,
    databaseId: string,
    downloadedAt: Date,
    filename?: string,
  ): string {
    const date = downloadedAt.toISOString().split("T")[0];
    const time = downloadedAt.toISOString().replace(/[:.]/g, "-");
    if (filename && filename.endsWith(".sql")) {
      return `${prefix}/${accountId}/${databaseId}/${date}/${time}-${filename}`;
    }

    return `${prefix}/${accountId}/${databaseId}/${date}/${time}-riot-registry-backup.sql`;
  }

  private async callExportApi(
    endpoint: string,
    headers: HeadersInit,
    payload: D1ExportPayload,
  ): Promise<D1ExportResult> {
    const response = await fetch(endpoint, {
      method: "POST",
      headers,
      body: JSON.stringify(payload),
    });

    const body = (await response.json()) as D1ExportResponse;
    if (!response.ok || body.success === false) {
      throw new Error(`D1 export API request failed: ${response.status}`);
    }

    if (!body.result) {
      throw new Error("D1 export API returned no result");
    }

    return body.result;
  }
}
