import { describe, expect, test } from "bun:test";
import { Buffer } from "node:buffer";

import {
  FakeD1Database,
  FakeExecutionContext,
  FakeQueue,
  FakeR2Bucket,
} from "../../api.pkgs.ml/tests/helpers.ts";
import type { PackagePublishedEvent } from "../../api.pkgs.ml/src/types.ts";
import type { DocsPipelineProcessResult, PackagePipelineExecutor } from "../src/pipeline-types.ts";
import worker from "../src/main.ts";

interface TestEnv {
  ASSETS: { fetch(request: Request): Promise<Response> | Response };
  ML_PKGS_CDN: R2Bucket;
  SEARCH_DB: D1Database;
  PACKAGE_PROCESSING_QUEUE: Queue<unknown>;
  PIPELINE_EXECUTOR?: PackagePipelineExecutor;
}

describe("docs.pkgs worker", () => {
  test("root redirects to pkgs.ml", async () => {
    const env = makeEnv();
    const response = await worker.fetch(new Request("https://docs.pkgs.ml/"), env);

    expect(response.status).toBe(302);
    expect(response.headers.get("location")).toBe("https://pkgs.ml/");
  });

  test("package docs route redirects to trailing slash", async () => {
    const env = makeEnv();
    const response = await worker.fetch(new Request("https://docs.pkgs.ml/p/std/0.1.0"), env);

    expect(response.status).toBe(308);
    expect(response.headers.get("location")).toBe("https://docs.pkgs.ml/p/std/0.1.0/");
  });

  test("package docs route serves generated docs from R2", async () => {
    const env = makeEnv();
    const bucket = env.ML_PKGS_CDN as unknown as FakeR2Bucket;
    await bucket.put("docs/std/0.1.0/index.html", "<h1>std docs</h1>", {
      httpMetadata: { contentType: "text/html; charset=utf-8" },
    });

    const response = await worker.fetch(new Request("https://docs.pkgs.ml/p/std/0.1.0/"), env);

    expect(response.status).toBe(200);
    expect(response.headers.get("content-type")).toContain("text/html");
    expect(await response.text()).toContain("std docs");
  });

  test("queue consumer records published releases for timer-driven processing", async () => {
    const env = makeEnv();
    const db = env.SEARCH_DB as unknown as FakeD1Database;
    const ctx = new FakeExecutionContext();
    let acked = false;

    await enqueuePublishedRelease(env, ctx, {
      ack: () => {
        acked = true;
      },
    });

    expect(acked).toBe(true);

    const rows = await db
      .prepare(
        "SELECT status, attempt_count, status_message FROM package_releases_to_process WHERE package_name = ? AND package_version = ?",
      )
      .bind("std", "0.1.0")
      .all<{
        status: string;
        attempt_count: number;
        status_message: string;
      }>();

    expect(rows.results).toEqual([
      {
        status: "pending",
        attempt_count: 0,
        status_message: "Queued for package post-publish processing.",
      },
    ]);

    const events = await db
      .prepare(
        "SELECT event_type FROM registry_events WHERE package_name = ? AND package_version = ? ORDER BY sequence_id",
      )
      .bind("std", "0.1.0")
      .all<{ event_type: string }>();

    expect(events.results.map((event) => event.event_type)).toEqual([
      "package.processing.queued",
    ]);
  });

  test("scheduled processing executes docs and build runs for queued releases", async () => {
    const env = makeEnv({
      PIPELINE_EXECUTOR: new FakePipelineExecutor({
        docs: {
          success: true,
          exit_code: 0,
          stdout: "generated docs",
          stderr: "",
          duration_ms: 120,
          command: ["riot", "doc", "--release", "-p", "std"],
          output_dir: "/tmp/workspace/_build/doc/std/0.1.0",
          files: [
            {
              path: "index.html",
              content_base64: Buffer.from("<h1>std docs</h1>").toString("base64"),
              content_type: "text/html; charset=utf-8",
            },
          ],
        },
        build: {
          success: true,
          exit_code: 0,
          stdout: "build ok",
          stderr: "",
          duration_ms: 90,
          command: ["riot", "build", "-p", "std"],
        },
      }),
    });
    const bucket = env.ML_PKGS_CDN as unknown as FakeR2Bucket;
    const db = env.SEARCH_DB as unknown as FakeD1Database;
    const ctx = new FakeExecutionContext();

    await enqueuePublishedRelease(env, ctx);
    await runScheduled(env, ctx);
    await drainProcessingQueue(env, ctx);

    const rows = await db
      .prepare(
        "SELECT run_kind, status, output_prefix, request_key, status_message FROM package_pipeline_runs WHERE package_name = ? AND package_version = ? ORDER BY run_kind",
      )
      .bind("std", "0.1.0")
      .all<{
        run_kind: string;
        status: string;
        output_prefix: string;
        request_key: string;
        status_message: string;
      }>();

    expect(rows.results).toHaveLength(2);
    expect(rows.results[0]).toMatchObject({
      run_kind: "build",
      status: "succeeded",
      output_prefix: "pipelines/builds/std/0.1.0/deadbeef/",
      request_key: "pipelines/builds/std/0.1.0/deadbeef/request.json",
    });
    expect(rows.results[1]).toMatchObject({
      run_kind: "docs",
      status: "succeeded",
      output_prefix: "docs/std/0.1.0/",
      request_key: "pipelines/docs/std/0.1.0/deadbeef/request.json",
    });

    const releaseRows = await db
      .prepare(
        "SELECT status, attempt_count, finished_at, status_message FROM package_releases_to_process WHERE package_name = ? AND package_version = ?",
      )
      .bind("std", "0.1.0")
      .all<{
        status: string;
        attempt_count: number;
        finished_at: string | null;
        status_message: string;
      }>();

    expect(releaseRows.results).toHaveLength(1);
    expect(releaseRows.results[0]).toMatchObject({
      status: "finished",
      attempt_count: 1,
      status_message: "Package docs and build verification completed successfully.",
    });
    expect(releaseRows.results[0]?.finished_at).not.toBeNull();

    const buildRequestPayload = await bucket.text("pipelines/builds/std/0.1.0/deadbeef/request.json");
    expect(buildRequestPayload).not.toBeNull();
    expect(buildRequestPayload).toContain('"run_kind": "build"');
    expect(buildRequestPayload).toContain('"command": [');
    expect(buildRequestPayload).toContain('"riot"');
    expect(buildRequestPayload).toContain('"build"');
    expect(buildRequestPayload).toContain('"result_key": "pipelines/builds/std/0.1.0/deadbeef/result.json"');
    expect(buildRequestPayload).toContain('"logs_key": "pipelines/builds/std/0.1.0/deadbeef/build.log"');

    const docsRequestPayload = await bucket.text("pipelines/docs/std/0.1.0/deadbeef/request.json");
    expect(docsRequestPayload).not.toBeNull();
    expect(docsRequestPayload).toContain('"run_kind": "docs"');
    expect(docsRequestPayload).toContain('"riot_install_url": "https://get.riot.ml"');
    expect(docsRequestPayload).toContain('"riot_release_metadata_url": "https://cdn.pkgs.ml/riot/latest.json"');
    expect(docsRequestPayload).toContain('"install-riot"');

    expect(await bucket.text("docs/std/0.1.0/index.html")).toBe("<h1>std docs</h1>");
    expect(await bucket.text("pipelines/builds/std/0.1.0/deadbeef/result.json")).toContain('"success": true');
    expect(await bucket.text("pipelines/builds/std/0.1.0/deadbeef/build.log")).toContain("build ok");

    const events = await db
      .prepare(
        "SELECT event_type FROM registry_events WHERE package_name = ? AND package_version = ?",
      )
      .bind("std", "0.1.0")
      .all<{ event_type: string }>();

    expect(events.results.map((event) => event.event_type).sort()).toEqual([
      "package.build.verified",
      "package.docs.generated",
      "package.processing.finished",
      "package.processing.queued",
      "package.processing.started",
    ]);
  });

  test("scheduled processing requeues failed releases", async () => {
    const env = makeEnv({
      PIPELINE_EXECUTOR: new FakePipelineExecutor({
        docs: {
          success: false,
          exit_code: 1,
          stdout: "",
          stderr: "riot doc exploded",
          duration_ms: 50,
          command: ["riot", "doc", "--release", "-p", "std"],
          output_dir: "/tmp/workspace/_build/doc/std/0.1.0",
          files: [],
        },
      }),
    });
    const db = env.SEARCH_DB as unknown as FakeD1Database;
    const ctx = new FakeExecutionContext();

    await enqueuePublishedRelease(env, ctx);
    await runScheduled(env, ctx);
    await drainProcessingQueue(env, ctx);

    const release = await db
      .prepare(
        "SELECT status, attempt_count, next_attempt_at, finished_at, status_message FROM package_releases_to_process WHERE package_name = ? AND package_version = ?",
      )
      .bind("std", "0.1.0")
      .first<{
        status: string;
        attempt_count: number;
        next_attempt_at: string;
        finished_at: string | null;
        status_message: string;
      }>();

    expect(release).not.toBeNull();
    expect(release?.status).toBe("pending");
    expect(release?.attempt_count).toBe(1);
    expect(release?.finished_at).toBeNull();
    expect(release?.status_message).toContain("Release processing failed and was requeued");

    const events = await db
      .prepare(
        "SELECT event_type FROM registry_events WHERE package_name = ? AND package_version = ? ORDER BY sequence_id",
      )
      .bind("std", "0.1.0")
      .all<{ event_type: string }>();

    expect(events.results.map((row) => row.event_type)).toEqual([
      "package.processing.queued",
      "package.processing.started",
      "package.docs.failed",
      "package.processing.requeued",
    ]);
  });

  test("scheduled processing blocks a release after the third failure", async () => {
    const env = makeEnv({
      PIPELINE_EXECUTOR: new FakePipelineExecutor({
        docs: {
          success: false,
          exit_code: 1,
          stdout: "",
          stderr: "riot doc exploded",
          duration_ms: 50,
          command: ["riot", "doc", "--release", "-p", "std"],
          output_dir: "/tmp/workspace/_build/doc/std/0.1.0",
          files: [],
        },
      }),
    });
    const db = env.SEARCH_DB as unknown as FakeD1Database;
    const ctx = new FakeExecutionContext();

    await enqueuePublishedRelease(env, ctx);
    await runScheduled(env, ctx);
    await drainProcessingQueue(env, ctx);
    await db.exec("UPDATE package_releases_to_process SET next_attempt_at = '1970-01-01T00:00:00.000Z'");
    await runScheduled(env, ctx);
    await drainProcessingQueue(env, ctx);
    await db.exec("UPDATE package_releases_to_process SET next_attempt_at = '1970-01-01T00:00:00.000Z'");
    await runScheduled(env, ctx);
    await drainProcessingQueue(env, ctx);

    const release = await db
      .prepare(
        "SELECT status, attempt_count, finished_at, status_message FROM package_releases_to_process WHERE package_name = ? AND package_version = ?",
      )
      .bind("std", "0.1.0")
      .first<{
        status: string;
        attempt_count: number;
        finished_at: string | null;
        status_message: string;
      }>();

    expect(release).not.toBeNull();
    expect(release?.status).toBe("blocked");
    expect(release?.attempt_count).toBe(3);
    expect(release?.finished_at).not.toBeNull();
    expect(release?.status_message).toContain("failed three times and is now blocked");

    const events = await db
      .prepare(
        "SELECT event_type FROM registry_events WHERE package_name = ? AND package_version = ? ORDER BY sequence_id",
      )
      .bind("std", "0.1.0")
      .all<{ event_type: string }>();

    expect(events.results.map((row) => row.event_type)).toEqual([
      "package.processing.queued",
      "package.processing.started",
      "package.docs.failed",
      "package.processing.requeued",
      "package.processing.started",
      "package.docs.failed",
      "package.processing.requeued",
      "package.processing.started",
      "package.docs.failed",
      "package.processing.blocked",
    ]);
  });

  test("missing docs route surfaces queued pipeline status", async () => {
    const env = makeEnv();
    const ctx = new FakeExecutionContext();

    await enqueuePublishedRelease(env, ctx);

    const response = await worker.fetch(new Request("https://docs.pkgs.ml/p/std/0.1.0/"), env);

    expect(response.status).toBe(404);
    expect(await response.text()).toContain("Current release-processing status: pending.");
  });
});

function makeEnv(overrides: Partial<TestEnv> = {}): TestEnv {
  return {
    ASSETS: {
      fetch: async () =>
        new Response("<html><body>landing</body></html>", {
          headers: {
            "content-type": "text/html; charset=utf-8",
          },
        }),
    },
    ML_PKGS_CDN: new FakeR2Bucket() as unknown as R2Bucket,
    SEARCH_DB: new FakeD1Database() as unknown as D1Database,
    PACKAGE_PROCESSING_QUEUE: new FakeQueue() as unknown as Queue<unknown>,
    ...overrides,
  };
}

async function enqueuePublishedRelease(
  env: TestEnv,
  ctx: FakeExecutionContext,
  callbacks: {
    ack?: () => void;
  } = {},
): Promise<void> {
  const event = makePublishedEvent();
  await worker.queue?.(
    {
      queue: "riot-package-published",
      messages: [
        {
          id: "msg-1",
          timestamp: new Date(),
          attempts: 1,
          body: event,
          ack: callbacks.ack ?? (() => {}),
          retry: () => {
            throw new Error("retry should not be called");
          },
        },
      ],
    } as unknown as MessageBatch<PackagePublishedEvent>,
    env,
    ctx,
  );
}

async function runScheduled(env: TestEnv, ctx: FakeExecutionContext): Promise<void> {
  await worker.scheduled?.(
    {
      cron: "* * * * *",
      scheduledTime: Date.now(),
    } as ScheduledController,
    env,
    ctx,
  );
  await ctx.drain();
}

async function drainProcessingQueue(env: TestEnv, ctx: FakeExecutionContext): Promise<void> {
  const queue = env.PACKAGE_PROCESSING_QUEUE as unknown as FakeQueue;
  if (queue.messages.length === 0) {
    return;
  }

  const messages = queue.messages.splice(0, queue.messages.length);
  await worker.queue?.(
    {
      queue: "riot-package-processing",
      messages: messages.map((body, index) => ({
        id: `process-${index}`,
        timestamp: new Date(),
        attempts: 1,
        body,
        ack: () => {},
        retry: () => {
          throw new Error("retry should not be called");
        },
      })),
    } as unknown as MessageBatch<
      | PackagePublishedEvent
      | {
          kind: "process_release";
          release_id: string;
          attempt_count: number;
          payload: PackagePublishedEvent;
        }
    >,
    env as never,
    ctx,
  );
  await ctx.drain();
}

function makePublishedEvent(): PackagePublishedEvent {
  return {
    type: "package.published",
    package_name: "std",
    package_version: "0.1.0",
    package_locator: "github.com/leostera/riot/packages/std",
    source_url: "https://github.com/leostera/riot",
    package_subdir: "packages/std",
    artifact_sha256: "deadbeef",
    package_description: "Std library",
    package_license: "Apache-2.0",
    package_homepage: "https://riot.ml",
    package_repository: "https://github.com/leostera/riot",
    package_root_module: "Std",
    package_categories: [],
    package_keywords: [],
    dependencies: [],
    source_archive_key: "sources/std/0.1.0/deadbeef.tar.gz",
    manifest_key: "manifests/std/0.1.0/deadbeef.json",
    published_at: "2026-04-04T05:00:00.000Z",
  };
}

class FakePipelineExecutor implements PackagePipelineExecutor {
  constructor(private readonly result: DocsPipelineProcessResult) {}

  async processRelease(): Promise<DocsPipelineProcessResult> {
    return this.result;
  }
}
