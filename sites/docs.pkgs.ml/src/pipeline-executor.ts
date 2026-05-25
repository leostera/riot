import { Buffer } from "node:buffer";
import { Sandbox, getSandbox } from "@cloudflare/sandbox";
import type {
  DocsPipelineProcessResult,
  GeneratedDocsFile,
  PackagePipelineCommandResult,
  PackagePipelineExecutor,
  PackagePipelineProcessRequest,
} from "./pipeline-types.ts";

export type {
  DocsPipelineProcessResult,
  PackagePipelineExecutor,
  PackagePipelineProcessRequest,
} from "./pipeline-types.ts";

export interface SandboxExecutorEnv {
  DOCS_PIPELINE_CONTAINER?: DurableObjectNamespace;
}

const WORKSPACE_ROOT = "/workspace";
const DEFAULT_TOOLCHAIN_VERSION = "5.5.0-riot.3";
const DEFAULT_TARGET = "x86_64-unknown-linux-gnu";
const RIOT_BIN_DIR = "/root/.riot/bin";
const RIOT_BIN = `${RIOT_BIN_DIR}/riot`;
const DOCS_PIPELINE_AGENT = "riot-docs-pipeline@1.0";
const COMMAND_TIMEOUT_MS = 15 * 60 * 1000;
const SANDBOX_STARTUP_RETRY_LIMIT = 8;
const SANDBOX_STARTUP_RETRY_DELAY_MS = 2_000;
const TRANSIENT_SANDBOX_ERROR_PATTERNS = [
  "container is starting. please retry in a moment.",
  "container port not found",
  "connection refused: container port",
  "the container is not listening",
  "failed to verify port",
  "container did not start",
  "network connection lost",
  "container suddenly disconnected",
  "monitor failed to find container",
  "container exited with unexpected exit code",
  "container exited before we could determine",
  "timed out",
  "timeout",
  "the operation was aborted",
  "no container instance",
] as const;

export class DocsPipelineContainer extends Sandbox {
  sleepAfter = "5m";
  enableInternet = true;

  override async onActivityExpired(): Promise<void> {
    await this.destroy();
  }

  override onError(error: unknown): never {
    console.error("docs pipeline sandbox error", error);
    if (error instanceof Error) {
      throw error;
    }

    throw new Error(String(error));
  }
}

export class SandboxPackagePipelineExecutor implements PackagePipelineExecutor {
  constructor(private readonly env: SandboxExecutorEnv) {}

  async processRelease(request: PackagePipelineProcessRequest): Promise<DocsPipelineProcessResult> {
    if (this.env.DOCS_PIPELINE_CONTAINER === undefined) {
      throw new Error("DOCS_PIPELINE_CONTAINER binding is not configured.");
    }

    const runnerId = buildRunnerId(
      request.package_name,
      request.package_version,
      request.artifact_sha256,
    );
    const sandbox = getSandbox(
      this.env.DOCS_PIPELINE_CONTAINER as unknown as DurableObjectNamespace<DocsPipelineContainer>,
      runnerId,
    ) as DocsPipelineContainer;
    const env = commandEnv();
    const packageDir = `${WORKSPACE_ROOT}/packages/${request.package_name}`;
    const artifactPath = `/tmp/${request.package_name}-${request.package_version}.tar.gz`;
    const docsOutputDir = `${WORKSPACE_ROOT}/_build/doc/${request.package_name}/${request.package_version}`;

    try {
      logPipelineStep(request, "sandbox.attached", {
        runner_id: runnerId,
      });

      const upgradeCommand = [RIOT_BIN, "upgrade"] as const;
      const upgradeStartedAt = Date.now();
      logPipelineStep(request, "riot.upgrade.started", {
        command: upgradeCommand,
      });
      const upgradeExec = await execWithStartupRetry(
        sandbox,
        shellCommand(upgradeCommand),
        {
          cwd: WORKSPACE_ROOT,
          env,
          timeout: COMMAND_TIMEOUT_MS,
        },
      );
      const upgrade = buildCommandResult(
        [...upgradeCommand],
        upgradeExec,
        Date.now() - upgradeStartedAt,
      );
      logPipelineStep(request, "riot.upgrade.finished", {
        success: upgrade.success,
        exit_code: upgrade.exit_code,
        duration_ms: upgrade.duration_ms,
      });
      const probeCommand = [
        "sh",
        "-lc",
        "printf 'glibc=%s\\n' \"$(getconf GNU_LIBC_VERSION 2>/dev/null || echo unknown)\" && printf 'riot=%s\\n' \"$(/root/.riot/bin/riot version 2>/dev/null || echo unavailable)\" && cat /etc/os-release",
      ] as const;
      const probeStartedAt = Date.now();
      logPipelineStep(request, "environment.probe.started");
      const probeExec = await execWithStartupRetry(
        sandbox,
        shellCommand(probeCommand),
        {
          cwd: WORKSPACE_ROOT,
          env,
          timeout: COMMAND_TIMEOUT_MS,
        },
      );
      const environmentProbe = buildCommandResult(
        [...probeCommand],
        probeExec,
        Date.now() - probeStartedAt,
      );
      logPipelineStep(request, "environment.probe.finished", {
        success: environmentProbe.success,
        exit_code: environmentProbe.exit_code,
        duration_ms: environmentProbe.duration_ms,
      });

      await sandbox.mkdir(WORKSPACE_ROOT, { recursive: true });
      await sandbox.mkdir(`${WORKSPACE_ROOT}/packages`, { recursive: true });
      await sandbox.mkdir(packageDir, { recursive: true });
      await sandbox.setEnvVars(env);

      await sandbox.writeFile(
        `${WORKSPACE_ROOT}/ocaml-toolchain.toml`,
        toolchainToml(),
      );
      await sandbox.writeFile(
        `${WORKSPACE_ROOT}/riot.toml`,
        workspaceManifest(request.package_name),
      );

      const downloadCommand = [
        "sh",
        "-lc",
        `curl -fsSL --retry 3 --retry-delay 1 -H 'X-Riot-Agent: ${DOCS_PIPELINE_AGENT}' -o ${shellEscape(artifactPath)} ${shellEscape(request.source_archive_url)}`,
      ] as const;
      logPipelineStep(request, "artifact.download.started", {
        source_archive_url: request.source_archive_url,
      });
      const downloadExec = await execWithStartupRetry(
        sandbox,
        shellCommand(downloadCommand),
        {
          cwd: WORKSPACE_ROOT,
          env,
          timeout: COMMAND_TIMEOUT_MS,
        },
      );
      if (!downloadExec.success) {
        throw new Error(
          normalizeFailure(
            "failed to download published package artifact",
            downloadExec.stdout,
            downloadExec.stderr,
            downloadExec.exitCode,
          ),
        );
      }
      logPipelineStep(request, "artifact.download.finished");

      const extractCommand = ["tar", "-xzf", artifactPath, "-C", packageDir] as const;
      logPipelineStep(request, "artifact.extract.started", {
        package_dir: packageDir,
      });
      const extractExec = await execWithStartupRetry(
        sandbox,
        shellCommand(extractCommand),
        {
          cwd: WORKSPACE_ROOT,
          env,
          timeout: COMMAND_TIMEOUT_MS,
        },
      );
      if (!extractExec.success) {
        throw new Error(
          normalizeFailure(
            "failed to extract published package artifact",
            extractExec.stdout,
            extractExec.stderr,
            extractExec.exitCode,
          ),
        );
      }
      logPipelineStep(request, "artifact.extract.finished");

      const result: DocsPipelineProcessResult = {
        environment_probe: environmentProbe,
        upgrade,
      };

      if (request.verify_build) {
        const buildCommand = [RIOT_BIN, "build", "--json", request.package_name] as const;
        const buildStartedAt = Date.now();
        logPipelineStep(request, "build.started", {
          command: buildCommand,
        });
        const buildExec = await execWithStartupRetry(
          sandbox,
          shellCommand(buildCommand),
          {
            cwd: WORKSPACE_ROOT,
            env,
            timeout: COMMAND_TIMEOUT_MS,
          },
        );
        result.build = buildCommandResult(
          [...buildCommand],
          buildExec,
          Date.now() - buildStartedAt,
        );
        logPipelineStep(request, "build.finished", {
          success: result.build.success,
          exit_code: result.build.exit_code,
          duration_ms: result.build.duration_ms,
          json_event_count: result.build.json_events?.length ?? 0,
        });
      }

      if (request.generate_docs) {
        const docsCommand = [RIOT_BIN, "doc", "--json", "--release", "-p", request.package_name] as const;
        const docsStartedAt = Date.now();
        logPipelineStep(request, "docs.started", {
          command: docsCommand,
        });
        const docsExec = await execWithStartupRetry(
          sandbox,
          shellCommand(docsCommand),
          {
            cwd: WORKSPACE_ROOT,
            env,
            timeout: COMMAND_TIMEOUT_MS,
          },
        );
        const docs = buildCommandResult(
          [...docsCommand],
          docsExec,
          Date.now() - docsStartedAt,
        );
        logPipelineStep(request, "docs.finished", {
          success: docs.success,
          exit_code: docs.exit_code,
          duration_ms: docs.duration_ms,
          json_event_count: docs.json_events?.length ?? 0,
        });

        const files = docs.success
          ? await collectGeneratedDocsFiles(sandbox, docsOutputDir)
          : [];
        logPipelineStep(request, "docs.artifacts.collected", {
          output_dir: docsOutputDir,
          file_count: files.length,
        });

        result.docs = {
          ...docs,
          output_dir: docsOutputDir,
          files,
        };
      }

      logPipelineStep(request, "release.process.finished", {
        generated_docs: request.generate_docs,
        verified_build: request.verify_build,
      });
      return result;
    } finally {
      logPipelineStep(request, "sandbox.destroy.scheduled", {
        runner_id: runnerId,
      });
      void sandbox.destroy().catch((error) => {
        console.warn("failed to destroy docs pipeline sandbox", error);
      });
    }
  }
}

const RUNNER_REVISION = "v7";

function buildRunnerId(packageName: string, packageVersion: string, artifactSha256: string): string {
  const identity = `${RUNNER_REVISION}-${packageName}-${packageVersion}-${artifactSha256.slice(0, 12)}`;
  return identity
    .toLowerCase()
    .replaceAll(/[^a-z0-9-]+/g, "-")
    .replaceAll(/-+/g, "-")
    .replaceAll(/^-|-$/g, "");
}

function logPipelineStep(
  request: PackagePipelineProcessRequest,
  step: string,
  details?: Record<string, unknown>,
): void {
  console.log("[docs-pipeline]", JSON.stringify({
    package_name: request.package_name,
    package_version: request.package_version,
    run_kind: [
      request.verify_build ? "build" : null,
      request.generate_docs ? "docs" : null,
    ].filter((value) => value !== null),
    step,
    details: details ?? {},
  }));
}

function commandEnv(): Record<string, string> {
  return {
    HOME: "/root",
    PATH: `${RIOT_BIN_DIR}:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin`,
    RIOT_AGENT_HEADER: DOCS_PIPELINE_AGENT,
  };
}

async function execWithStartupRetry(
  sandbox: DocsPipelineContainer,
  command: string,
  options: {
    cwd: string;
    env: Record<string, string>;
    timeout: number;
  },
) {
  let attempt = 0;
  let lastError: unknown;

  while (attempt < SANDBOX_STARTUP_RETRY_LIMIT) {
    try {
      return await sandbox.exec(command, options);
    } catch (error) {
      if (!isSandboxStartingError(error)) {
        throw error;
      }

      lastError = error;
      attempt += 1;
      console.warn("retrying transient sandbox exec error", {
        attempt,
        limit: SANDBOX_STARTUP_RETRY_LIMIT,
        error: error instanceof Error ? error.message : String(error),
      });
      if (attempt >= SANDBOX_STARTUP_RETRY_LIMIT) {
        break;
      }

      await sleep(SANDBOX_STARTUP_RETRY_DELAY_MS);
    }
  }

  throw lastError;
}

function isSandboxStartingError(error: unknown): boolean {
  if (!(error instanceof Error)) {
    return false;
  }

  const message = error.message.toLowerCase();
  return TRANSIENT_SANDBOX_ERROR_PATTERNS.some((pattern) => message.includes(pattern));
}

function sleep(durationMs: number): Promise<void> {
  return new Promise((resolve) => setTimeout(resolve, durationMs));
}

function toolchainToml(): string {
  return [
    "[toolchain]",
    `version = "${DEFAULT_TOOLCHAIN_VERSION}"`,
    `targets = ["${DEFAULT_TARGET}"]`,
    "",
  ].join("\n");
}

function workspaceManifest(packageName: string): string {
  return [
    "[workspace]",
    `members = ["packages/${packageName}"]`,
    "",
  ].join("\n");
}

function shellCommand(command: readonly string[]): string {
  return command.map(shellEscape).join(" ");
}

function shellEscape(value: string): string {
  if (value.length === 0) {
    return "''";
  }

  return `'${value.replaceAll("'", "'\"'\"'")}'`;
}

function normalizeFailure(prefix: string, stdout: string, stderr: string, exitCode: number): string {
  const details = [stderr.trim(), stdout.trim()].filter((value) => value.length > 0).join("\n");
  if (details.length > 0) {
    return `${prefix}: ${details}`;
  }

  return `${prefix}: exit code ${exitCode}`;
}

function buildCommandResult(
  command: string[],
  result: { success: boolean; exitCode: number; stdout: string; stderr: string },
  durationMs: number,
): PackagePipelineCommandResult {
  const parsed = parseJsonlStdout(result.stdout);

  return {
    success: result.success,
    exit_code: result.exitCode,
    stdout: result.stdout,
    stderr: result.stderr,
    duration_ms: durationMs,
    command,
    json_events: parsed.jsonEvents,
    non_json_stdout_lines: parsed.nonJsonStdoutLines,
  };
}

function parseJsonlStdout(stdout: string): {
  jsonEvents: Record<string, unknown>[];
  nonJsonStdoutLines: string[];
} {
  const jsonEvents: Record<string, unknown>[] = [];
  const nonJsonStdoutLines: string[] = [];

  for (const rawLine of stdout.split(/\r?\n/u)) {
    const line = rawLine.trim();
    if (line.length === 0) {
      continue;
    }

    try {
      const parsed = JSON.parse(line) as unknown;
      if (parsed !== null && typeof parsed === "object" && !Array.isArray(parsed)) {
        jsonEvents.push(parsed as Record<string, unknown>);
      } else {
        nonJsonStdoutLines.push(rawLine);
      }
    } catch {
      nonJsonStdoutLines.push(rawLine);
    }
  }

  return {
    jsonEvents,
    nonJsonStdoutLines,
  };
}

async function collectGeneratedDocsFiles(
  sandbox: Sandbox,
  outputDir: string,
): Promise<GeneratedDocsFile[]> {
  const exists = await sandbox.exists(outputDir);
  if (!exists.exists) {
    return [];
  }

  const listed = await sandbox.listFiles(outputDir, {
    recursive: true,
    includeHidden: true,
  });

  const files: GeneratedDocsFile[] = [];
  for (const file of listed.files) {
    if (file.type !== "file") {
      continue;
    }

    const read = await sandbox.readFile(file.absolutePath);
    const contentBase64 =
      read.encoding === "base64"
        ? read.content
        : Buffer.from(read.content, "utf8").toString("base64");

    files.push({
      path: file.relativePath,
      content_base64: contentBase64,
      content_type: read.mimeType,
    });
  }

  return files.sort((left, right) => left.path.localeCompare(right.path));
}
