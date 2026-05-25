import type { RegistryEventRecord } from "./types.ts";

export function eventLabel(eventType: RegistryEventRecord["event_type"]): string {
  switch (eventType) {
    case "package.submitted":
      return "Submitted";
    case "package.verified":
      return "Verified";
    case "package.published":
      return "Published";
    case "package.searchable":
      return "Searchable";
    case "package.indexed":
      return "Indexed";
    case "package.processing.queued":
      return "Queued";
    case "package.processing.started":
      return "Processing";
    case "package.processing.requeued":
      return "Requeued";
    case "package.processing.blocked":
      return "Blocked";
    case "package.processing.finished":
      return "Processed";
    case "package.docs.staged":
      return "Docs staged";
    case "package.docs.generated":
      return "Docs ready";
    case "package.docs.failed":
      return "Docs failed";
    case "package.build.staged":
      return "Build staged";
    case "package.build.verified":
      return "Build verified";
    case "package.build.failed":
      return "Build failed";
  }
}

export function eventTone(eventType: RegistryEventRecord["event_type"]): string {
  switch (eventType) {
    case "package.submitted":
      return "border-slate-200 bg-slate-50 text-slate-700";
    case "package.verified":
      return "border-sky-200 bg-sky-50 text-sky-700 dark:border-sky-900 dark:bg-sky-950/40 dark:text-sky-300";
    case "package.published":
      return "border-emerald-200 bg-emerald-50 text-emerald-700 dark:border-emerald-900 dark:bg-emerald-950/40 dark:text-emerald-300";
    case "package.searchable":
      return "border-violet-200 bg-violet-50 text-violet-700 dark:border-violet-900 dark:bg-violet-950/40 dark:text-violet-300";
    case "package.indexed":
      return "border-amber-200 bg-amber-50 text-amber-700 dark:border-amber-900 dark:bg-amber-950/40 dark:text-amber-300";
    case "package.processing.queued":
      return "border-slate-200 bg-slate-50 text-slate-700";
    case "package.processing.started":
      return "border-sky-200 bg-sky-50 text-sky-700 dark:border-sky-900 dark:bg-sky-950/40 dark:text-sky-300";
    case "package.processing.requeued":
      return "border-rose-200 bg-rose-50 text-rose-700 dark:border-rose-900 dark:bg-rose-950/40 dark:text-rose-300";
    case "package.processing.blocked":
      return "border-rose-300 bg-rose-100 text-rose-800 dark:border-rose-900 dark:bg-rose-950/60 dark:text-rose-200";
    case "package.processing.finished":
      return "border-emerald-200 bg-emerald-50 text-emerald-700 dark:border-emerald-900 dark:bg-emerald-950/40 dark:text-emerald-300";
    case "package.docs.staged":
      return "border-indigo-200 bg-indigo-50 text-indigo-700 dark:border-indigo-900 dark:bg-indigo-950/40 dark:text-indigo-300";
    case "package.docs.generated":
      return "border-emerald-200 bg-emerald-50 text-emerald-700 dark:border-emerald-900 dark:bg-emerald-950/40 dark:text-emerald-300";
    case "package.docs.failed":
      return "border-rose-200 bg-rose-50 text-rose-700 dark:border-rose-900 dark:bg-rose-950/40 dark:text-rose-300";
    case "package.build.staged":
      return "border-teal-200 bg-teal-50 text-teal-700 dark:border-teal-900 dark:bg-teal-950/40 dark:text-teal-300";
    case "package.build.verified":
      return "border-emerald-200 bg-emerald-50 text-emerald-700 dark:border-emerald-900 dark:bg-emerald-950/40 dark:text-emerald-300";
    case "package.build.failed":
      return "border-rose-200 bg-rose-50 text-rose-700 dark:border-rose-900 dark:bg-rose-950/40 dark:text-rose-300";
  }
}

export function formatEventTimestamp(value: string): string {
  const date = new Date(value);
  if (Number.isNaN(date.getTime())) {
    return value;
  }

  return new Intl.DateTimeFormat("en", {
    year: "numeric",
    month: "short",
    day: "numeric",
    hour: "2-digit",
    minute: "2-digit",
    second: "2-digit",
  }).format(date);
}

export function formatEventTimestampCompact(value: string): string {
  const date = new Date(value);
  if (Number.isNaN(date.getTime())) {
    return value;
  }

  return new Intl.DateTimeFormat("en", {
    month: "short",
    day: "numeric",
    hour: "2-digit",
    minute: "2-digit",
  }).format(date);
}

export function packageLabel(event: RegistryEventRecord): string {
  if (!event.package_name) {
    return "Unknown package";
  }

  return event.package_version
    ? `${event.package_name} ${event.package_version}`
    : event.package_name;
}

export function packageHref(event: RegistryEventRecord): string | null {
  if (!event.package_name) {
    return null;
  }

  return event.package_version
    ? `/p/${encodeURIComponent(event.package_name)}/${encodeURIComponent(event.package_version)}`
    : `/p/${encodeURIComponent(event.package_name)}`;
}

export function packageActivityHref(event: RegistryEventRecord): string | null {
  if (!event.package_name || !event.package_version) {
    return packageHref(event);
  }

  return `/p/${encodeURIComponent(event.package_name)}/${encodeURIComponent(event.package_version)}/activity`;
}

export function secondaryFacts(event: RegistryEventRecord): string[] {
  const facts: string[] = [];
  const artifactSha = event.payload.artifact_sha256;
  const dependencyCount = event.payload.dependency_count;
  const latest = event.payload.latest;
  const attemptCount = event.payload.attempt_count;
  const runKind = event.payload.run_kind;
  const exitCode = event.payload.exit_code;
  const jsonEventCount = event.payload.json_event_count;
  const lastJsonEventType = event.payload.last_json_event_type;

  if (typeof dependencyCount === "number" && Number.isFinite(dependencyCount)) {
    facts.push(`${dependencyCount} deps`);
  }

  if (typeof latest === "string" && latest.length > 0) {
    facts.push(`latest ${latest}`);
  }

  if (typeof runKind === "string" && runKind.length > 0) {
    facts.push(runKind);
  }

  if (typeof exitCode === "number" && Number.isFinite(exitCode)) {
    facts.push(`exit ${exitCode}`);
  }

  if (typeof jsonEventCount === "number" && Number.isFinite(jsonEventCount) && jsonEventCount > 0) {
    facts.push(`${jsonEventCount} json`);
  }

  if (typeof lastJsonEventType === "string" && lastJsonEventType.length > 0) {
    facts.push(lastJsonEventType);
  }

  if (typeof attemptCount === "number" && Number.isFinite(attemptCount)) {
    facts.push(`attempt ${attemptCount}`);
  }

  if (typeof artifactSha === "string" && artifactSha.length >= 7) {
    facts.push(`artifact ${artifactSha.slice(0, 12)}`);
  }

  if (typeof event.package_locator === "string" && event.package_locator.length > 0) {
    facts.push(event.package_locator);
  }

  return facts;
}
