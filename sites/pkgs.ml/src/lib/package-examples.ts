export function packageExampleLabel(path: string): string {
  return trimExamplePath(path).replace(/\.ml$/i, "");
}

export function packagePlaygroundHref(
  packageName: string,
  version: string,
  latestVersion?: string,
): string {
  const requestedVersion = latestVersion && version === latestVersion ? "latest" : version;
  return `https://play.riot.ml/?deps=${encodeURIComponent(`${packageName}:${requestedVersion}`)}`;
}

export function packageExamplePlayHref(
  packageName: string,
  version: string,
  path: string,
): string {
  const relativePath = trimExamplePath(path)
    .split("/")
    .map((segment) => encodeURIComponent(segment))
    .join("/");

  return `https://play.riot.ml/p/${encodeURIComponent(packageName)}/${encodeURIComponent(version)}/examples/${relativePath}`;
}

function trimExamplePath(path: string): string {
  return path.replace(/^examples\//, "");
}
