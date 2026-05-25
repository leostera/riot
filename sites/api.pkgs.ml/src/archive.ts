import { HttpError } from "./errors.ts";

export async function readArchiveFileFromTarGz(
  archiveBytes: Uint8Array<ArrayBuffer>,
  archiveRelativePath: string,
): Promise<string | null> {
  return await readFileFromTarGz(archiveBytes, archiveRelativePath);
}

export async function listArchiveFilesFromTarGz(
  archiveBytes: Uint8Array<ArrayBuffer>,
  options?: { prefix?: string },
): Promise<string[]> {
  const tarBytes = await gunzip(archiveBytes);
  const normalizedPrefix = options?.prefix ? normalizeArchivePath(options.prefix) : null;
  const files: string[] = [];

  for (const entry of iterateTarEntries(tarBytes)) {
    if (!isRegularFileEntry(entry.typeFlag)) {
      continue;
    }

    const normalized = normalizeArchivePath(entry.name);
    if (normalizedPrefix !== null && !normalized.startsWith(`${normalizedPrefix}/`)) {
      continue;
    }

    files.push(normalized);
  }

  return files;
}

export async function readArchiveFilesFromTarGz(
  archiveBytes: Uint8Array<ArrayBuffer>,
  archiveRelativePaths: string[],
): Promise<Array<{ path: string; contents: string }>> {
  const tarBytes = await gunzip(archiveBytes);
  const candidates = new Map<string, string>();

  for (const path of archiveRelativePaths) {
    const normalized = normalizeArchivePath(path);
    candidates.set(normalized.toLowerCase(), normalized);
  }

  const files: Array<{ path: string; contents: string }> = [];
  for (const entry of iterateTarEntries(tarBytes)) {
    if (!isRegularFileEntry(entry.typeFlag)) {
      continue;
    }

    const normalized = normalizeArchivePath(entry.name);
    const key = normalized.toLowerCase();
    if (!candidates.has(key)) {
      continue;
    }

    files.push({
      path: normalized,
      contents: new TextDecoder().decode(entry.body),
    });
  }

  return files;
}

export async function readFirstArchiveFileFromTarGz(
  archiveBytes: Uint8Array<ArrayBuffer>,
  archiveRelativePaths: string[],
): Promise<{ path: string; contents: string } | null> {
  const tarBytes = await gunzip(archiveBytes);
  const candidates = new Set<string>();

  for (const path of archiveRelativePaths) {
    candidates.add(normalizeArchivePath(path).toLowerCase());
  }

  for (const entry of iterateTarEntries(tarBytes)) {
    if (!isRegularFileEntry(entry.typeFlag)) {
      continue;
    }

    const normalized = normalizeArchivePath(entry.name);
    if (candidates.has(normalized.toLowerCase())) {
      return {
        path: normalized,
        contents: new TextDecoder().decode(entry.body),
      };
    }
  }

  return null;
}

async function gunzip(bytes: Uint8Array<ArrayBuffer>): Promise<Uint8Array<ArrayBuffer>> {
  const stream = new Response(bytes).body;
  if (stream === null) {
    throw new HttpError(500, "archive_read_failed", "Archive body was unexpectedly empty.");
  }

  const decompressed = stream.pipeThrough(new DecompressionStream("gzip"));
  return new Uint8Array(await new Response(decompressed).arrayBuffer());
}

async function readFileFromTarGz(
  archiveBytes: Uint8Array<ArrayBuffer>,
  relativePath: string,
): Promise<string | null> {
  const normalizedPath = normalizeArchivePath(relativePath);
  const tarBytes = await gunzip(archiveBytes);

  for (const entry of iterateTarEntries(tarBytes)) {
    if (!isRegularFileEntry(entry.typeFlag)) {
      continue;
    }

    if (normalizeArchivePath(entry.name) === normalizedPath) {
      return new TextDecoder().decode(entry.body);
    }
  }

  return null;
}

function *iterateTarEntries(bytes: Uint8Array<ArrayBuffer>): Iterable<TarEntry> {
  let offset = 0;

  while (offset + 512 <= bytes.length) {
    const header = bytes.subarray(offset, offset + 512);
    if (isZeroBlock(header)) {
      break;
    }

    const name = readString(header, 0, 100);
    const prefix = readString(header, 345, 155);
    const mode = parseOctal(readString(header, 100, 8));
    const size = parseOctal(readString(header, 124, 12));
    const mtime = parseOctal(readString(header, 136, 12));
    const typeFlag = readString(header, 156, 1);
    const linkName = readString(header, 157, 100);
    const fullName = prefix.length > 0 ? `${prefix}/${name}` : name;
    const bodyOffset = offset + 512;
    const body = bytes.subarray(bodyOffset, bodyOffset + size);

    if (typeFlag === "" || typeFlag === "0" || typeFlag === "5" || typeFlag === "2") {
      yield {
        name: fullName,
        body,
        mode,
        mtime,
        typeFlag,
        linkName,
      };
    }

    offset = bodyOffset + alignTo512(size);
  }
}

function normalizeArchivePath(path: string): string {
  return path.replace(/^\/+|\/+$/g, "");
}

function readString(bytes: Uint8Array<ArrayBuffer>, start: number, length: number): string {
  const slice = bytes.subarray(start, start + length);
  const raw = new TextDecoder().decode(slice);
  const nul = raw.indexOf("\u0000");
  return (nul === -1 ? raw : raw.slice(0, nul)).trim();
}

function parseOctal(value: string): number {
  return value.length === 0 ? 0 : Number.parseInt(value, 8);
}

function alignTo512(size: number): number {
  return Math.ceil(size / 512) * 512;
}

function isZeroBlock(bytes: Uint8Array<ArrayBuffer>): boolean {
  for (const byte of bytes) {
    if (byte !== 0) {
      return false;
    }
  }

  return true;
}

function isRegularFileEntry(typeFlag: string): boolean {
  return typeFlag === "" || typeFlag === "0";
}

interface TarEntry {
  name: string;
  body: Uint8Array<ArrayBuffer>;
  mode: number;
  mtime: number;
  typeFlag: string;
  linkName: string;
}
