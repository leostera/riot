import { describe, expect, it } from "bun:test";

import { buildPackageFacts, packageIndexPath } from "../src/lib/package-index.ts";
import type { PackageIndexDocument } from "../src/lib/types.ts";

describe("packageIndexPath", () => {
  it("matches the cargo-style shard layout", () => {
    expect(packageIndexPath("a", "index/v1")).toBe("index/v1/1/a.json");
    expect(packageIndexPath("ab", "index/v1")).toBe("index/v1/2/ab.json");
    expect(packageIndexPath("abc", "index/v1")).toBe("index/v1/3/a/abc.json");
    expect(packageIndexPath("kernel", "index/v1")).toBe("index/v1/ke/rn/kernel.json");
  });
});

describe("buildPackageFacts", () => {
  it("produces install-oriented facts for the package header", () => {
    const document: PackageIndexDocument = {
      schema_version: 1,
      name: "kernel",
      latest: "0.0.1",
      updated_at: "2026-03-27T00:00:00.000Z",
      releases: [
        {
          version: "0.0.1",
          published_at: "2026-03-27T00:00:00.000Z",
          canonical_locator: "github.com/leostera/riot-new/packages/kernel",
          repo_url: "https://github.com/leostera/riot-new",
          subdir: "packages/kernel",
          artifact_sha256: "abc123",
          description: "Kernel package",
          license: "MIT",
          homepage: undefined,
          repository: "https://github.com/leostera/riot-new",
          root_module: "Kernel",
          manifest_key: "packages/example/abc.manifest.json",
          source_key: "sources/example/abc.tar.gz",
          dependencies: [],
        },
      ],
    };

    const facts = buildPackageFacts(document, document.releases[0]!);

    expect(facts.find((fact) => fact.label === "Install")?.value).toBe("riot add kernel");
    expect(facts.find((fact) => fact.label === "riot.toml")?.value).toBe('kernel = "0.0.1"');
    expect(facts.find((fact) => fact.label === "License")).toBeUndefined();
    expect(facts.find((fact) => fact.label === "Dependencies")).toBeUndefined();
    expect(facts).toHaveLength(2);
    expect(facts.find((fact) => fact.label === "Versions")).toBeUndefined();
    expect(facts.find((fact) => fact.label === "OCaml")).toBeUndefined();
    expect(facts.find((fact) => fact.label === "Code size")).toBeUndefined();
    expect(facts.find((fact) => fact.label === "Archive")).toBeUndefined();
    expect(facts.find((fact) => fact.label === "Downloads")).toBeUndefined();
  });
});
