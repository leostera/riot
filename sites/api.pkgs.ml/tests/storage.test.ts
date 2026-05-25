import { describe, expect, test } from "bun:test";

import { packageIndexKey } from "../src/storage.ts";
import type { IndexConfig } from "../src/types.ts";

describe("registry storage", () => {
  test("package documents use cargo-style sharding", () => {
    const config: IndexConfig = {
      cdnBaseUrl: "https://cdn.pkgs.ml",
      indexBaseUrl: "https://cdn.pkgs.ml",
      indexBasePath: "index/v1",
      indexRoutePath: "v1/index",
      viewsBasePath: "views/v1",
      authCookieDomain: "pkgs.ml",
      pkgsWebBaseUrl: "https://pkgs.ml",
      playWebBaseUrl: "https://play.riot.ml",
    };

    expect(packageIndexKey(config, "x")).toBe("index/v1/1/x.json");
    expect(packageIndexKey(config, "io")).toBe("index/v1/2/io.json");
    expect(packageIndexKey(config, "mcp")).toBe("index/v1/3/m/mcp.json");
    expect(packageIndexKey(config, "kernel")).toBe("index/v1/ke/rn/kernel.json");
  });
});
