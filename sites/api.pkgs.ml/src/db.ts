import { drizzle } from "drizzle-orm/d1";

import * as schema from "./schema.ts";

export function registryDb(db: D1Database) {
  return drizzle(db, { schema });
}

export { schema };
