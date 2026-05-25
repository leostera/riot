import { DurableObject } from "cloudflare:workers";

import { HttpError } from "./errors.ts";
import { json } from "./http.ts";
import { handlePublicationCoordinatorRequest } from "./publication-coordinator-handler.ts";
import type { Env } from "./types.ts";

export class PublicationCoordinator extends DurableObject<Env> {
  #tail: Promise<void> = Promise.resolve();

  override async fetch(request: Request): Promise<Response> {
    if (request.method !== "POST") {
      return json(
        {
          error: "method_not_allowed",
          message: "Publication coordination only supports POST.",
        },
        { status: 405, headers: { allow: "POST" } },
      );
    }

    return await this.#serialize(() => handlePublicationCoordinatorRequest(request, this.env));
  }

  async #serialize(work: () => Promise<Response>): Promise<Response> {
    const previous = this.#tail;
    let release!: () => void;
    this.#tail = new Promise<void>((resolve) => {
      release = resolve;
    });

    await previous;

    try {
      return await work();
    } catch (error) {
      const httpError = normalizeError(error);
      return json(
        {
          error: httpError.error,
          message: httpError.message,
        },
        { status: httpError.status },
      );
    } finally {
      release();
    }
  }
}

function normalizeError(error: unknown): HttpError {
  if (error instanceof HttpError) {
    return error;
  }

  const message = error instanceof Error ? error.toString() : "Unexpected internal error.";
  return new HttpError(500, "internal_error", message);
}
