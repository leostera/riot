import type { APIRoute } from "astro";

import { createApiToken, fetchSession } from "@/lib/auth";

export const prerender = false;

export const POST: APIRoute = async ({ request }) => {
  const session = await fetchSession(request);
  if (!session.authenticated || !session.user) {
    return Response.json(
      {
        error: "unauthorized",
        message: "Authentication is required to create publish tokens.",
      },
      { status: 401 },
    );
  }

  let name = "";
  const contentType = request.headers.get("content-type") ?? "";

  if (contentType.includes("application/json")) {
    const body = (await request.json()) as { name?: unknown };
    name = typeof body.name === "string" ? body.name : "";
  } else {
    const form = await request.formData();
    name = String(form.get("name") ?? "");
  }

  try {
    const created = await createApiToken(request, name);
    return Response.json(created, { status: 201 });
  } catch (error) {
    if (error instanceof Error && error.message === "unauthorized") {
      return Response.json(
        {
          error: "unauthorized",
          message: "Authentication is required to create publish tokens.",
        },
        { status: 401 },
      );
    }

    return Response.json(
      {
        error: "token_create_failed",
        message: error instanceof Error ? error.message : "Token creation failed.",
      },
      { status: 400 },
    );
  }
};
