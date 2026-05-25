import type { APIRoute } from "astro";

import { fetchSession, revokeApiToken } from "@/lib/auth";

export const prerender = false;

export const DELETE: APIRoute = async ({ request, params }) => {
  const session = await fetchSession(request);
  if (!session.authenticated || !session.user) {
    return Response.json(
      {
        error: "unauthorized",
        message: "Authentication is required to revoke publish tokens.",
      },
      { status: 401 },
    );
  }

  const tokenId = params.tokenId ?? "";
  if (tokenId.length === 0) {
    return Response.json(
      {
        error: "token_not_found",
        message: "Token id is required.",
      },
      { status: 404 },
    );
  }

  try {
    await revokeApiToken(request, tokenId);
    return Response.json({ ok: true });
  } catch (error) {
    if (error instanceof Error && error.message === "unauthorized") {
      return Response.json(
        {
          error: "unauthorized",
          message: "Authentication is required to revoke publish tokens.",
        },
        { status: 401 },
      );
    }

    return Response.json(
      {
        error: "token_revoke_failed",
        message: error instanceof Error ? error.message : "Token revocation failed.",
      },
      { status: 400 },
    );
  }
};
