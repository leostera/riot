import type { APIRoute } from "astro";

import { fetchSession, yankPackageRelease } from "@/lib/auth";

export const prerender = false;

export const POST: APIRoute = async ({ request, params }) => {
  const session = await fetchSession(request);
  if (!session.authenticated || !session.user) {
    return Response.json(
      {
        error: "unauthorized",
        message: "Authentication is required to yank package releases.",
      },
      { status: 401 },
    );
  }

  const packageName = params.packageName ?? "";
  const version = params.version ?? "";
  if (packageName.length === 0 || version.length === 0) {
    return Response.json(
      {
        error: "invalid_release",
        message: "Package name and version are required.",
      },
      { status: 400 },
    );
  }

  try {
    const yanked = await yankPackageRelease(request, packageName, version);
    return Response.json(yanked, { status: 200 });
  } catch (error) {
    if (error instanceof Error && error.message === "unauthorized") {
      return Response.json(
        {
          error: "unauthorized",
          message: "Authentication is required to yank package releases.",
        },
        { status: 401 },
      );
    }

    return Response.json(
      {
        error: "release_yank_failed",
        message: error instanceof Error ? error.message : "Failed to yank release.",
      },
      { status: 400 },
    );
  }
};
