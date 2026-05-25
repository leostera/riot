export interface Env {
  RIOT_INSTALL_SCRIPT_URL?: string;
}

const DEFAULT_INSTALL_SCRIPT_URL = "https://cdn.pkgs.ml/riot/install.sh";
const INSTALLER_PATHS = new Set(["/", "/install.sh"]);

const notFound = (): Response =>
  new Response("Not Found", {
    status: 404,
    headers: {
      "content-type": "text/plain; charset=utf-8",
    },
  });

const installerUnavailable = (): Response =>
  new Response("Installer unavailable", {
    status: 502,
    headers: {
      "content-type": "text/plain; charset=utf-8",
      "cache-control": "public, max-age=30",
    },
  });

export default {
  async fetch(request: Request, env: Env): Promise<Response> {
    const url = new URL(request.url);
    if (!INSTALLER_PATHS.has(url.pathname)) {
      return notFound();
    }

    if (request.method !== "GET" && request.method !== "HEAD") {
      return new Response("Method Not Allowed", {
        status: 405,
        headers: {
          allow: "GET, HEAD",
          "content-type": "text/plain; charset=utf-8",
        },
      });
    }

    const upstream = await fetch(env.RIOT_INSTALL_SCRIPT_URL ?? DEFAULT_INSTALL_SCRIPT_URL, {
      method: request.method,
      headers: {
        accept: request.headers.get("accept") ?? "*/*",
      },
    });

    if (!upstream.ok) {
      return installerUnavailable();
    }

    const headers = new Headers(upstream.headers);
    headers.set("cache-control", "public, max-age=300");
    headers.set("x-robots-tag", "noindex");

    return new Response(request.method === "HEAD" ? null : upstream.body, {
      status: upstream.status,
      headers,
    });
  },
};
