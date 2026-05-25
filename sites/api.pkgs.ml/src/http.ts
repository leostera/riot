export function json(body: unknown, init: ResponseInit = {}): Response {
  const headers = new Headers(init.headers);
  if (!headers.has("content-type")) {
    headers.set("content-type", "application/json; charset=utf-8");
  }

  return new Response(JSON.stringify(body, null, 2), {
    ...init,
    headers,
  });
}

export function methodNotAllowed(allowed: string[]): Response {
  return json(
    {
      error: "method_not_allowed",
      message: `Allowed methods: ${allowed.join(", ")}.`,
    },
    {
      status: 405,
      headers: {
        allow: allowed.join(", "),
      },
    },
  );
}

export function immutableHeaders(contentType: string): Headers {
  const headers = new Headers();
  headers.set("content-type", contentType);
  headers.set("cache-control", "public, max-age=31536000, immutable");
  return headers;
}
