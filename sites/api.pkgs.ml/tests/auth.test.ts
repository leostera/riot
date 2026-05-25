import { describe, expect, test } from "bun:test";

import { handleRequest } from "../src/routes.ts";
import { applyMetadataMigrations, readPackageClaim, readUserLoginRecord } from "../src/metadata-db.ts";
import { FakeExecutionContext, makeEnv, makeTarGz, withMockedFetch } from "./helpers.ts";

describe("riot package registry auth", () => {
  test("github oauth callback creates a session and /v1/me returns the user", async () => {
    const { env, db, bucket } = makeEnv({
      GITHUB_OAUTH_CLIENT_ID: "github-client-id",
      GITHUB_OAUTH_CLIENT_SECRET: "github-client-secret",
      PKGS_WEB_BASE_URL: "https://pkgs.ml",
    });

    const startCtx = new FakeExecutionContext();
    const startResponse = await handleRequest(
      new Request("https://registry.test/v1/auth/github/start?return_to=/u/leostera/tokens"),
      env,
      startCtx,
    );
    await startCtx.drain();

    expect(startResponse.status).toBe(302);
    const authorizationUrl = new URL(startResponse.headers.get("location") ?? "");
    expect(authorizationUrl.origin).toBe("https://github.com");
    expect(authorizationUrl.pathname).toBe("/login/oauth/authorize");

    const state = authorizationUrl.searchParams.get("state");
    expect(typeof state).toBe("string");
    expect(state).not.toBeNull();

    const callbackCtx = new FakeExecutionContext();
    const callbackResponse = await withMockedFetch(async (input) => {
      const url = new URL(toRequestUrl(input));
      if (url.origin === "https://github.com" && url.pathname === "/login/oauth/access_token") {
        return Response.json({
          access_token: "github-access-token",
          token_type: "bearer",
        });
      }

      if (url.origin === "https://api.github.com" && url.pathname === "/user") {
        return Response.json({
          id: 42,
          login: "leostera",
          name: "Leo Stera",
          avatar_url: "https://avatars.githubusercontent.com/u/42",
        });
      }

      if (url.origin === "https://api.github.com" && url.pathname === "/user/emails") {
        return Response.json([
          {
            email: "leo@example.com",
            primary: true,
            verified: true,
          },
        ]);
      }

      if (url.toString() === "https://avatars.githubusercontent.com/u/42") {
        return new Response("avatar", {
          status: 200,
          headers: {
            "content-type": "image/png",
          },
        });
      }

      throw new Error(`Unexpected fetch to ${url.toString()}`);
    }, async () => {
      return await handleRequest(
        new Request(
          `https://registry.test/v1/auth/github/callback?code=oauth-code&state=${encodeURIComponent(
            state ?? "",
          )}`,
        ),
        env,
        callbackCtx,
      );
    });
    await callbackCtx.drain();

    expect(callbackResponse.status).toBe(302);
    expect(callbackResponse.headers.get("location")).toBe("https://pkgs.ml/u/leostera/tokens");

    const cookie = callbackResponse.headers.get("set-cookie");
    expect(cookie).toContain("pkgs_session=");
    await applyMetadataMigrations(db as unknown as D1Database);
    expect(await readUserLoginRecord(db as unknown as D1Database, "leostera")).not.toBeNull();

    const sessionCtx = new FakeExecutionContext();
    const sessionResponse = await handleRequest(
      new Request("https://registry.test/v1/me", {
        headers: {
          cookie: cookie ?? "",
          accept: "application/json",
        },
      }),
      env,
      sessionCtx,
    );
    await sessionCtx.drain();

    expect(sessionResponse.status).toBe(200);
    expect(await readJson(sessionResponse)).toMatchObject({
      authenticated: true,
      user: {
        github_login: "leostera",
        github_name: "Leo Stera",
        github_avatar_url: "https://cdn.pkgs.ml/avatars/leostera",
        github_email: "leo@example.com",
        github_email_verified: true,
      },
    });
    expect(await bucket.head("avatars/leostera")).not.toBeNull();
  });

  test("github oauth callback rejects users without verified primary email", async () => {
    const { env } = makeEnv({
      GITHUB_OAUTH_CLIENT_ID: "github-client-id",
      GITHUB_OAUTH_CLIENT_SECRET: "github-client-secret",
      PKGS_WEB_BASE_URL: "https://pkgs.ml",
    });

    const startCtx = new FakeExecutionContext();
    const startResponse = await handleRequest(
      new Request("https://registry.test/v1/auth/github/start?return_to=/u/leostera/tokens"),
      env,
      startCtx,
    );
    await startCtx.drain();

    const state = new URL(startResponse.headers.get("location") ?? "").searchParams.get("state");
    if (state === null) {
      throw new Error("OAuth state was missing.");
    }

    const callbackCtx = new FakeExecutionContext();
    const callbackResponse = await withMockedFetch(async (input) => {
      const url = new URL(toRequestUrl(input));
      if (url.origin === "https://github.com" && url.pathname === "/login/oauth/access_token") {
        return Response.json({
          access_token: "github-access-token",
          token_type: "bearer",
        });
      }
      if (url.origin === "https://api.github.com" && url.pathname === "/user") {
        return Response.json({
          id: 42,
          login: "leostera",
          name: "Leo Stera",
          avatar_url: "https://avatars.githubusercontent.com/u/42",
        });
      }
      if (url.origin === "https://api.github.com" && url.pathname === "/user/emails") {
        return Response.json([
          {
            email: "leo@example.com",
            primary: true,
            verified: false,
          },
        ]);
      }
      throw new Error(`Unexpected fetch to ${url.toString()}`);
    }, async () => {
      return await handleRequest(
        new Request(
          `https://registry.test/v1/auth/github/callback?code=oauth-code&state=${encodeURIComponent(
            state,
          )}`,
        ),
        env,
        callbackCtx,
      );
    });
    await callbackCtx.drain();

    expect(callbackResponse.status).toBe(403);
    expect(await readJson(callbackResponse)).toMatchObject({
      error: "github_email_not_verified",
    });
  });

  test("authenticated users can create, list, and revoke publish tokens", async () => {
    const { env } = makeEnv({
      GITHUB_OAUTH_CLIENT_ID: "github-client-id",
      GITHUB_OAUTH_CLIENT_SECRET: "github-client-secret",
      PKGS_WEB_BASE_URL: "https://pkgs.ml",
    });

    const cookie = await loginAsGitHubUser(env, "leostera");

    const createCtx = new FakeExecutionContext();
    const createResponse = await handleRequest(
      new Request("https://registry.test/v1/me/tokens", {
        method: "POST",
        headers: {
          cookie,
          accept: "application/json",
          "content-type": "application/json",
        },
        body: JSON.stringify({ name: "local publish" }),
      }),
      env,
      createCtx,
    );
    await createCtx.drain();

    expect(createResponse.status).toBe(201);
    const created = (await readJson(createResponse)) as {
      plaintext_token: string;
      token: { token_id: string; name: string };
    };
    expect(created.plaintext_token.startsWith("sk-")).toBe(true);
    expect(created.token.name).toBe("local publish");

    const listCtx = new FakeExecutionContext();
    const listResponse = await handleRequest(
      new Request("https://registry.test/v1/me/tokens", {
        headers: {
          cookie,
          accept: "application/json",
        },
      }),
      env,
      listCtx,
    );
    await listCtx.drain();

    expect(listResponse.status).toBe(200);
    expect(await readJson(listResponse)).toMatchObject({
      user: {
        github_login: "leostera",
      },
      tokens: [
        {
          token_id: created.token.token_id,
          name: "local publish",
          capabilities: ["publish"],
        },
      ],
    });

    const deleteCtx = new FakeExecutionContext();
    const deleteResponse = await handleRequest(
      new Request(`https://registry.test/v1/me/tokens/${created.token.token_id}`, {
        method: "DELETE",
        headers: {
          cookie,
          accept: "application/json",
        },
      }),
      env,
      deleteCtx,
    );
    await deleteCtx.drain();

    expect(deleteResponse.status).toBe(200);
    expect(await readJson(deleteResponse)).toMatchObject({
      token: {
        token_id: created.token.token_id,
        revoked_at: expect.any(String),
      },
    });
  });

  test("publish tokens can create claims for matching github owners and adopt legacy claims on later versions", async () => {
    const { env, db, queue, indexedQueue } = makeEnv({
      GITHUB_OAUTH_CLIENT_ID: "github-client-id",
      GITHUB_OAUTH_CLIENT_SECRET: "github-client-secret",
      PKGS_WEB_BASE_URL: "https://pkgs.ml",
    });
    const cookie = await loginAsGitHubUser(env, "leostera");
    const publishToken = await createPublishToken(env, cookie, "publish");
    const firstArchive = await makeTarGz({
      "riot.toml": [
        "[package]",
        'name = "minttea"',
        'version = "0.4.1"',
        "public = true",
        'description = "Terminal UI toolkit for Riot"',
        'license = "MIT"',
      ].join("\n"),
    }, "");
    const secondArchive = await makeTarGz({
      "riot.toml": [
        "[package]",
        'name = "minttea"',
        'version = "0.4.2"',
        "public = true",
        'description = "Terminal UI toolkit for Riot"',
        'license = "MIT"',
      ].join("\n"),
    }, "");

    const rootCtx = new FakeExecutionContext();
    const rootPublish = await handleRequest(
      new Request("https://registry.test/v1/publish", {
        method: "POST",
        headers: {
          authorization: "Bearer root-secret",
          "content-type": "application/gzip",
        },
        body: firstArchive,
      }),
      env,
      rootCtx,
    );
    await rootCtx.drain();
    expect(rootPublish.status).toBe(200);

    const userCtx = new FakeExecutionContext();
    const userPublish = await handleRequest(
      new Request("https://registry.test/v1/publish", {
        method: "POST",
        headers: {
          authorization: `Bearer ${publishToken}`,
          "content-type": "application/gzip",
        },
        body: secondArchive,
      }),
      env,
      userCtx,
    );
    await userCtx.drain();

    expect(userPublish.status).toBe(200);

    await applyMetadataMigrations(db as unknown as D1Database);
    const claim = await readPackageClaim(db as unknown as D1Database, "minttea");
    expect(claim).toMatchObject({
      owner_github_login: "leostera",
    });
    expect(queue.messages).toHaveLength(2);
    expect(indexedQueue.messages).toHaveLength(2);
  });

  test("publish tokens can claim packages independent of source locator owner", async () => {
    const { env, db } = makeEnv({
      GITHUB_OAUTH_CLIENT_ID: "github-client-id",
      GITHUB_OAUTH_CLIENT_SECRET: "github-client-secret",
      PKGS_WEB_BASE_URL: "https://pkgs.ml",
    });
    const cookie = await loginAsGitHubUser(env, "someoneelse");
    const publishToken = await createPublishToken(env, cookie, "publish");
    const archive = await makeTarGz({
      "riot.toml": [
        "[package]",
        'name = "minttea"',
        'version = "0.4.2"',
        "public = true",
        'description = "Terminal UI toolkit for Riot"',
        'license = "MIT"',
      ].join("\n"),
    }, "");

    const publishCtx = new FakeExecutionContext();
    const publishResponse = await handleRequest(
      new Request("https://registry.test/v1/publish", {
        method: "POST",
        headers: {
          authorization: `Bearer ${publishToken}`,
          "content-type": "application/gzip",
        },
        body: archive,
      }),
      env,
      publishCtx,
    );
    await publishCtx.drain();

    expect(publishResponse.status).toBe(200);
    expect(await readJson(publishResponse)).toMatchObject({
      package_name: "minttea",
      package_version: "0.4.2",
    });

    await applyMetadataMigrations(db as unknown as D1Database);
    expect(await readPackageClaim(db as unknown as D1Database, "minttea")).toMatchObject({
      owner_github_login: "someoneelse",
      package_locator: "",
    });
  });

  test("logging in after publish refreshes package overview owner avatars", async () => {
    const { env } = makeEnv({
      GITHUB_OAUTH_CLIENT_ID: "github-client-id",
      GITHUB_OAUTH_CLIENT_SECRET: "github-client-secret",
      PKGS_WEB_BASE_URL: "https://pkgs.ml",
    });
    const initialCookie = await loginAsGitHubUser(env, "leostera");
    const publishToken = await createPublishToken(env, initialCookie, "publish");
    const archive = await makeTarGz({
      "riot.toml": [
        "[package]",
        'name = "minttea"',
        'version = "0.4.2"',
        "public = true",
        'description = "Terminal UI toolkit for Riot"',
        'license = "MIT"',
      ].join("\n"),
    }, "");

    await withMockedFetch(async (input) => {
      const url = new URL(toRequestUrl(input));
      if (url.origin === "https://github.com" && url.pathname === "/login/oauth/access_token") {
        return Response.json({
          access_token: "token-for-leostera",
          token_type: "bearer",
        });
      }
      if (url.origin === "https://api.github.com" && url.pathname === "/user") {
        return Response.json({
          id: 42,
          login: "leostera",
          name: "Leo Stera",
          avatar_url: "https://avatars.githubusercontent.com/u/42",
        });
      }
      if (url.origin === "https://api.github.com" && url.pathname === "/user/emails") {
        return Response.json([
          {
            email: "leo@example.com",
            primary: true,
            verified: true,
          },
        ]);
      }

      throw new Error(`Unexpected fetch to ${url.toString()}`);
    }, async () => {
      const publishCtx = new FakeExecutionContext();
      const publishResponse = await handleRequest(
        new Request("https://registry.test/v1/publish", {
          method: "POST",
          headers: {
            authorization: `Bearer ${publishToken}`,
            "content-type": "application/gzip",
          },
          body: archive,
        }),
        env,
        publishCtx,
      );
      await publishCtx.drain();
      expect(publishResponse.status).toBe(200);

      const overviewBeforeLoginCtx = new FakeExecutionContext();
      const overviewBeforeLogin = await handleRequest(
        new Request("https://registry.test/v1/views/packages/minttea/overview"),
        env,
        overviewBeforeLoginCtx,
      );
      await overviewBeforeLoginCtx.drain();
      expect(overviewBeforeLogin.status).toBe(200);
      const beforePayload = await readJson(overviewBeforeLogin) as {
        owner_github_login: string;
        owner_github_avatar_url?: string;
      };
      expect(beforePayload.owner_github_login).toBe("leostera");
      expect(beforePayload.owner_github_avatar_url).toBeUndefined();

      const refreshedLoginCookie = await loginAsGitHubUser(env, "leostera", {
        avatarUrl: "https://avatars.githubusercontent.com/u/42",
      });
      expect(refreshedLoginCookie).toContain("pkgs_session=");

      const overviewAfterLoginCtx = new FakeExecutionContext();
      const overviewAfterLogin = await handleRequest(
        new Request("https://registry.test/v1/views/packages/minttea/overview"),
        env,
        overviewAfterLoginCtx,
      );
      await overviewAfterLoginCtx.drain();
      expect(overviewAfterLogin.status).toBe(200);
      const afterPayload = await readJson(overviewAfterLogin) as {
        owner_github_login: string;
        owner_github_avatar_url?: string;
      };
      expect(afterPayload.owner_github_login).toBe("leostera");
      expect(afterPayload.owner_github_avatar_url).toBe("https://cdn.pkgs.ml/avatars/leostera");
    });
  });
});

async function loginAsGitHubUser(
  env: ReturnType<typeof makeEnv>["env"],
  login: string,
  options?: { avatarUrl?: string },
): Promise<string> {
  const startCtx = new FakeExecutionContext();
  const startResponse = await handleRequest(
    new Request(`https://registry.test/v1/auth/github/start?return_to=${encodeURIComponent(`/u/${login}/tokens`)}`),
    env,
    startCtx,
  );
  await startCtx.drain();

  const authorizationUrl = new URL(startResponse.headers.get("location") ?? "");
  const state = authorizationUrl.searchParams.get("state");
  if (state === null) {
    throw new Error("OAuth state was missing.");
  }

  const callbackCtx = new FakeExecutionContext();
  const callbackResponse = await withMockedFetch(async (input) => {
    const url = new URL(toRequestUrl(input));
    if (url.origin === "https://github.com" && url.pathname === "/login/oauth/access_token") {
      return Response.json({
        access_token: `token-for-${login}`,
        token_type: "bearer",
      });
    }

    if (url.origin === "https://api.github.com" && url.pathname === "/user") {
      return Response.json({
        id: Math.floor(Math.random() * 10_000),
        login,
        name: login,
        avatar_url: options?.avatarUrl,
      });
    }

    if (url.origin === "https://api.github.com" && url.pathname === "/user/emails") {
      return Response.json([
        {
          email: `${login}@example.com`,
          primary: true,
          verified: true,
        },
      ]);
    }

    if (options?.avatarUrl !== undefined && url.toString() === options.avatarUrl) {
      return new Response("avatar", {
        status: 200,
        headers: {
          "content-type": "image/png",
        },
      });
    }

    throw new Error(`Unexpected fetch to ${url.toString()}`);
  }, async () => {
    return await handleRequest(
      new Request(
        `https://registry.test/v1/auth/github/callback?code=oauth-code&state=${encodeURIComponent(state)}`,
      ),
      env,
      callbackCtx,
    );
  });
  await callbackCtx.drain();

  const cookie = callbackResponse.headers.get("set-cookie");
  if (cookie === null) {
    throw new Error("Session cookie was missing.");
  }

  return cookie;
}

async function createPublishToken(
  env: ReturnType<typeof makeEnv>["env"],
  cookie: string,
  name: string,
): Promise<string> {
  const ctx = new FakeExecutionContext();
  const response = await handleRequest(
    new Request("https://registry.test/v1/me/tokens", {
      method: "POST",
      headers: {
        cookie,
        accept: "application/json",
        "content-type": "application/json",
      },
      body: JSON.stringify({ name }),
    }),
    env,
    ctx,
  );
  await ctx.drain();

  const payload = (await readJson(response)) as { plaintext_token: string };
  return payload.plaintext_token;
}

async function readJson(response: Response): Promise<unknown> {
  return await response.json();
}

function toRequestUrl(input: RequestInfo | URL): string {
  if (input instanceof Request) {
    return input.url;
  }

  return typeof input === "string" ? input : input.toString();
}
