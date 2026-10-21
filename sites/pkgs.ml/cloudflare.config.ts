import { bindings, defineConfig } from "cf/config";

/**
 * Secret-like files were detected but not read or migrated: .env.example. Only `secrets.required` entries are migrated.
 * @see https://developers.cloudflare.com/workers/configuration/secrets/
 */

export default defineConfig({
	worker: {
		name: "riot-pkgs-web",
		compatibilityDate: "2026-03-27",
		compatibilityFlags: [
			"nodejs_compat",
		],
		entrypoint: "@astrojs/cloudflare/entrypoints/server",
		workersDev: false,
		observability: {
			enabled: true,
			logs: {
				enabled: true,
				headSamplingRate: 1,
				persist: true,
				invocationLogs: true,
			},
			traces: {
				enabled: true,
				persist: true,
				headSamplingRate: 1,
			},
		},
		domains: [
			"pkgs.ml",
		],
		env: {
			PUBLIC_INDEX_BASE_URL: bindings.text("https://cdn.pkgs.ml"),
			PUBLIC_INDEX_BASE_PATH: bindings.text("index/v1"),
			ASSETS: bindings.assets(),
		},
	},
});
