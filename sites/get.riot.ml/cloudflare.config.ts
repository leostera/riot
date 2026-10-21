import { bindings, defineConfig } from "cf/config";

export default defineConfig({
	worker: {
		name: "riot-get",
		compatibilityDate: "2026-04-02",
		entrypoint: "src/main.ts",
		workersDev: false,
		observability: {
			enabled: true,
			headSamplingRate: 1,
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
			"get.riot.ml",
		],
		env: {
			RIOT_INSTALL_SCRIPT_URL: bindings.text("https://cdn.pkgs.ml/riot/install.sh"),
		},
	},
});
