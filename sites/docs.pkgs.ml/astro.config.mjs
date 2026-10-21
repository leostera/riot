// @ts-check
import { defineConfig } from "astro/config";
import starlight from "@astrojs/starlight";

export default defineConfig({
  site: "https://docs.pkgs.ml",
  integrations: [
    starlight({
      title: "docs.pkgs.ml",
      customCss: ["./src/styles/riot.css"],
      description: "Generated API documentation for OCaml packages published on pkgs.ml.",
      favicon: "/favicon.svg",
      social: [
        { icon: "github", label: "GitHub", href: "https://github.com/leostera/riot" },
      ],
      sidebar: [
        {
          label: "Overview",
          items: [
            { label: "How docs are built", slug: "overview/what-lives-here" },
          ],
        },
      ],
    }),
  ],
});
