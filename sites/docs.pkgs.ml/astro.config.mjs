// @ts-check
import { defineConfig } from "astro/config";
import starlight from "@astrojs/starlight";

export default defineConfig({
  site: "https://docs.pkgs.ml",
  integrations: [
    starlight({
      title: "pkgs Docs",
      description: "Generated package documentation surface for Riot packages published through pkgs.ml.",
      social: [
        { icon: "github", label: "GitHub", href: "https://github.com/leostera/riot" },
      ],
      sidebar: [
        {
          label: "Overview",
          items: [
            { label: "What lives here", slug: "overview/what-lives-here" },
          ],
        },
      ],
    }),
  ],
});
