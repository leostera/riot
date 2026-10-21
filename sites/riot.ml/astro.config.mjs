import { defineConfig } from "astro/config";
import react from "@astrojs/react";
import tailwindcss from "@tailwindcss/vite";

export default defineConfig({
  site: "https://riot.ml",
  integrations: [react()],
  vite: {
    plugins: [tailwindcss()],
    server: {
      host: true,
    },
  },
});
