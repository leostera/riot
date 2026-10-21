/**
 * Presentation for generated package docs served under /p/<package>/<version>/.
 *
 * `riot doc` emits stable class names (docs-shell, sidebar, item-detail, ...)
 * and links `_shared/doc.css`. The docs worker always answers that path with
 * this stylesheet, so every published version picks up the current Riot theme
 * without regenerating docs. Colors mirror the Kumo-based Riot theme used by
 * riot.ml and pkgs.ml, and follow the reader's system color scheme.
 */
export const GENERATED_DOCS_CSS = `
@import url("https://fonts.googleapis.com/css2?family=Inter:wght@400..600&family=JetBrains+Mono:wght@400..600&display=swap");

:root {
  color-scheme: light dark;
  --canvas: light-dark(#fbf8f2, #0e0d10);
  --base: light-dark(#fffdf9, #151317);
  --elevated: light-dark(#f6f1e8, #1b191e);
  --recessed: light-dark(#f1ebe0, #0b0a0d);
  --tint: light-dark(#f3ede3, #1f1c23);
  --hairline: light-dark(rgb(21 19 23 / 0.1), rgb(255 248 237 / 0.09));
  --line: light-dark(rgb(21 19 23 / 0.16), rgb(255 248 237 / 0.15));
  --strong: light-dark(#151317, #f6f1ea);
  --text: light-dark(#2a262d, #e4ded6);
  --subtle: light-dark(#6b6470, #a49ca8);
  --inactive: light-dark(#8f8894, #7c7580);
  --brand: light-dark(#e11d3f, #ff4d68);
  --link: light-dark(#a8420f, #f4a06a);
  --k-type: light-dark(#7c3aed, #c4b5fd);
  --k-record: light-dark(#0f766e, #5eead4);
  --k-variant: light-dark(#b45309, #fcd34d);
  --k-val: light-dark(#1d4ed8, #93c5fd);
  --k-fn: light-dark(#c01634, #ff8597);
  --k-module: light-dark(#15803d, #86efac);
  --sans: "Inter", ui-sans-serif, system-ui, -apple-system, "Segoe UI", sans-serif;
  --mono: "JetBrains Mono", ui-monospace, SFMono-Regular, Menlo, monospace;
}

* { box-sizing: border-box; }
html { -webkit-text-size-adjust: 100%; scroll-behavior: smooth; background: var(--canvas); }
body {
  margin: 0;
  background: var(--canvas);
  color: var(--text);
  font-family: var(--sans);
  font-size: 14px;
  line-height: 1.6;
  -webkit-font-smoothing: antialiased;
}
a { color: var(--link); text-decoration: none; }
a:hover { text-decoration: underline; text-underline-offset: 3px; text-decoration-thickness: 1px; }
code, pre, kbd { font-family: var(--mono); font-variant-ligatures: none; }
::selection { background: var(--brand); color: #fff; }
*:focus-visible { outline: 2px solid var(--brand); outline-offset: 2px; }

.docs-shell {
  display: grid;
  grid-template-columns: 280px minmax(0, 1fr);
  min-height: 100vh;
}

/* Sidebar */
.sidebar {
  position: sticky;
  top: 0;
  height: 100vh;
  overflow-y: auto;
  padding: 24px 16px 32px;
  border-right: 1px solid var(--hairline);
  background: var(--base);
  scrollbar-width: thin;
  scrollbar-color: var(--line) transparent;
}
.sidebar-brand {
  display: inline-flex;
  align-items: center;
  gap: 6px;
  margin: 0 8px 20px;
  color: var(--subtle);
  font-size: 13px;
}
.sidebar-brand::before { content: "\\2190"; }
.sidebar-brand:hover { color: var(--strong); text-decoration: none; }
.sidebar-title {
  margin: 0 8px;
  color: var(--strong);
  font-family: var(--mono);
  font-size: 18px;
  font-weight: 600;
  line-height: 1.25;
  overflow-wrap: anywhere;
}
.sidebar-meta {
  margin: 4px 8px 0;
  color: var(--subtle);
  font-family: var(--mono);
  font-size: 12px;
  overflow-wrap: anywhere;
}
.sidebar-filter { position: relative; margin: 16px 0 20px; }
.sidebar-filter input {
  width: 100%;
  height: 32px;
  padding: 0 32px 0 10px;
  border: 1px solid var(--line);
  border-radius: 8px;
  background: var(--canvas);
  color: var(--strong);
  font: inherit;
  font-size: 13px;
}
.sidebar-filter input::placeholder { color: var(--inactive); }
.sidebar-filter input:focus { outline: none; border-color: var(--brand); }
.sidebar-filter kbd {
  position: absolute;
  top: 50%;
  right: 8px;
  transform: translateY(-50%);
  padding: 0 5px;
  border: 1px solid var(--line);
  border-radius: 4px;
  color: var(--subtle);
  font-size: 11px;
  line-height: 16px;
}
.sidebar-group { margin-top: 20px; }
.sidebar-group h2 {
  margin: 0 8px 6px;
  color: var(--subtle);
  font-size: 12px;
  font-weight: 500;
}
.sidebar-group ul { list-style: none; margin: 0; padding: 0; }
.sidebar-group a {
  display: block;
  padding: 3px 8px;
  border-radius: 6px;
  color: var(--text);
  font-family: var(--mono);
  font-size: 13px;
  overflow-wrap: anywhere;
}
.sidebar-group a:hover { background: var(--tint); color: var(--strong); text-decoration: none; }
.sidebar-group a.is-active { background: var(--tint); color: var(--brand); }
.sidebar-group a.is-hidden { display: none; }
.sidebar-group .empty-result { display: none; padding: 3px 8px; color: var(--inactive); font-size: 13px; }
.sidebar-group.has-no-matches .empty-result { display: block; }

/* Content */
.content {
  min-width: 0;
  width: 100%;
  max-width: 960px;
  padding: 40px 48px 96px;
}
.page-header { margin-bottom: 32px; }
.breadcrumbs { margin-bottom: 8px; color: var(--subtle); font-family: var(--mono); font-size: 13px; }
.breadcrumbs a { color: var(--subtle); }
.breadcrumbs a:hover { color: var(--strong); }
.section-header {
  display: flex;
  align-items: baseline;
  justify-content: space-between;
  gap: 16px;
}
.eyebrow { display: none; }
.page-title {
  margin: 0;
  color: var(--strong);
  font-family: var(--mono);
  font-size: 28px;
  font-weight: 600;
  line-height: 1.2;
  overflow-wrap: anywhere;
}
.page-header .section-header > a {
  flex: none;
  padding: 2px 8px;
  border: 1px solid var(--line);
  border-radius: 6px;
  color: var(--subtle);
  font-family: var(--mono);
  font-size: 12px;
}
.page-header .section-header > a:hover { color: var(--strong); text-decoration: none; background: var(--tint); }
.summary-block { margin-top: 12px; }
.summary-toggle { display: none; }
.summary-toggle::-webkit-details-marker { display: none; }
.module-docstring { max-width: 72ch; color: var(--text); font-size: 15px; }
.module-docstring > :first-child { margin-top: 0; }
.module-docstring h1, .module-docstring h2, .module-docstring h3 {
  margin: 28px 0 8px;
  color: var(--strong);
  font-weight: 600;
  line-height: 1.3;
}
.module-docstring h1 { font-size: 20px; }
.module-docstring h2 { font-size: 18px; }
.module-docstring h3 { font-size: 16px; }

/* Sections */
.section-card { margin: 40px 0; scroll-margin-top: 24px; }
.section-card > .section-header {
  margin-bottom: 12px;
  padding-bottom: 8px;
  border-bottom: 1px solid var(--hairline);
}
.section-card > .section-header h2 {
  margin: 0;
  color: var(--strong);
  font-size: 18px;
  font-weight: 600;
}
.section-note { color: var(--subtle); font-family: var(--mono); font-size: 12px; }
.empty-state { color: var(--subtle); }

.item-list {
  list-style: none;
  margin: 0;
  padding: 0;
  overflow: hidden;
  border: 1px solid var(--hairline);
  border-radius: 10px;
  background: var(--base);
}
.item-row {
  display: grid;
  grid-template-columns: minmax(160px, 240px) minmax(0, 1fr);
  gap: 16px;
  padding: 10px 14px;
  border-bottom: 1px solid var(--hairline);
}
.item-row:last-child { border-bottom: 0; }
.item-row .item-name { color: var(--strong); font-family: var(--mono); font-size: 13px; font-weight: 500; overflow-wrap: anywhere; }
a.item-name:hover { color: var(--brand); text-decoration: none; }
.item-row .item-summary { color: var(--subtle); }
.item-row.is-filter-hidden, .item-detail.is-filter-hidden { display: none; }

/* Item details */
.item-detail-list { display: flex; flex-direction: column; gap: 0; }
.item-detail {
  padding: 20px 0;
  border-top: 1px solid var(--hairline);
  scroll-margin-top: 24px;
}
.item-detail:first-child { border-top: 0; padding-top: 4px; }
.item-detail-title {
  display: flex;
  flex-wrap: wrap;
  align-items: baseline;
  gap: 4px 8px;
  margin: 0;
  font-family: var(--mono);
  font-size: 14px;
  font-weight: 400;
  line-height: 1.5;
}
.item-detail-title a { color: var(--strong); font-weight: 600; }
.item-detail-title a:hover { color: var(--brand); text-decoration: none; }
.item-detail-title .anchor { color: var(--inactive); font-weight: 400; opacity: 0; }
.item-detail:hover .anchor, .item-detail-title .anchor:focus-visible { opacity: 1; }
.item-detail-signature { color: var(--subtle); overflow-wrap: anywhere; }
.kind {
  display: inline-flex;
  align-items: center;
  padding: 0 6px;
  border-radius: 4px;
  background: var(--tint);
  font-size: 11px;
  font-weight: 500;
  line-height: 18px;
}
.kind-type { color: var(--k-type); }
.kind-record { color: var(--k-record); }
.kind-variant { color: var(--k-variant); }
.kind-val { color: var(--k-val); }
.kind-fn { color: var(--k-fn); }
.kind-module { color: var(--k-module); }

.item-docstring, .item-subitem-docstring { max-width: 72ch; margin-top: 8px; color: var(--text); }
.item-docstring p, .module-docstring p { margin: 0 0 10px; }
.item-docstring ul, .module-docstring ul, .item-subitem-docstring ul { margin: 0 0 10px; padding-left: 20px; }
.item-docstring li::marker, .module-docstring li::marker { color: var(--inactive); }
.item-subitem-docstring > :first-child { margin-top: 0; }
.item-subitem-docstring > :last-child { margin-bottom: 0; }
.item-subitem-docstring p { margin: 0; color: var(--subtle); }
.item-subitem-docstring p + p { margin-top: 4px; }
.item-subitem-docstring li { color: var(--subtle); }

:not(pre) > code {
  padding: 1px 5px;
  border-radius: 4px;
  background: var(--tint);
  color: var(--strong);
  font-size: 0.9em;
  overflow-wrap: anywhere;
}
pre, .item-snippet {
  margin: 12px 0;
  padding: 12px 14px;
  overflow-x: auto;
  border: 1px solid var(--hairline);
  border-radius: 8px;
  background: var(--recessed);
  color: var(--strong);
  font-size: 13px;
  line-height: 1.6;
}
pre code, .item-snippet code { padding: 0; background: none; font-size: inherit; }

.item-subsections { margin-top: 12px; }
.item-subsection + .item-subsection { margin-top: 12px; }
.item-subsection h4 { margin: 0 0 6px; color: var(--subtle); font-size: 12px; font-weight: 500; }
.item-subitem-list { display: flex; flex-direction: column; gap: 6px; }
.item-subitem { padding-left: 12px; border-left: 2px solid var(--hairline); }
.item-subitem-signature { color: var(--strong); font-family: var(--mono); font-size: 13px; overflow-wrap: anywhere; }

/* Prism tokens */
.token.keyword, .token.builtin { color: var(--k-fn); }
.token.class-name { color: var(--k-type); }
.token.string, .token.char { color: var(--k-record); }
.token.comment { color: var(--inactive); font-style: italic; }
.token.number, .token.constant { color: var(--k-variant); }
.token.variable { color: var(--k-val); }
.token.operator, .token.punctuation { color: var(--subtle); }

@media (max-width: 880px) {
  .docs-shell { display: block; }
  .sidebar {
    position: static;
    height: auto;
    max-height: none;
    border-right: 0;
    border-bottom: 1px solid var(--hairline);
  }
  .sidebar-group ul { display: flex; flex-wrap: wrap; gap: 2px; }
  /* Symbol lists repeat the page body; keep navigation short on phones. */
  .sidebar-group[data-filterable] { display: none; }
  .content { padding: 24px 16px 64px; }
  .item-row { grid-template-columns: minmax(0, 1fr); gap: 2px; }
  .page-title { font-size: 22px; }
}
`;

/** Preview notice injected at the top of generated docs HTML. */
export const TECH_DEMO_BANNER_HTML = `
<style data-pkgs-docs-tech-demo-banner-style>
.pkgs-docs-tech-demo-banner {
  box-sizing: border-box;
  width: 100%;
  border-bottom: 1px solid light-dark(rgb(21 19 23 / 0.1), rgb(255 248 237 / 0.09));
  background: light-dark(#f6f1e8, #1b191e);
  color: light-dark(#151317, #f6f1ea);
  font: 13px/1.4 "Inter", ui-sans-serif, system-ui, -apple-system, "Segoe UI", sans-serif;
  color-scheme: light dark;
}
.pkgs-docs-tech-demo-banner__inner {
  display: flex;
  flex-wrap: wrap;
  align-items: center;
  justify-content: center;
  gap: 4px 8px;
  margin: 0 auto;
  padding: 8px 16px;
  text-align: center;
}
.pkgs-docs-tech-demo-banner__label {
  padding: 1px 8px;
  border: 1px dashed light-dark(rgb(21 19 23 / 0.16), rgb(255 248 237 / 0.15));
  border-radius: 999px;
  color: light-dark(#c01634, #ff6b82);
  font-size: 12px;
  font-weight: 500;
}
</style>
<aside class="pkgs-docs-tech-demo-banner" data-pkgs-docs-tech-demo-banner aria-label="Tech demo notice">
  <div class="pkgs-docs-tech-demo-banner__inner">
    <strong class="pkgs-docs-tech-demo-banner__label">tech-demo</strong>
    <span>Generated package docs are in preview. Routes and output may change before release.</span>
  </div>
</aside>`;
