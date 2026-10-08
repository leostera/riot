# Riot ML

VS Code integration for Riot.

This extension stays intentionally thin and delegates editor behavior to Riot
itself.

When Riot is available, the extension prefers `riot lsp stdio` for editor
formatting and diagnostics. If that is unavailable, it falls back to the older
CLI-based formatter and diagnostics path.

## Features

- `Riot: Install Riot` installs a managed Riot binary for the extension by
  fetching `https://get.riot.ml`
- shows startup notifications for the resolved Riot version and whether a newer
  published Riot is available
- contributes bundled language support for `*.ml` and `*.mli`
- formats `*.ml` and `*.mli` files through `riot lsp stdio` when available
- surfaces parser and lint diagnostics through `riot lsp stdio` when available
- falls back to `riot fmt` / `riot fix` CLI integration when the LSP server is
  unavailable
- adds `Riot: Build Workspace` and `Riot: Test Workspace` commands
- adds `Riot: Add Package` and `Riot: Remove Package` commands that shell out to
  `riot add` / `riot rm` against the nearest Riot manifest for the active file
- contributes VS Code tasks for `riot build` and `riot test`

## Requirements

- macOS or Linux
- a Riot workspace rooted at `riot.toml` for build and test commands

## Extension Settings

This extension contributes the following settings:

- `riot.path`: explicit Riot executable path
- `riot.installUrl`: installer URL used by `Riot: Install Riot`
- `riot.latestMetadataUrl`: release metadata URL used for startup upgrade checks
- `riot.formatOnSave`: format `*.ml` and `*.mli` on save
- `riot.diagnostics.enabled`: enable Riot diagnostics
- `riot.diagnostics.runFix`: include `riot-fix` lint diagnostics

## Development

This extension uses Bun for local development:

```sh
bun run compile
bun run watch
```

To package the version of the extension under local development as
a [VSIX](https://learn.microsoft.com/en-us/visualstudio/extensibility/anatomy-of-a-vsix-package?view=visualstudio)
(to make it easier to deploy locally for testing or to, ultimately,
publish to the marketplace):

```sh
bun run vscode:vsix
```

## Known Issues

- build and test integration still shells out through VS Code tasks instead of
  the LSP
