# docs AGENTS

`docs/` contains project-facing design and narrative documents.

## Routing

- `docs/rfds/`: request for discussion documents, architecture snapshots, and design records
- `docs/agents/`: shipped user-facing Codex skills for working with Riot projects
- `docs/contributing/`: contributor-facing operational docs for repeatable release, packaging, and maintenance tasks

## Rules

1. Keep docs descriptive unless the file is explicitly a proposal.
2. Prefer present-tense system descriptions in snapshot RFDs.
3. Avoid historical cleanup language unless the document is specifically about a migration.
4. Snapshot RFDs are historical records of the system at the time they were written. Do not refresh an old snapshot to match later behavior; write a new snapshot RFD if the system has changed enough to warrant one.
5. When architecture or package boundaries change, update the affected living docs and proposals in the same change, but only update snapshot RFDs by writing a new snapshot.
6. Preserve the existing voice of a document instead of normalizing everything to contributor docs.
7. Daily-driver v0.1 proposals start at RFD0044. Keep per-run validation results and execution checklists in root `TODO.md`; do not mark a proposal implemented merely because one acceptance check passes.

## Validate

1. Read the edited markdown for tone and structure.
2. Make sure package and file names match the current repo.
