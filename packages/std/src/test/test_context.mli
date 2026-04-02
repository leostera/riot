open Global

(** Fixture metadata attached to a test context when a case comes from a shared
    fixture runner. *)
type fixture = {
  path: Path.t;
  relpath: string;
  name: string;
}

(** Stable per-test metadata supplied by the shared test runner.

    This context intentionally carries enough identity to support snapshot
    storage, fixture-backed cases, and future temp-artifact helpers without
    relying on hidden global state.
*)
type t = {
  suite_name: string;
  test_name: string;
  test_index: int;
  source_file: string option;
  binary_path: string option;
  workspace_root: Path.t option;
  package_name: string option;
  fixture: fixture option;
}

val with_fixture: t -> fixture -> t
