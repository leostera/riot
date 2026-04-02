open Global

(** Shared fixture discovery for [Std.Test] suites.

    [cases ~dir ~run] recursively discovers fixture inputs under [dir], skips
    adjacent snapshot artifacts such as [.expected] and [.expected.new], and
    turns each remaining file into a regular [Std.Test.case].

    The callback receives both the ordinary test context and fixture-specific
    metadata, allowing it to reuse [Std.Test.Snapshot] without inventing a
    package-local harness.
*)

type ctx = {
  test: Test_context.t;
  fixture_path: Path.t;
  fixture_relpath: string;
  fixture_name: string;
}

val cases:
  dir:string ->
  run:(ctx -> (unit, string) result) ->
  Test_case.t list
