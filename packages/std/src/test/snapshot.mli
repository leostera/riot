open Global

(** File-backed and inline snapshot assertions for [Std.Test].

    External snapshots compare a rendered value against an approved artifact on
    disk. When the approved artifact is missing or differs, the assertion
    writes a pending [*.expected.new] candidate and fails. Approved snapshots
    are never mutated by ordinary test execution.

    Non-fixture snapshots live under:

    {[
      .riot/snapshots/<package>/<suite>/<test>.expected
    ]}

    Fixture-backed snapshots live adjacent to the fixture input, replacing the
    input extension with [.expected].
*)
val assert_text: ctx:Test_context.t -> actual:string -> (unit, string) result

(** Compare opaque text against an external snapshot derived from [ctx]. *)
val assert_json: ctx:Test_context.t -> actual:Data.Json.t -> (unit, string) result

(** Compare canonical JSON against an external snapshot derived from [ctx]. *)
val assert_with: ctx:Test_context.t -> render:('a -> string) -> actual:'a -> (unit, string) result

(** Render a value to text with a custom function before snapshotting it. *)
val assert_inline_text: ctx:Test_context.t -> actual:string -> expected:string -> (unit, string) result

(** Compare two inline strings without creating external snapshot artifacts. *)
val assert_inline_json:
  ctx:Test_context.t -> actual:Data.Json.t -> expected:Data.Json.t -> (unit, string) result

(** Compare two inline JSON values after canonicalizing object-key order and
    rendering them through [Std.Data.Json.to_string_pretty]. *)
