open Std

(** Encode a prototype check result as structured JSON. *)
val to_json: Check_result.t -> Data.Json.t

(** Render a prototype check result into text.

    This is now a thin debug helper over {!to_json}; snapshot tests should
    prefer {!to_json} plus [Std.Test.Snapshot.assert_json]. *)
val render_report: Check_result.t -> string
