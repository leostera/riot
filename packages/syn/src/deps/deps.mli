open Std
open Std.Data

module Env: sig
  type t
  val empty: t
end

type t

type parse_error =
  | Parse_diagnostics of Diagnostic.t list
  | Cst_builder_error of Cst_builder.error

val modules: t -> string list

val env: t -> Env.t

val to_json: t -> Json.t

val of_cst: ?env:Env.t -> Cst.source_file -> (t, Cst_builder.error) result

val of_parse_result: ?env:Env.t -> Parser.parse_result -> (t, parse_error) result
