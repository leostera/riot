open Std

module Error: sig
  type t = {
    message : string;
    span : Syn.Ceibo.Span.t option;
  }
  val make : ?span:Syn.Ceibo.Span.t -> string -> t

  val message : t -> string
end

module Token_stream: sig
  type t
  val source : t -> string

  val span : t -> Syn.Ceibo.Span.t option
end

module Result: sig
  type t = {
    output : Token_stream.t;
    diagnostics : Error.t list;
  }
end

module Environment: sig
  type t
  val create : filename:Path.t -> string -> t

  val filename : t -> Path.t

  val source : t -> string

  val parsed : t -> Syn.Parser.parse_result

  val source_of_span : t -> Syn.Ceibo.Span.t -> string

  val span_of_node : (Syn.SyntaxKind.t, string) Syn.Ceibo.Red.syntax_node -> Syn.Ceibo.Span.t option

  val span_of_node_with_leading_trivia :
    (Syn.SyntaxKind.t, string) Syn.Ceibo.Red.syntax_node -> Syn.Ceibo.Span.t option

  val source_of_node : t -> (Syn.SyntaxKind.t, string) Syn.Ceibo.Red.syntax_node -> string

  val source_of_node_with_leading_trivia :
    t -> (Syn.SyntaxKind.t, string) Syn.Ceibo.Red.syntax_node -> string
end

module Parser: sig
  type invocation = {
    callee_path : string list;
    span : Syn.Ceibo.Span.t;
    body : Token_stream.t;
  }
  val collect_invocations : Environment.t -> (invocation list, Error.t) result
end

module Parse: sig
  val unwrap_grouping :
    (Syn.SyntaxKind.t, string) Syn.Ceibo.Red.syntax_node ->
    (Syn.SyntaxKind.t, string) Syn.Ceibo.Red.syntax_node

  val expr : Token_stream.t -> ((Syn.SyntaxKind.t, string) Syn.Ceibo.Red.syntax_node, Error.t) result

  val expr_arguments :
    Token_stream.t -> ((Syn.SyntaxKind.t, string) Syn.Ceibo.Red.syntax_node list, Error.t) result
end

module Provider: sig
  type macro_fn = Token_stream.t -> Result.t
  type exported_macro
  type t
  val fn : string -> macro_fn -> exported_macro

  val v : module_path:string list -> exported_macro list -> t

  val module_path : t -> string list
end

module Runner: sig
  type plan

  val providers_hash : Riot_model.Macro_provider.t list -> string

  val plan :
    workspace_root:Path.t -> target_dir_root:Path.t -> Riot_model.Macro_provider.t list -> plan

  val workspace_root : plan -> Path.t

  val binary_path : plan -> Path.t

  val materialize :
    workspace_root:Path.t -> target_dir_root:Path.t -> Riot_model.Macro_provider.t list -> plan

  val run_file :
    workspace_root:Path.t ->
    target_dir_root:Path.t ->
    Riot_model.Macro_provider.t list ->
    input_path:Path.t ->
    output_path:Path.t ->
    (unit, string) result
end

module Format: sig
  val expand : Token_stream.t -> Result.t

  val provider : unit -> Provider.t
end

module Format_parser: sig
  type hole =
    | Next_arg_to_string
    | Var_to_string of string
  type item =
    | String of string
    | Hole of hole
  type t = item list
  val parse_literal : literal_text:string -> span:Syn.Ceibo.Span.t -> (t, Error.t) result
end

module Validator: sig
  val validate_source : filename:Path.t -> string -> (unit, Error.t) result
end

module Expander: sig
  type expansion = {
    source : string;
    changed : bool;
  }
  val builtin_providers : unit -> Provider.t list

  val expand_environment : ?providers:Provider.t list -> Environment.t -> (expansion, Error.t) result

  val expand_source :
    ?providers:Provider.t list -> filename:Path.t -> string -> (expansion, Error.t) result
end

type error = Error.t = {
  message : string;
  span : Syn.Ceibo.Span.t option;
}
type expansion = Expander.expansion = {
  source : string;
  changed : bool;
}
val error_message : error -> string

val builtin_providers : unit -> Provider.t list

val expand_source : ?providers:Provider.t list -> filename:Path.t -> string -> (expansion, error) result
