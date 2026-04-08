open Std

(** Public API for Riot's procedural macro prototype.

    The package is intentionally split into two layers:
    - parser-backed source rewriting utilities used by the planner
    - provider/runner utilities used to execute package-scoped procedural macros

    The core ABI stays token-stream based even when a macro chooses to parse its
    input through [syn]. *)
module Error: sig
  (** A macro failure or warning tied to an optional source span. *)
  type t = {
    message: string;
    span: Syn.Ceibo.Span.t option;
  }

  (** Construct a diagnostic. *)
  val make: ?span:Syn.Ceibo.Span.t -> string -> t

  (** Human-readable diagnostic text without span formatting. *)
  val message: t -> string
end

module Token_stream: sig
  (** Opaque token slice passed to and returned from procedural macros.

      Token streams preserve the original source text and the source span that
      produced it. Procedural macros can parse that text further if they want
      OCaml structure, but the ABI itself stays textual/token-oriented. *)
  type t

  (** Raw source text covered by this token stream. *)
  val source: t -> string

  (** Original source span, when the stream came from parsed syntax. *)
  val span: t -> Syn.Ceibo.Span.t option
end

module Result: sig
  (** Procedural macro result: rewritten tokens plus any diagnostics emitted by
      the macro implementation. *)
  type t = {
    output: Token_stream.t;
    diagnostics: Error.t list;
  }
end

module Environment: sig
  (** Parsed source plus helper accessors used while collecting and rewriting
      macro invocations. *)
  type t

  (** Parse a source file once and retain enough context to recover original
      token text for rewritten nodes later on. *)
  val create: filename:Path.t -> string -> t

  (** Source file path associated with this environment. *)
  val filename: t -> Path.t

  (** Original source text. *)
  val source: t -> string

  (** Raw [syn] parse result. *)
  val parsed: t -> Syn.Parser.parse_result

  (** Slice original source text back out from a span. *)
  val source_of_span: t -> Syn.Ceibo.Span.t -> string

  (** Recover the span for a syntax node without leading trivia. *)
  val span_of_node: (Syn.SyntaxKind.t, string) Syn.Ceibo.Red.syntax_node -> Syn.Ceibo.Span.t option

  (** Recover the span for a syntax node including leading trivia.

      This is useful when a rewrite needs to preserve indentation or comments
      that conceptually belong to the rewritten node. *)
  val span_of_node_with_leading_trivia:
    (Syn.SyntaxKind.t, string) Syn.Ceibo.Red.syntax_node -> Syn.Ceibo.Span.t option

  (** Recover the original source text for a syntax node. *)
  val source_of_node: t -> (Syn.SyntaxKind.t, string) Syn.Ceibo.Red.syntax_node -> string

  (** Recover the original source text for a syntax node including leading
      trivia. *)
  val source_of_node_with_leading_trivia:
    t -> (Syn.SyntaxKind.t, string) Syn.Ceibo.Red.syntax_node -> string
end

module Parser: sig
  (** One parsed function-like macro invocation.

      [callee_path] already reflects path qualification such as
      [Sqlx.query! ...], while [body] preserves the body as a token stream so
      the macro can decide whether to parse it further. *)
  type invocation = {
    callee_path: string list;
    span: Syn.Ceibo.Span.t;
    body: Token_stream.t;
  }

  (** Collect every [MACRO_EXPR] in the parsed source file. *)
  val collect_invocations: Environment.t -> (invocation list, Error.t) result
end

module Parse: sig
  (** Strip grouping syntax such as redundant parentheses before downstream
      inspection. *)
  val unwrap_grouping:
    (Syn.SyntaxKind.t, string) Syn.Ceibo.Red.syntax_node ->
    (Syn.SyntaxKind.t, string) Syn.Ceibo.Red.syntax_node

  (** Parse a token stream as one OCaml expression. *)
  val expr: Token_stream.t -> ((Syn.SyntaxKind.t, string) Syn.Ceibo.Red.syntax_node, Error.t) result

  (** Parse a token stream as a macro body that semantically behaves like
      call-style arguments.

      The current [format!] prototype uses this to accept both:
      - [Macro.format! "hello {}" name]
      - [Macro.format!("hello {}", name)] *)
  val expr_arguments:
    Token_stream.t -> ((Syn.SyntaxKind.t, string) Syn.Ceibo.Red.syntax_node list, Error.t) result
end

module Provider: sig
  (** Macro function ABI used by runtime providers. *)
  type macro_fn = Token_stream.t -> Result.t

  (** One named exported macro within a provider. *)
  type exported_macro

  (** Runtime provider value returned from [let provider () = ...]. *)
  type t

  (** Name one exported macro function. *)
  val fn: string -> macro_fn -> exported_macro

  (** Construct a provider for a qualified module path such as [["Sqlx"]]. *)
  val v: module_path:string list -> exported_macro list -> t

  (** Qualified module path used to resolve [Pkg.macro!] call sites. *)
  val module_path: t -> string list

  (** Exported macro names in this provider. *)
  val macro_names: t -> string list
end

module Provider_contract: sig
  (** Validate the source-side provider contract for one manifest-declared
      provider.

      This is intentionally a shallow parser-backed check: it proves the
      declared provider source parses and exposes [let provider () = ...].
      Runtime export drift is validated separately through [Runner]. *)
  val validate: Riot_model.Macro_provider.t -> (unit, Error.t) result

  (** Validate a whole provider set before runner materialization. *)
  val validate_all: Riot_model.Macro_provider.t list -> (unit, Error.t) result
end

module Runner: sig
  (** Materialized generated workspace used to build and execute procedural
      macro providers. *)
  type plan

  (** Stable hash for the generated runner inputs, including provider sources,
      helper modules, toolchain inputs, and copied dependency closure. *)
  val providers_hash: workspace_root:Path.t -> Riot_model.Macro_provider.t list -> string

  (** Source-side provider contract validation. *)
  val validate_providers: Riot_model.Macro_provider.t list -> (unit, Error.t) result

  (** Compute the generated runner layout without writing it. *)
  val plan: workspace_root:Path.t -> target_dir_root:Path.t -> Riot_model.Macro_provider.t list -> plan

  (** Root of the generated runner workspace. *)
  val workspace_root: plan -> Path.t

  (** Built runner binary path inside the generated workspace target dir. *)
  val binary_path: plan -> Path.t

  (** Materialize the generated workspace, copying the provider dependency
      closure needed to build it in isolation. *)
  val materialize:
    workspace_root:Path.t -> target_dir_root:Path.t -> Riot_model.Macro_provider.t list -> plan

  (** Build the generated runner and prove that its actual exported provider
      metadata matches the manifest declarations passed in from [riot-model]. *)
  val validate_provider_exports:
    workspace_root:Path.t -> target_dir_root:Path.t -> Riot_model.Macro_provider.t list -> (unit, string) result

  (** Expand one concrete source file through the generated runner. *)
  val run_file:
    workspace_root:Path.t ->
    target_dir_root:Path.t ->
    Riot_model.Macro_provider.t list ->
    input_path:Path.t ->
    output_path:Path.t ->
    (unit, string) result
end

module Format: sig
  (** First prototype implementation of [Macro.format!].

      The current lowering is intentionally simple: it parses the format literal
      into a small IR and rewrites into a [Std.IO.Buffer]-based builder
      expression. A later typed macro pass can reuse that IR to choose
      type-directed formatting behavior. *)
  val expand: Token_stream.t -> Result.t

  (** Convenience provider used by focused macro tests and local fixtures. *)
  val provider: unit -> Provider.t
end

module Format_parser: sig
  (** One parsed placeholder in a [format!] literal. *)
  type hole =
    | Next_arg_to_string
    | Var_to_string of string

  (** One parsed format-literal segment. *)
  type item =
    | String of string
    | Hole of hole

  (** Parsed format literal IR. *)
  type t = item list

  (** Parse the literal text of the first [format!] argument.

      The current grammar is intentionally narrow and only supports:
      - literal text
      - [{}] positional placeholders
      - [{name}] named captures
      - escaped braces via [{{] and [}}] *)
  val parse_literal: literal_text:string -> span:Syn.Ceibo.Span.t -> (t, Error.t) result
end

module Validator: sig
  (** Reparse rewritten source and fail before planner/executor hand it to the
      normal compiler pipeline. *)
  val validate_source: filename:Path.t -> string -> (unit, Error.t) result
end

module Expander: sig
  (** Result of one source rewrite pass. *)
  type expansion = {
    source: string;
    changed: bool;
  }

  (** Expand every parsed macro invocation in one already-parsed environment. *)
  val expand_environment: ?providers:Provider.t list -> Environment.t -> (expansion, Error.t) result

  (** Parse, expand, and revalidate one source file. *)
  val expand_source:
    ?providers:Provider.t list -> filename:Path.t -> string -> (expansion, Error.t) result
end

type error = Error.t = {
  message: string;
  span: Syn.Ceibo.Span.t option;
}
type expansion = Expander.expansion = {
  source: string;
  changed: bool;
}

(** Extract the human-readable diagnostic message from a macro error. *)
val error_message: error -> string

(** Convenience wrapper around [Expander.expand_source]. *)
val expand_source: ?providers:Provider.t list -> filename:Path.t -> string -> (expansion, error) result
