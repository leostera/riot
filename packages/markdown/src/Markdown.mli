open Std

(** Structured diagnostics used by the CommonMark parser. *)
module Error: sig
  (** Stable diagnostic identifier token. *)
  type id = string
  val to_string: id -> string

  val of_string: string -> id

  val to_json: id -> Data.Json.t

  val from_json: Data.Json.t -> (id, string) result
end

(** Core syntax kinds used by the CEIBO tree. *)
module Syntax_kind: sig
  type t =
    | Document
    | Heading
    | Paragraph
    | Block_quote
    | Code_block
    | List
    | List_item
    | Task_list_item
    | Table
    | Table_row
    | Table_cell
    | Horizontal_rule
    | Raw_html
    | Strikethrough
    | Text
    | Error
  val to_string: t -> string
end

(** Parse diagnostics produced from the markdown compiler pipeline. *)
module Diagnostic: sig
  type found_token = {
    kind: string;
    text: string;
  }
  type kind =
    | Invalid_markdown of { found: found_token }
    | Unsupported_feature of { found: found_token; feature: string }
    | Unclosed_fenced_code_block of { found: found_token; opener: string }
    | Unexpected_control_character of { found: found_token; code: int }
    | Parser_internal of { message: string; found: found_token }
  type t = {
    kind: kind;
    span: Ceibo.Span.t;
  }
  val make: kind:kind -> span:Ceibo.Span.t -> t

  val invalid_markdown: found:found_token -> span:Ceibo.Span.t -> t

  val unsupported_feature: found:found_token -> feature:string -> span:Ceibo.Span.t -> t

  val unclosed_fenced_code_block: found:found_token -> opener:string -> span:Ceibo.Span.t -> t

  val unexpected_control_character: found:found_token -> code:int -> span:Ceibo.Span.t -> t

  val parser_internal: found:found_token -> message:string -> span:Ceibo.Span.t -> t

  val found_token: t -> found_token

  val error_id: t -> Error.id

  val id: t -> string

  val expected_message: t -> string

  val fix_message: t -> string option

  val hint_message: t -> string

  val main_message: t -> string

  val to_json: t -> Data.Json.t

  val from_json: Data.Json.t -> (t, string) result
end

(** Human-readable diagnostic rendering helper. *)
module Diagnostic_reporter: sig
  val print: file:string -> source:string -> Diagnostic.t list -> unit

  val format: file:string -> source:string -> Diagnostic.t list -> string
end

(** A CommonMark fixture row (loaded from `tests/spec_fixtures.json` when
    available). *)
type fixture = {
  markdown: string;
  html: string;
  example: int option;
  section: string option;
}
type parse_result = {
  root: (Syntax_kind.t, string) Ceibo.Green.node;
  source: string;
  diagnostics: Diagnostic.t list;
  blocks: Markdown_parser.block_node list;
}

(** Parse markdown into a CEIBO red-green tree plus structured diagnostics.
    Parsing is lossless and never raises. *)
val parse: string -> parse_result

val parse_gfm: string -> parse_result

(** Compile a parsed result into HTML. *)
val to_html: parse_result -> string

(** Parse and compile in one step. *)
val compile: string -> string

val compile_gfm: string -> string

(** Access the spec fixture corpus loaded from disk.
    Returns an empty list when fixtures cannot be loaded (for embedded or
    distribution-only builds). *)
val all_spec_fixtures: unit -> fixture list
