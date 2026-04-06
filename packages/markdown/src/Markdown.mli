open Std

module Error: sig
  type id = string
  val to_string: id -> string

  val of_string: string -> id

  val to_json: id -> Data.Json.t

  val from_json: Data.Json.t -> (id, string) result
end

module Syntax_kind: sig
  type t =
    | Document
    | Heading_1
    | Heading_2
    | Heading_3
    | Heading_4
    | Heading_5
    | Heading_6
    | Paragraph
    | Block_quote
    | Ordered_list_tight
    | Ordered_list_loose
    | Unordered_list_tight
    | Unordered_list_loose
    | List_item
    | Task_list_item_checked
    | Task_list_item_unchecked
    | Fenced_code_block
    | Indented_code_block
    | Horizontal_rule
    | Raw_html_block
    | Table
    | Table_header
    | Table_row
    | Table_cell_default
    | Table_cell_left
    | Table_cell_center
    | Table_cell_right
    | Info_string
    | Raw_html
    | Text
    | Error
  val to_string: t -> string
end

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

module Diagnostic_reporter: sig
  val print: file:string -> source:string -> Diagnostic.t list -> unit

  val format: file:string -> source:string -> Diagnostic.t list -> string
end

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
val parse: string -> parse_result

val parse_gfm: string -> parse_result

val to_html: parse_result -> string

val compile: string -> string

val compile_gfm: string -> string

val all_spec_fixtures: unit -> fixture list
