open Std
open Commonmark_syntax_kind
open Commonmark_diagnostic

type inline_node =
  | Text of string
  | Emphasis of inline_node list
  | Strong of inline_node list
  | Code_span of string
  | Raw_html of string
  | Link of { label: inline_node list; destination: string }
type block_node =
  | Heading of { level: int; inlines: inline_node list; span: Ceibo.Span.t }
  | Paragraph of { inlines: inline_node list; span: Ceibo.Span.t }
  | Block_quote of { blocks: block_node list; span: Ceibo.Span.t }
  | List of { ordered: bool; items: block_node list list; span: Ceibo.Span.t }
  | List_item of { blocks: block_node list; span: Ceibo.Span.t }
  | Code_block of { info: string; code: string; span: Ceibo.Span.t; fenced: bool }
  | Horizontal_rule of Ceibo.Span.t
  | Raw_html of { html: string; span: Ceibo.Span.t }
  | Error_block of { message: string; span: Ceibo.Span.t }
type parsed = {
  source: string;
  blocks: block_node list;
  diagnostics: Commonmark_diagnostic.t list;
}
val parse: string -> parsed

val blocks: parsed -> block_node list

val to_green: source:string -> block_node list -> (Commonmark_syntax_kind.t, string) Ceibo.Green.node
