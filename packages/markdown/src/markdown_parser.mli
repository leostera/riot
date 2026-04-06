open Std
open Markdown_syntax_kind
open Markdown_diagnostic

type inline_node =
  | Text of string
  | Emphasis of inline_node list
  | Strong of inline_node list
  | Strikethrough of inline_node list
  | Code_span of string
  | Raw_html of string
  | Link of { label: inline_node list; destination: string }

type table_row = {
  cells: inline_node list list;
  alignments: table_alignment list;
}

and table_alignment =
  | Default
  | Left
  | Center
  | Right

type block_node =
  | Heading of { level: int; inlines: inline_node list; span: Ceibo.Span.t }
  | Paragraph of { inlines: inline_node list; span: Ceibo.Span.t }
  | Block_quote of { blocks: block_node list; span: Ceibo.Span.t }
  | List of { ordered: bool; tight: bool; items: block_node list list; span: Ceibo.Span.t }
  | Task_list_item of {
      checked: bool;
      blocks: block_node list;
      span: Ceibo.Span.t;
    }
  | List_item of { blocks: block_node list; span: Ceibo.Span.t }
  | Code_block of { info: string; code: string; span: Ceibo.Span.t; fenced: bool }
  | Horizontal_rule of Ceibo.Span.t
  | Raw_html of { html: string; span: Ceibo.Span.t }
  | Table of { header: table_row; rows: table_row list; span: Ceibo.Span.t }
  | Error_block of { message: string; span: Ceibo.Span.t }

type flavor = Markdown | Gfm
type parsed = {
  source: string;
  blocks: block_node list;
  diagnostics: Markdown_diagnostic.t list;
}
val parse: ?flavor:flavor -> string -> parsed

val blocks: parsed -> block_node list

val to_green: source:string -> block_node list -> (Markdown_syntax_kind.t, string) Ceibo.Green.node
