open Std

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

let to_string = function
  | Document -> "document"
  | Heading -> "heading"
  | Paragraph -> "paragraph"
  | Block_quote -> "block_quote"
  | Code_block -> "code_block"
  | List -> "list"
  | List_item -> "list_item"
  | Task_list_item -> "task_list_item"
  | Table -> "table"
  | Table_row -> "table_row"
  | Table_cell -> "table_cell"
  | Horizontal_rule -> "horizontal_rule"
  | Raw_html -> "raw_html"
  | Strikethrough -> "strikethrough"
  | Text -> "text"
  | Error -> "error"
