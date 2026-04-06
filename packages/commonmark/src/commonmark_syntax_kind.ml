open Std

type t =
  | Document
  | Heading
  | Paragraph
  | Block_quote
  | Code_block
  | List
  | List_item
  | Horizontal_rule
  | Raw_html
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
  | Horizontal_rule -> "horizontal_rule"
  | Raw_html -> "raw_html"
  | Text -> "text"
  | Error -> "error"
