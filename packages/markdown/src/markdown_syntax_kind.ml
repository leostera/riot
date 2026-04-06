open Std

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

let to_string = function
  | Document -> "document"
  | Heading_1 -> "heading_1"
  | Heading_2 -> "heading_2"
  | Heading_3 -> "heading_3"
  | Heading_4 -> "heading_4"
  | Heading_5 -> "heading_5"
  | Heading_6 -> "heading_6"
  | Paragraph -> "paragraph"
  | Block_quote -> "block_quote"
  | Ordered_list_tight -> "ordered_list_tight"
  | Ordered_list_loose -> "ordered_list_loose"
  | Unordered_list_tight -> "unordered_list_tight"
  | Unordered_list_loose -> "unordered_list_loose"
  | List_item -> "list_item"
  | Task_list_item_checked -> "task_list_item_checked"
  | Task_list_item_unchecked -> "task_list_item_unchecked"
  | Fenced_code_block -> "fenced_code_block"
  | Indented_code_block -> "indented_code_block"
  | Horizontal_rule -> "horizontal_rule"
  | Raw_html_block -> "raw_html_block"
  | Table -> "table"
  | Table_header -> "table_header"
  | Table_row -> "table_row"
  | Table_cell_default -> "table_cell_default"
  | Table_cell_left -> "table_cell_left"
  | Table_cell_center -> "table_cell_center"
  | Table_cell_right -> "table_cell_right"
  | Info_string -> "info_string"
  | Raw_html -> "raw_html"
  | Text -> "text"
  | Error -> "error"
