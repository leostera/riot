open Std
open Std.Collections
open Markdown_syntax_kind
open Markdown_diagnostic
module Syntax_kind = Markdown_syntax_kind

type inline_node =
  | Text of string
  | Emphasis of inline_node list
  | Strong of inline_node list
  | Strikethrough of inline_node list
  | Code_span of string
  | Hard_break
  | Raw_html of string
  | Link of { label: inline_node list; destination: string; title: string option }
  | Image of { alt: inline_node list; destination: string; title: string option }

type table_alignment =
  | Default
  | Left
  | Center
  | Right

type table_row = {
  cells: inline_node list list;
  alignments: table_alignment list;
}

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

type parsed = {
  source: string;
  tokens: Markdown_token.t list;
  tree: (Markdown_syntax_kind.t, string) Ceibo.Green.node;
  diagnostics: Markdown_diagnostic.t list;
}

type flavor =
  | Markdown
  | Gfm

type line = {
  text: string;
  start: int;
}

type list_marker = {
  indent: int;
  indent_offset: int;
  marker_len: int;
  marker_after: int;
  marker_after_columns: int;
  ordered: bool;
}

let token = fun kind text -> Ceibo.Builder.make_token ~kind ~text ~width:(String.length text)
let node = fun kind children -> Ceibo.Builder.make_node ~kind children

let is_space = fun char -> char = ' ' || char = '\t'
let char_not = fun left right -> not (Char.equal left right)
let is_gfm = fun flavor -> flavor = Gfm
let is_some = Option.is_some

let string_all_equal = fun char text ->
  let rec loop index =
    if index >= String.length text then
      true
    else if Char.equal text.[index] char then
      loop (index + 1)
    else
      false
  in
  loop 0

let line_end = fun line -> line.start + String.length line.text
let make_span = fun ~start ~len -> Ceibo.Span.make ~start ~end_:(start + len)

let line_is_blank = fun line ->
  let len = String.length line in
  let rec loop index =
    if index >= len then
      true
    else if is_space line.[index] then
      loop (index + 1)
    else
      false
  in
  loop 0

let next_tab_stop = fun column -> column + (4 - (column mod 4))

let count_indent = fun text max_indent ->
  let len = String.length text in
  let rec loop index column =
    if index >= len then
      index
    else
      match text.[index] with
      | ' ' ->
          if column + 1 > max_indent then
            index
          else
            loop (index + 1) (column + 1)
      | '\t' ->
          let next = next_tab_stop column in
          if next > max_indent then
            index
          else
            loop (index + 1) next
      | _ ->
          index
  in
  loop 0 0

let columns_of_prefix = fun text offset ->
  let limit = Int.min offset (String.length text) in
  let rec loop index column =
    if index >= limit then
      column
    else
      match text.[index] with
      | ' ' -> loop (index + 1) (column + 1)
      | '\t' -> loop (index + 1) (next_tab_stop column)
      | _ -> column
  in
  loop 0 0

let strip_columns_from = fun text offset start_column drop_columns ->
  let len = String.length text in
  let remainder = fun value_offset ->
    if value_offset >= len then
      ""
    else
      String.sub text value_offset (len - value_offset)
  in
  let target = start_column + drop_columns in
  let rec preserve index column =
    if index >= len then
      Some (index, String.make (column - target) ' ')
    else
      match text.[index] with
      | ' ' -> preserve (index + 1) (column + 1)
      | '\t' -> preserve (index + 1) (next_tab_stop column)
      | _ -> Some (index, String.make (column - target) ' ' ^ remainder index)
  in
  let rec consume index column =
    if column >= target then
      preserve index column
    else if index >= len then
      None
    else
      match text.[index] with
      | ' ' -> consume (index + 1) (column + 1)
      | '\t' -> consume (index + 1) (next_tab_stop column)
      | _ -> None
  in
  consume offset start_column

let drop_indent_columns = fun text target -> strip_columns_from text 0 0 target

let consume_one_following_space = fun text offset column ->
  match strip_columns_from text offset column 1 with
  | Some result -> result
  | None ->
      (offset, "")

let trim_left = fun text ->
  let len = String.length text in
  let rec loop index =
    if index >= len then
      index
    else if is_space text.[index] then
      loop (index + 1)
    else
      index
  in
  let left = loop 0 in
  String.sub text left (len - left)

let trim_right = fun text ->
  let len = String.length text in
  let rec loop index =
    if index <= 0 then
      index
    else if is_space text.[index - 1] then
      loop (index - 1)
    else
      index
  in
  let right = loop len in
  if right <= 0 then
    ""
  else
    String.sub text 0 right

let trim = fun text -> trim_right (trim_left text)

let find_substring = fun text start pattern ->
  let pattern_len = String.length pattern in
  let len = String.length text in
  let start = if start < 0 then 0 else start in
  if pattern_len = 0 then
    Some start
  else
    let rec loop index =
      if index + pattern_len > len then
        None
      else if String.sub text index pattern_len = pattern then
        Some index
      else
        loop (index + 1)
    in
    loop start

let has_char = fun subject char ->
  let rec loop index =
    if index >= String.length subject then
      false
    else if subject.[index] = char then
      true
    else
      loop (index + 1)
  in
  loop 0

let remove_prefix = fun text count ->
  if count >= String.length text then
    ""
  else
    String.sub text count (String.length text - count)

let normalize_paragraph_line = fun text ->
  let indent = count_indent text 3 in
  remove_prefix text indent

let take = fun count items ->
  let rec loop n acc remaining =
    if n <= 0 then
      List.rev acc
    else
      match remaining with
      | [] -> List.rev acc
      | head :: tail -> loop (n - 1) (head :: acc) tail
  in
  loop count [] items

let repeat = fun value count ->
  let rec loop n acc =
    if n <= 0 then
      List.rev acc
    else
      loop (n - 1) (value :: acc)
  in
  loop count []

let string_of_char = fun char len ->
  if len <= 0 then
    ""
  else
    String.make len char

let make_control_diagnostics = fun source ->
  let len = String.length source in
  let rec loop index diags =
    if index >= len then
      diags
    else
      let char = source.[index] in
      if Char.code char < 32 && char_not char '\t' && char_not char '\n' && char_not char '\r' then
        let found = { kind = "control"; text = String.make 1 char } in
        let diag =
          unexpected_control_character
            ~found
            ~code:(Char.code char)
            ~span:(make_span ~start:index ~len:1)
        in
        loop (index + 1) (diag :: diags)
      else
        loop (index + 1) diags
  in
  loop 0 []

let parse_heading = fun text ->
  let indent = count_indent text 3 in
  let len = String.length text in
  let rec loop index level =
    if index >= len || level >= 6 || char_not text.[index] '#' then
      (index, level)
    else
      loop (index + 1) (level + 1)
  in
  let after_hash, level = loop indent 0 in
  if level = 0 then
    None
  else if after_hash >= len then
    Some (level, "")
  else
    let sep = text.[after_hash] in
    if char_not sep ' ' && char_not sep '\t' then
      None
    else
      let title = String.sub text (after_hash + 1) (len - after_hash - 1) |> trim_right in
      let trimmed_len =
        let rec trim_hashes index =
          if index <= 0 then
            index
          else if title.[index - 1] = '#' then
            trim_hashes (index - 1)
          else
            index
        in
        trim_hashes (String.length title)
      in
      let content =
        if trimmed_len <= 0 then
          ""
        else
          String.sub title 0 trimmed_len |> trim
      in
      Some (level, content)

let parse_thematic_break = fun text ->
  let line = trim text in
  let len = String.length line in
  if len < 3 then
    None
  else
    let marker = line.[0] in
    if char_not marker '*' && char_not marker '-' && char_not marker '_' then
      None
    else
      let rec loop index =
        if index >= len then
          true
        else if line.[index] = marker || is_space line.[index] then
          loop (index + 1)
        else
          false
      in
      if loop 0 then Some marker else None

let parse_setext_underline = fun text ->
  let line = trim text in
  let len = String.length line in
  if len = 0 then
    None
  else
    let marker = line.[0] in
    if char_not marker '=' && char_not marker '-' then
      None
    else if string_all_equal marker line then
      if marker = '=' then Some 1 else Some 2
    else
      None

let heading_kind = function
  | 1 -> Syntax_kind.Heading_1
  | 2 -> Syntax_kind.Heading_2
  | 3 -> Syntax_kind.Heading_3
  | 4 -> Syntax_kind.Heading_4
  | 5 -> Syntax_kind.Heading_5
  | _ -> Syntax_kind.Heading_6

let parse_fence_open = fun text ->
  let indent = count_indent text 3 in
  let indent_columns = columns_of_prefix text indent in
  let len = String.length text in
  if indent >= len then
    None
  else
    let marker = text.[indent] in
    if char_not marker '`' && char_not marker '~' then
      None
    else
      let rec count index =
        if index >= len then
          index
        else if text.[index] = marker then
          count (index + 1)
        else
          index
      in
      let after = count indent in
      let marker_len = after - indent in
      if marker_len < 3 then
        None
      else
        let info =
          trim (remove_prefix text after)
          |> fun value ->
          let rec scan index =
            if index >= String.length value then
              index
            else if value.[index] = ' ' || value.[index] = '\t' then
              index
            else
              scan (index + 1)
          in
          let word_len = scan 0 in
          if word_len <= 0 then "" else String.sub value 0 word_len
        in
        if marker = '`' && has_char (trim (remove_prefix text after)) '`' then
          None
        else
          Some (marker, marker_len, indent_columns, info)

let parse_fence_close = fun marker_len marker line ->
  let indent = count_indent line.text 3 in
  let remaining = remove_prefix line.text indent in
  if String.length remaining < marker_len then
    false
  else
    let rec count index =
      if index >= String.length remaining then
        index
      else if remaining.[index] = marker then
        count (index + 1)
      else
        index
    in
    let actual_len = count 0 in
    actual_len >= marker_len && trim_left (remove_prefix remaining actual_len) = ""

let parse_fenced_code_block = fun lines start ->
  if start >= Array.length lines then
    None
  else
    let first = lines.(start) in
    match parse_fence_open first.text with
    | None -> None
    | Some (marker, marker_len, opener_indent, info) ->
        let marker_text = string_of_char marker marker_len in
        let strip_content_indent = fun text ->
          let offset = count_indent text opener_indent in
          remove_prefix text offset
        in
        let rec collect_code_lines index acc =
          if index >= Array.length lines then
            (None, List.rev acc)
          else if parse_fence_close marker_len marker lines.(index) then
            (Some index, List.rev acc)
          else
            collect_code_lines (index + 1) (strip_content_indent lines.(index).text :: acc)
        in
        (
          match collect_code_lines (start + 1) [] with
          | None, code_lines ->
              let content =
                if code_lines = [] then
                  ""
                else
                  String.concat "\n" code_lines ^ "\n"
              in
              let span = make_span ~start:first.start ~len:(line_end first - first.start) in
              let found = { kind = "fence"; text = marker_text } in
              let diag = unclosed_fenced_code_block ~found ~opener:marker_text ~span in
              let children =
                (if info = "" then [] else [ token Syntax_kind.Info_string info ])
                @ [ token Syntax_kind.Text content ]
              in
              Some (node Syntax_kind.Fenced_code_block children, Array.length lines, [ diag ])
          | Some close_index, code_lines ->
              let content =
                if code_lines = [] then
                  ""
                else
                  String.concat "\n" code_lines ^ "\n"
              in
              let children =
                (if info = "" then [] else [ token Syntax_kind.Info_string info ])
                @ [ token Syntax_kind.Text content ]
              in
              Some (node Syntax_kind.Fenced_code_block children, close_index + 1, [])
        )

let parse_indented_code_block = fun lines start ->
  if start >= Array.length lines then
    None
  else
    let first = lines.(start) in
    let first_indented = Option.is_some (drop_indent_columns first.text 4) in
    if not first_indented then
      None
    else
      let strip_line = fun text ->
        match drop_indent_columns text 4 with
        | Some (_, stripped) -> stripped
        | None -> ""
      in
      let rec collect index acc =
        if index >= Array.length lines then
          (index, List.rev acc)
        else
          let line = lines.(index).text in
          if Option.is_some (drop_indent_columns line 4) then
            collect (index + 1) (strip_line line :: acc)
          else if line_is_blank line then
            collect (index + 1) ("" :: acc)
          else
            (index, List.rev acc)
      in
      let next, code_lines = collect start [] in
      let content =
        if code_lines = [] then
          ""
        else
          String.concat "\n" code_lines ^ "\n"
      in
      Some
        (node Syntax_kind.Indented_code_block [ token Syntax_kind.Text content ], next, [])

let parse_block_quote_prefix = fun text ->
  let indent_offset = count_indent text 3 in
  let indent = columns_of_prefix text indent_offset in
  if indent_offset >= String.length text || char_not text.[indent_offset] '>' then
    None
  else
    let after = indent_offset + 1 in
    let after_column = indent + 1 in
    if after < String.length text && is_space text.[after] then
      let content_start, content = consume_one_following_space text after after_column in
      Some (content_start, content)
    else
      Some (after, remove_prefix text after)

let parse_list_marker = fun text ->
  let len = String.length text in
  if len = 0 then
    None
  else
    let indent_offset = count_indent text 3 in
    let indent = columns_of_prefix text indent_offset in
    if indent_offset >= len then
      None
    else
      let marker = text.[indent_offset] in
      if marker = '-' || marker = '*' || marker = '+' then
        if indent_offset + 1 < len && is_space text.[indent_offset + 1] then
          let marker_after = indent_offset + 2 in
          Some {
            indent;
            indent_offset;
            marker_len = 1;
            marker_after;
            marker_after_columns = indent + (marker_after - indent_offset);
            ordered = false;
          }
        else
          None
      else
        let rec scan index =
          if index >= len then
            None
          else if text.[index] < '0' || text.[index] > '9' then
            None
          else if index + 1 < len && (text.[index + 1] = '.' || text.[index + 1] = ')') then
            if index + 2 < len && is_space text.[index + 2] then
              let marker_after = index + 3 in
              Some {
                indent;
                indent_offset;
                marker_len = index - indent_offset + 1;
                marker_after;
                marker_after_columns = indent + (marker_after - indent_offset);
                ordered = true;
              }
            else
              None
          else
            scan (index + 1)
        in
        scan (indent_offset + 1)

let parse_task_list_marker = fun flavor text ->
  if not (is_gfm flavor) then
    None
  else if String.length text >= 4 && text.[0] = '[' && text.[1] = ' ' && text.[2] = ']' && is_space text.[3] then
    Some (false, String.sub text 4 (String.length text - 4))
  else if String.length text >= 4 && text.[0] = '[' && (text.[1] = 'x' || text.[1] = 'X') && text.[2] = ']' && is_space text.[3] then
    Some (true, String.sub text 4 (String.length text - 4))
  else
    None

let split_table_cells = fun text ->
  let len = String.length text in
  let rec loop index start acc =
    if index >= len then
      List.rev (trim (String.sub text start (index - start)) :: acc)
    else if text.[index] = '|' then
      let cell = String.sub text start (index - start) in
      loop (index + 1) (index + 1) (trim cell :: acc)
    else
      loop (index + 1) start acc
  in
  let cells = loop 0 0 [] in
  let cells =
    match cells with
    | "" :: tail -> tail
    | _ -> cells
  in
  match List.rev cells with
  | "" :: tail -> List.rev tail
  | _ -> cells

let parse_table_alignment = fun text ->
  let trimmed = trim text in
  let len = String.length trimmed in
  if len < 3 then
    None
  else
    let rec has_dash index =
      if index >= len then
        false
      else if trimmed.[index] = '-' then
        true
      else
        has_dash (index + 1)
    in
    let rec valid index =
      if index >= len then
        true
      else
        match trimmed.[index] with
        | '-' | ':' | ' ' | '\t' -> valid (index + 1)
        | _ -> false
    in
    if not (has_dash 0) || not (valid 0) then
      None
    else
      let left = trimmed.[0] = ':' in
      let right = trimmed.[len - 1] = ':' in
      if left && right then
        Some Center
      else if left then
        Some Left
      else if right then
        Some Right
      else
        Some Default

let cell_kind_of_alignment = function
  | Default -> Syntax_kind.Table_cell_default
  | Left -> Syntax_kind.Table_cell_left
  | Center -> Syntax_kind.Table_cell_center
  | Right -> Syntax_kind.Table_cell_right

let html_block_tags = [
  "address"; "article"; "aside"; "base"; "basefont"; "blockquote"; "body"; "caption";
  "center"; "col"; "colgroup"; "dd"; "details"; "dialog"; "dir"; "div"; "dl"; "dt";
  "fieldset"; "figcaption"; "figure"; "footer"; "form"; "frame"; "frameset"; "h1";
  "h2"; "h3"; "h4"; "h5"; "h6"; "head"; "header"; "hr"; "html"; "iframe"; "legend";
  "li"; "link"; "main"; "menu"; "menuitem"; "nav"; "noframes"; "ol"; "optgroup";
  "option"; "p"; "param"; "search"; "section"; "summary"; "table"; "tbody"; "td";
  "tfoot"; "th"; "thead"; "title"; "tr"; "track"; "ul"; "pre"; "script"; "style";
  "textarea"
]

let starts_html_block = fun line ->
  let trimmed = trim_left line in
  let len = String.length trimmed in
  if len = 0 || not (Char.equal trimmed.[0] '<') then
    false
  else if len >= 4 && String.sub trimmed 0 4 = "<!--" then
    true
  else if len >= 2 && String.sub trimmed 0 2 = "<?" then
    true
  else if len >= 9 && String.sub trimmed 0 9 = "<![CDATA[" then
    true
  else if len >= 2 && String.sub trimmed 0 2 = "<!" then
    true
  else
    let start =
      if len >= 2 && trimmed.[1] = '/' then
        2
      else
        1
    in
    if start >= len then
      false
    else
      let rec scan index =
        if index >= len then
          index
        else
          let char = trimmed.[index] in
          if
            (char >= 'a' && char <= 'z')
            || (char >= 'A' && char <= 'Z')
            || (char >= '0' && char <= '9')
            || char = '-'
          then
            scan (index + 1)
          else
            index
      in
      let finish = scan start in
      if finish <= start then
        false
      else
        let tag = String.sub trimmed start (finish - start) |> String.lowercase_ascii in
        List.mem tag html_block_tags

let rec parse_block_quote = fun flavor lines start ->
  if start >= Array.length lines then
    None
  else
    let first = lines.(start) in
    match parse_block_quote_prefix first.text with
    | None -> None
    | Some (content_start, content) ->
        let rec collect index acc =
          if index >= Array.length lines then
            (List.rev acc, index)
          else
            let text = lines.(index).text in
            if line_is_blank text then
              let rec next_nonblank next =
                if next >= Array.length lines then
                  next
                else if line_is_blank lines.(next).text then
                  next_nonblank (next + 1)
                else
                  next
              in
              let next = next_nonblank (index + 1) in
              if next < Array.length lines && is_some (parse_block_quote_prefix lines.(next).text) then
                collect (index + 1) ({ text = ""; start = lines.(index).start } :: acc)
              else
                (List.rev acc, index)
            else
              match parse_block_quote_prefix text with
              | None -> (List.rev acc, index)
              | Some (nested_start, nested) ->
                  let nested_offset = lines.(index).start + nested_start in
                  collect (index + 1) ({ text = nested; start = nested_offset } :: acc)
        in
        let quote_lines, next =
          collect
            (start + 1)
            [ { text = content; start = first.start + content_start } ]
        in
        let nested_lines = Array.of_list quote_lines in
        let blocks, diagnostics = parse_blocks ~flavor nested_lines 0 in
        Some (node Syntax_kind.Block_quote blocks, next, diagnostics)

and parse_table_row = fun flavor text ->
  if not (is_gfm flavor) || not (has_char text '|') then
    None
  else
    match split_table_cells text with
    | [] -> None
    | cells -> Some cells

and parse_table = fun flavor lines start ->
  if not (is_gfm flavor) || start + 1 >= Array.length lines then
    None
  else
    match parse_table_row flavor lines.(start).text with
    | None -> None
    | Some header_cells ->
        let delimiter_cells = split_table_cells lines.(start + 1).text in
        if delimiter_cells = [] then
          None
        else
          let alignments = List.map parse_table_alignment delimiter_cells in
          if List.exists (fun value -> value = None) alignments then
            None
          else
            let alignments = List.map (fun value -> Option.unwrap_or ~default:Default value) alignments in
            let width = List.length alignments in
            let normalize_row = fun row ->
              let cells = take width row in
              let missing = width - List.length cells in
              let cells = cells @ repeat "" missing in
              List.map2
                (fun alignment value ->
                  node (cell_kind_of_alignment alignment) [ token Syntax_kind.Text value ])
                alignments
                cells
            in
            let rec collect_rows index acc =
              if index >= Array.length lines then
                (List.rev acc, index)
              else if line_is_blank lines.(index).text then
                (List.rev acc, index)
              else
                match parse_table_row flavor lines.(index).text with
                | None -> (List.rev acc, index)
                | Some row ->
                    collect_rows
                      (index + 1)
                      (node Syntax_kind.Table_row (normalize_row row) :: acc)
            in
            let rows, next = collect_rows (start + 2) [] in
            Some
              ( node Syntax_kind.Table (node Syntax_kind.Table_header (normalize_row header_cells) :: rows),
                next,
                []
              )

and parse_list = fun flavor lines start ->
  if start >= Array.length lines then
    None
  else
    match parse_list_marker lines.(start).text with
    | None -> None
    | Some first ->
        let continuation_min = first.marker_after_columns in
        let rec collect_item_body index acc had_blank =
          if index >= Array.length lines then
            (index, List.rev acc, had_blank)
          else
            let text = lines.(index).text in
            if line_is_blank text then
              collect_item_body
                (index + 1)
                ({ text = ""; start = lines.(index).start } :: acc)
                true
            else
              match parse_list_marker text with
              | Some next_marker when next_marker.indent = first.indent && next_marker.ordered = first.ordered ->
                  (index, List.rev acc, had_blank)
              | _ ->
                  (
                    match drop_indent_columns text continuation_min with
                    | None -> (index, List.rev acc, had_blank)
                    | Some (content_offset, content) ->
                        collect_item_body
                          (index + 1)
                          ({
                            text = content;
                            start = lines.(index).start + content_offset;
                          }
                          :: acc)
                          had_blank
                  )
        in
        let rec collect_items index acc diagnostics loose =
          if index >= Array.length lines then
            (List.rev acc, index, List.rev diagnostics, loose)
          else
            match parse_list_marker lines.(index).text with
            | Some marker when marker.indent = first.indent && marker.ordered = first.ordered ->
                (
                  let marker_end = marker.indent_offset + marker.marker_len in
                  let marker_column = marker.indent + marker.marker_len in
                  let head_offset, head_text =
                    consume_one_following_space lines.(index).text marker_end marker_column
                  in
                  let head_start = lines.(index).start + head_offset in
                  let task, body_text =
                    match parse_task_list_marker flavor head_text with
                    | Some (checked, content) -> Some checked, content
                    | None -> None, head_text
                  in
                  let head_start = if Option.is_some task then head_start + 4 else head_start in
                  let next_index, body_lines, body_has_blank =
                    collect_item_body (index + 1) [ { text = body_text; start = head_start } ] false
                  in
                  let body_lines = Array.of_list body_lines in
                  let body_blocks, body_diagnostics = parse_blocks ~flavor body_lines 0 in
                  let item_kind =
                    match task with
                    | Some true -> Syntax_kind.Task_list_item_checked
                    | Some false -> Syntax_kind.Task_list_item_unchecked
                    | None -> Syntax_kind.List_item
                  in
                  let item = node item_kind body_blocks in
                  collect_items
                    next_index
                    (item :: acc)
                    (List.rev_append body_diagnostics diagnostics)
                    (loose || body_has_blank)
                )
            | _ -> (List.rev acc, index, List.rev diagnostics, loose)
        in
        let items, next, diagnostics, loose = collect_items start [] [] false in
        if items = [] then
          None
        else
          let kind =
            match (first.ordered, loose) with
            | true, true -> Syntax_kind.Ordered_list_loose
            | true, false -> Syntax_kind.Ordered_list_tight
            | false, true -> Syntax_kind.Unordered_list_loose
            | false, false -> Syntax_kind.Unordered_list_tight
          in
          Some (node kind items, next, diagnostics)

and parse_raw_html_line = fun lines start ->
  if start >= Array.length lines then
    None
  else
    let line = lines.(start).text in
    if String.length line = 0 || not (starts_html_block line) then
      None
    else
      match find_substring line 1 ">" with
      | None -> None
      | Some close ->
          let html = String.sub line 0 (close + 1) in
          Some
            ( node Syntax_kind.Raw_html_block [ token Syntax_kind.Raw_html html ],
              start + 1,
              []
            )

and parse_paragraph = fun flavor lines start ->
  if start >= Array.length lines then
    None
  else
    let len = Array.length lines in
    let is_block_start = fun index text ->
      is_some (parse_heading text)
      || is_some (parse_fence_open text)
      || is_some (parse_list_marker text)
      || is_some (parse_block_quote_prefix text)
      || is_some (parse_thematic_break text)
      || is_some (parse_raw_html_line lines index)
      || is_some (parse_table flavor lines index)
    in
    let rec collect index acc =
      if index >= len then
        (index, None, List.rev acc)
      else
        let line = lines.(index).text in
        match parse_setext_underline line with
        | Some level ->
            (index + 1, Some (level, lines.(index)), List.rev acc)
        | None ->
            if line_is_blank line then
              (index, None, List.rev acc)
            else if index > start && is_block_start index line then
              (index, None, List.rev acc)
            else
              collect (index + 1) (normalize_paragraph_line line :: acc)
    in
    let next, setext, texts =
      collect (start + 1) [ normalize_paragraph_line lines.(start).text ]
    in
    let text = String.concat "\n" texts in
    let setext_text =
      texts
      |> List.map (fun line ->
        let indent = count_indent line 3 in
        remove_prefix line indent |> trim_right)
      |> String.concat "\n"
    in
    match parse_heading lines.(start).text with
    | Some (level, content) ->
        Some (node (heading_kind level) [ token Syntax_kind.Text content ], next, [])
    | None ->
        (
          match setext with
          | Some (level, _) ->
              Some (node (heading_kind level) [ token Syntax_kind.Text setext_text ], next, [])
          | None ->
              Some (node Syntax_kind.Paragraph [ token Syntax_kind.Text text ], next, [])
        )

and parse_blocks = fun ~flavor lines start ->
  if start >= Array.length lines then
    ([], [])
  else if line_is_blank lines.(start).text then
    parse_blocks ~flavor lines (start + 1)
  else
    let block =
      match parse_fenced_code_block lines start with
      | Some result -> Some result
      | None ->
          (
            match parse_indented_code_block lines start with
            | Some result -> Some result
            | None ->
                (
                  match parse_block_quote flavor lines start with
                  | Some result -> Some result
                  | None ->
                      (
                        match parse_table flavor lines start with
                        | Some result -> Some result
                        | None ->
                            (
                              if is_some (parse_thematic_break lines.(start).text) then
                                Some (node Syntax_kind.Horizontal_rule [], start + 1, [])
                              else
                                (
                                  match parse_list flavor lines start with
                                  | Some result -> Some result
                                  | None ->
                                      (
                                        match parse_raw_html_line lines start with
                                        | Some result -> Some result
                                        | None -> parse_paragraph flavor lines start
                                      )
                                )
                            )
                      )
                )
          )
    in
    match block with
    | Some (node_, next, diagnostics) ->
        let parsed_next, nested = parse_blocks ~flavor lines next in
        (node_ :: parsed_next, diagnostics @ nested)
    | None ->
        parse_blocks ~flavor lines (start + 1)

let lines_of_tokens = fun tokens ->
  tokens
  |> List.filter_map (fun token ->
    match token.Markdown_token.kind with
    | Markdown_token.Line_text -> Some { text = token.text; start = token.span.start }
    | _ -> None)
  |> Array.of_list

let parse = fun ?(flavor = Markdown) source ->
  let source = Markdown_lexer.normalize_newlines source in
  let tokens = Markdown_lexer.tokenize source in
  let lines = lines_of_tokens tokens in
  let control_diagnostics = make_control_diagnostics source in
  try
    let blocks, diagnostics = parse_blocks ~flavor lines 0 in
    let diagnostics = List.rev_append control_diagnostics diagnostics in
    let tree = Ceibo.Green.make_node ~kind:Syntax_kind.Document ~children:blocks in
    { source; tokens; tree; diagnostics }
  with
  | exn ->
      let message = Exception.to_string exn in
      let found = { kind = "exception"; text = message } in
      let span = make_span ~start:0 ~len:(String.length source) in
      let diagnostic = parser_internal ~found ~message ~span in
      {
        source;
        tokens;
        tree =
          Ceibo.Green.make_node
            ~kind:Syntax_kind.Document
            ~children:[ node Syntax_kind.Error [ token Syntax_kind.Text message ] ];
        diagnostics = List.rev_append control_diagnostics [ diagnostic ];
      }
