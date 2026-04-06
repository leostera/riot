open Std
open Std.Collections
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
  blocks: block_node list;
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

let is_space = fun char -> char = ' ' || char = '\t'

let has_tab_prefix = fun text ->
  if String.length text = 0 then
    false
  else
    text.[0] = '\t'

let next_tab_stop = fun column -> column + (4 - (column mod 4))

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

let normalize_newlines = fun source ->
  if not (has_char source '\r') then
    source
  else
    let buffer = IO.Buffer.create (String.length source) in
    String.iter
      (fun char ->
        if not (Char.equal char '\r') then
          IO.Buffer.add_char buffer char)
      source;
    IO.Buffer.contents buffer

let split_lines = fun source ->
  let length = String.length source in
  let rec loop index line_start acc =
    if index >= length then
      if length > 0 && source.[length - 1] = '\n' then
        List.rev acc |> Array.of_list
      else
        { text = String.sub source line_start (length - line_start); start = line_start } :: acc
        |> List.rev
        |> Array.of_list
    else if source.[index] = '\n' then
      loop
        (index + 1)
        (index + 1)
        ({ text = String.sub source line_start (index - line_start); start = line_start } :: acc)
    else
      loop (index + 1) line_start acc
  in
  loop 0 0 []

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

let consume_indent_columns = fun text target ->
  let len = String.length text in
  let rec loop index column =
    if column >= target then
      Some index
    else if index >= len then
      None
    else
      match text.[index] with
      | ' ' -> loop (index + 1) (column + 1)
      | '\t' ->
          let next = next_tab_stop column in
          if next > target then
            None
          else
            loop (index + 1) next
      | _ -> None
  in
  loop 0 0

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

let starts_with = fun ~prefix text index ->
  let len = String.length text in
  let prefix_len = String.length prefix in
  if index < 0 || index + prefix_len > len then
    false
  else
    String.sub text index prefix_len = prefix

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

let drop = fun count items ->
  let rec loop n remaining =
    if n <= 0 then
      remaining
    else
      match remaining with
      | [] -> []
      | _ :: tail -> loop (n - 1) tail
  in
  loop count items

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

let parse_fence_open = fun text ->
  let indent = count_indent text 3 in
  if indent >= String.length text then
    None
  else
    let marker = text.[indent] in
    if char_not marker '`' && char_not marker '~' then
      None
    else
      let rec count index =
        if index < String.length text && text.[index] = marker then
          count (index + 1)
        else
          index
      in
      let after = count indent in
      let marker_len = after - indent in
      if marker_len < 3 then
        None
      else
        let info = trim (remove_prefix text after) in
        Some (marker, marker_len, info)

let parse_fence_close = fun marker_len marker line ->
  let indent = count_indent line.text 3 in
  let remaining = remove_prefix line.text indent in
  if String.length remaining < marker_len then
    false
  else
    let marker_text = String.sub remaining 0 marker_len in
    string_all_equal marker marker_text && trim_left (remove_prefix remaining marker_len) = ""

let parse_fenced_code_block = fun lines start ->
  if start >= Array.length lines then
    None
  else
    let first = lines.(start) in
    match parse_fence_open first.text with
    | None -> None
    | Some (marker, marker_len, info) ->
        let marker_text = string_of_char marker marker_len in
        let rec scan index =
          if index >= Array.length lines then
            None
          else if parse_fence_close marker_len marker lines.(index) then
            Some index
          else
            scan (index + 1)
        in
        (
          match scan (start + 1) with
          | None ->
              let code_lines =
                Array.to_list lines |> drop (start + 1) |> List.map (fun line -> line.text)
              in
              let content =
                if code_lines = [] then
                  ""
                else
                  String.concat "\n" code_lines ^ "\n"
              in
              let span = make_span ~start:first.start ~len:(line_end first - first.start) in
              let found = { kind = "fence"; text = marker_text } in
              let diag = unclosed_fenced_code_block ~found ~opener:marker_text ~span in
              Some (
                Code_block { info; code = content; span; fenced = true },
                Array.length lines,
                [ diag ]
              )
          | Some close_index ->
              let code_lines =
                Array.to_list lines
                |> drop (start + 1)
                |> take (close_index - start - 1)
                |> List.map (fun line -> line.text)
              in
              let content =
                if code_lines = [] then
                  ""
                else
                  String.concat "\n" code_lines ^ "\n"
              in
              let span_end = line_end lines.(close_index) in
              let span = make_span ~start:first.start ~len:(span_end - first.start) in
              Some (Code_block { info; code = content; span; fenced = true }, close_index + 1, [])
        )

let parse_indented_code_block = fun lines start ->
  if start >= Array.length lines then
    None
  else
    let first = lines.(start) in
    let first_indented =
      starts_with ~prefix:"    " first.text 0
      || has_tab_prefix first.text
    in
    if not first_indented then
      None
    else
      let strip_line = fun text ->
        if starts_with ~prefix:"    " text 0 then
          remove_prefix text 4
        else if has_tab_prefix text then
          remove_prefix text 1
        else
          ""
      in
      let rec collect index acc =
        if index >= Array.length lines then
          (index, List.rev acc)
        else
          let line = lines.(index).text in
          if starts_with ~prefix:"    " line 0 || has_tab_prefix line then
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
      let end_line = lines.(next - 1) in
      let span = make_span ~start:first.start ~len:(line_end end_line - first.start) in
      Some (Code_block { info = ""; code = content; span; fenced = false }, next, [])

let parse_block_quote_prefix = fun text ->
  let indent = count_indent text 3 in
  if indent >= String.length text || char_not text.[indent] '>' then
    None
  else
    let after = indent + 1 in
    let content_start =
      if after < String.length text && is_space text.[after] then
        after + 1
      else
        after
    in
    Some content_start

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

let rec parse_block_quote = fun flavor lines start ->
  if start >= Array.length lines then
    None
  else
    let first = lines.(start) in
    match parse_block_quote_prefix first.text with
    | None -> None
    | Some content_start ->
        let rec collect index acc =
          if index >= Array.length lines then
            (List.rev acc, index)
          else
            let text = lines.(index).text in
            if line_is_blank text then
              collect (index + 1) ({ text = ""; start = lines.(index).start } :: acc)
            else
              match parse_block_quote_prefix text with
              | None -> (List.rev acc, index)
              | Some nested_start ->
                  let nested = remove_prefix text nested_start in
                  let nested_offset = lines.(index).start + nested_start in
                  collect (index + 1) ({ text = nested; start = nested_offset } :: acc)
        in
        let quote_lines, next =
          collect
            (start + 1)
            [ { text = remove_prefix first.text content_start; start = first.start + content_start } ]
        in
        let nested_lines = Array.of_list quote_lines in
        let blocks, diagnostics = parse_blocks ~flavor nested_lines 0 in
        let span_end =
          if next <= start then
            line_end first
          else
            line_end lines.(next - 1)
        in
        let span = make_span ~start:first.start ~len:(span_end - first.start) in
        Some (Block_quote { blocks; span }, next, diagnostics)

and parse_table_row = fun flavor text ->
  if not (is_gfm flavor) || Option.is_none (find_substring text 0 "|") then
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
              {
                cells = List.map (fun value -> parse_inline ~flavor value) cells;
                alignments;
              }
            in
            let rec collect_rows index acc =
              if index >= Array.length lines then
                (List.rev acc, index)
              else if line_is_blank lines.(index).text then
                (List.rev acc, index)
              else
                match parse_table_row flavor lines.(index).text with
                | None -> (List.rev acc, index)
                | Some row -> collect_rows (index + 1) (normalize_row row :: acc)
            in
            let rows, next = collect_rows (start + 2) [] in
            let span_end = line_end lines.(next - 1) in
            let span = make_span ~start:lines.(start).start ~len:(span_end - lines.(start).start) in
            Some (Table { header = normalize_row header_cells; rows; span }, next, [])

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
                    match consume_indent_columns text continuation_min with
                    | None -> (index, List.rev acc, had_blank)
                    | Some content_offset ->
                        collect_item_body
                          (index + 1)
                          ({
                            text = remove_prefix text content_offset;
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
                  let head_text = remove_prefix lines.(index).text marker.marker_after in
                  let head_start = lines.(index).start + marker.marker_after in
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
                  let span_end =
                    if Array.length body_lines = 0 then
                      line_end lines.(index)
                    else
                      line_end body_lines.(Array.length body_lines - 1)
                  in
                  let span = make_span ~start:head_start ~len:(span_end - head_start) in
                  let item =
                    match task with
                    | Some checked -> [ Task_list_item { checked; blocks = body_blocks; span } ]
                    | None -> [ List_item { blocks = body_blocks; span } ]
                  in
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
          let span_end =
            if next = start then
              line_end lines.(start)
            else
              line_end lines.(next - 1)
          in
          let span = make_span ~start:lines.(start).start ~len:(span_end - lines.(start).start) in
          Some (List { ordered = first.ordered; tight = not loose; items; span }, next, diagnostics)

and parse_raw_html_line = fun lines start ->
  if start >= Array.length lines then
    None
  else
    let line = lines.(start).text in
    if String.length line = 0 || char_not line.[0] '<' then
      None
    else
      match find_substring line 1 ">" with
      | None -> None
      | Some close ->
          let html = String.sub line 0 (close + 1) in
          let span = make_span ~start:lines.(start).start ~len:(String.length html) in
          Some (Raw_html { html; span }, start + 1, [])

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
    let end_line =
      match setext with
      | Some (_, underline) -> underline
      | None ->
          if next <= start then
            lines.(start)
          else
            lines.(next - 1)
    in
    let span = make_span ~start:lines.(start).start ~len:(line_end end_line - lines.(start).start) in
    match parse_heading lines.(start).text with
    | Some (level, content) ->
        Some (Heading { level; inlines = parse_inline ~flavor content; span }, next, [])
    | None ->
        (
          match setext with
          | Some (level, _) ->
              Some (Heading { level; inlines = parse_inline ~flavor setext_text; span }, next, [])
          | None ->
              Some (Paragraph { inlines = parse_inline ~flavor text; span }, next, [])
        )

and parse_inline = fun ~flavor text ->
  let len = String.length text in
  let rec loop index acc =
    if index >= len then
      List.rev acc
    else if text.[index] = '\\' then
      if index + 1 < len then
        loop (index + 2) (Text (String.make 1 text.[index + 1]) :: acc)
      else
        loop (index + 1) (Text "\\" :: acc)
    else if starts_with ~prefix:"**" text index then
      (
        match find_substring text (index + 2) "**" with
        | None -> loop (index + 2) (Text "**" :: acc)
        | Some close ->
            let body = String.sub text (index + 2) (close - index - 2) in
            loop (close + 2) (Strong (parse_inline ~flavor body) :: acc)
      )
    else if starts_with ~prefix:"~~" text index then
      (
        match find_substring text (index + 2) "~~" with
        | None -> loop (index + 2) (Text "~~" :: acc)
        | Some close ->
            let body = String.sub text (index + 2) (close - index - 2) in
            if is_gfm flavor then
              loop (close + 2) (Strikethrough (parse_inline ~flavor body) :: acc)
            else
              loop (close + 2) (Text (String.sub text index (close - index + 2)) :: acc)
      )
    else if text.[index] = '*' then
      (
        match find_substring text (index + 1) "*" with
        | None -> loop (index + 1) (Text "*" :: acc)
        | Some close ->
            if close > index + 1 then
              let body = String.sub text (index + 1) (close - index - 1) in
              loop (close + 1) (Emphasis (parse_inline ~flavor body) :: acc)
            else
              loop (index + 1) (Text "*" :: acc)
      )
    else if text.[index] = '`' then
      let rec count_backticks current =
        if current < len && text.[current] = '`' then
          count_backticks (current + 1)
        else
          current
      in
      let close_start = count_backticks (index + 1) in
      let marker_len = close_start - index in
      let marker = string_of_char '`' marker_len in
      (
        match find_substring text close_start marker with
        | None -> loop (index + marker_len) (Text (String.sub text index marker_len) :: acc)
        | Some close ->
            let body =
              if close <= close_start then
                ""
              else
                String.sub text close_start (close - close_start)
            in
            loop (close + marker_len) (Code_span body :: acc)
      )
    else if text.[index] = '[' then
      (
        match find_substring text (index + 1) "]" with
        | None -> loop (index + 1) (Text "[" :: acc)
        | Some close_text ->
            if close_text + 1 >= len then
              loop (close_text + 1) (Text (String.sub text index (close_text - index + 1)) :: acc)
            else if text.[close_text + 1] = '(' then
              (
                match find_substring text (close_text + 2) ")" with
                | None ->
                    loop
                      (close_text + 1)
                      (Text (String.sub text index (close_text - index + 1)) :: acc)
                | Some close_link ->
                    let label_text = String.sub text (index + 1) (close_text - index - 1) in
                    let destination =
                      String.sub text (close_text + 2) (close_link - close_text - 2) |> trim
                    in
                    loop
                      (close_link + 1)
                      (Link { label = parse_inline ~flavor label_text; destination } :: acc)
              )
            else
              loop (close_text + 1) (Text (String.sub text index (close_text - index + 1)) :: acc)
      )
    else if text.[index] = '<' then
      (
        match find_substring text (index + 1) ">" with
        | None -> loop (index + 1) (Text "<" :: acc)
        | Some close ->
            let inside = String.sub text (index + 1) (close - index - 1) in
            if has_char inside ' ' then
              loop (close + 1) (Text (String.sub text index (close - index + 1)) :: acc)
            else
              loop (close + 1) (Raw_html (String.sub text index (close - index + 1)) :: acc)
      )
    else
      let rec scan current =
        if current >= len then
          current
        else
          match text.[current] with
          | '\\' | '*' | '~' | '`' | '[' | '<' -> current
          | _ -> scan (current + 1)
      in
      let next = scan (index + 1) in
      loop next (Text (String.sub text index (next - index)) :: acc)
  in
  let parsed = loop 0 [] in
  if parsed = [] then [ Text text ] else parsed

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
                              match parse_list flavor lines start with
                              | Some result -> Some result
                              | None ->
                                  if is_some (parse_thematic_break lines.(start).text) then
                                    Some (
                                      Horizontal_rule
                                        (make_span
                                           ~start:lines.(start).start
                                           ~len:(String.length lines.(start).text)),
                                      start + 1,
                                      []
                                    )
                                  else
                                    (
                                      match parse_raw_html_line lines start with
                                      | Some result -> Some result
                                      | None -> parse_paragraph flavor lines start
                                    )
                            )
                      )
                )
          )
    in
    match block with
    | Some (node, next, diagnostics) ->
        let parsed_next, nested = parse_blocks ~flavor lines next in
        (node :: parsed_next, diagnostics @ nested)
    | None ->
        parse_blocks ~flavor lines (start + 1)

let rec to_green = fun ~source blocks ->
  ignore source;
  let token kind text = Ceibo.Builder.make_token ~kind ~text ~width:(String.length text) in
  let html_escape = fun text ->
    let buffer = IO.Buffer.create (String.length text) in
    String.iter
      (fun char ->
        match char with
        | '&' -> IO.Buffer.add_string buffer "&amp;"
        | '<' -> IO.Buffer.add_string buffer "&lt;"
        | '>' -> IO.Buffer.add_string buffer "&gt;"
        | '"' -> IO.Buffer.add_string buffer "&quot;"
        | _ -> IO.Buffer.add_char buffer char)
      text;
    IO.Buffer.contents buffer
  in
  let rec inline_to_children = fun inline ->
    match inline with
    | Text text ->
        [ token Markdown_syntax_kind.Text (html_escape text) ]
    | Emphasis inlines ->
        [ token Markdown_syntax_kind.Text "<em>" ]
        @ List.concat_map inline_to_children inlines
        @ [ token Markdown_syntax_kind.Text "</em>" ]
    | Strong inlines ->
        [ token Markdown_syntax_kind.Text "<strong>" ]
        @ List.concat_map inline_to_children inlines
        @ [ token Markdown_syntax_kind.Text "</strong>" ]
    | Strikethrough inlines ->
        [ token Markdown_syntax_kind.Text "<del>" ]
        @ List.concat_map inline_to_children inlines
        @ [ token Markdown_syntax_kind.Text "</del>" ]
    | Code_span text ->
        [
          token Markdown_syntax_kind.Text "<code>";
          token Markdown_syntax_kind.Text (html_escape text);
          token Markdown_syntax_kind.Text "</code>";
        ]
    | Raw_html html ->
        [ token Markdown_syntax_kind.Raw_html html ]
    | Link { label; destination } ->
        let label_children = List.concat_map inline_to_children label in
        [ token Markdown_syntax_kind.Text "<a href=\"" ]
        @ [ token Markdown_syntax_kind.Text (html_escape destination) ]
        @ [ token Markdown_syntax_kind.Text "\">" ]
        @ label_children
        @ [ token Markdown_syntax_kind.Text "</a>" ]
  in
  let rec block_to_node = fun block ->
    match block with
    | Heading { inlines; _ } ->
        Ceibo.Builder.make_node
          ~kind:Markdown_syntax_kind.Heading
          (List.concat_map inline_to_children inlines)
    | Paragraph { inlines; _ } ->
        Ceibo.Builder.make_node
          ~kind:Markdown_syntax_kind.Paragraph
          (List.concat_map inline_to_children inlines)
    | Block_quote { blocks; _ } ->
        Ceibo.Builder.make_node
          ~kind:Markdown_syntax_kind.Block_quote
          (List.map block_to_node blocks)
    | List { items; _ } ->
        let item_nodes =
          List.map
            (fun item ->
              match item with
              | [ List_item _ as node ] -> block_to_node node
              | [ Task_list_item _ as node ] -> block_to_node node
              | nodes ->
                  Ceibo.Builder.make_node
                    ~kind:Markdown_syntax_kind.List_item
                    (List.map block_to_node nodes))
            items
        in
        Ceibo.Builder.make_node ~kind:Markdown_syntax_kind.List item_nodes
    | Task_list_item { checked; blocks; _ } ->
        let checkbox =
          if checked then
            "<input type=\"checkbox\" checked disabled />"
          else
            "<input type=\"checkbox\" disabled />"
        in
        Ceibo.Builder.make_node
          ~kind:Markdown_syntax_kind.Task_list_item
          (token Markdown_syntax_kind.Raw_html checkbox :: List.map block_to_node blocks)
    | List_item { blocks; _ } ->
        Ceibo.Builder.make_node
          ~kind:Markdown_syntax_kind.List_item
          (List.map block_to_node blocks)
    | Code_block { info; code; _ } ->
        if info = "" then
          Ceibo.Builder.make_node
            ~kind:Markdown_syntax_kind.Code_block
            [ token Markdown_syntax_kind.Text code ]
        else
          Ceibo.Builder.make_node
            ~kind:Markdown_syntax_kind.Code_block
            [
              token Markdown_syntax_kind.Text info;
              token Markdown_syntax_kind.Text "\n";
              token Markdown_syntax_kind.Text code;
            ]
    | Horizontal_rule _ ->
        Ceibo.Builder.make_node ~kind:Markdown_syntax_kind.Horizontal_rule []
    | Raw_html { html; _ } ->
        Ceibo.Builder.make_node
          ~kind:Markdown_syntax_kind.Raw_html
          [ token Markdown_syntax_kind.Raw_html html ]
    | Table { header; rows; _ } ->
        let row_to_node = fun row ->
          let cells =
            List.map
              (fun cell ->
                Ceibo.Builder.make_node
                  ~kind:Markdown_syntax_kind.Table_cell
                  (List.concat_map inline_to_children cell))
              row.cells
          in
          Ceibo.Builder.make_node ~kind:Markdown_syntax_kind.Table_row cells
        in
        Ceibo.Builder.make_node
          ~kind:Markdown_syntax_kind.Table
          (row_to_node header :: List.map row_to_node rows)
    | Error_block { message; _ } ->
        Ceibo.Builder.make_node
          ~kind:Markdown_syntax_kind.Error
          [ token Markdown_syntax_kind.Text message ]
  in
  Ceibo.Green.make_node
    ~kind:Markdown_syntax_kind.Document
    ~children:(List.map block_to_node blocks)

let parse = fun ?(flavor = Markdown) source ->
  let source = normalize_newlines source in
  let lines = split_lines source in
  let fallback_span = make_span ~start:0 ~len:(String.length source) in
  let result =
    try Some (parse_blocks ~flavor lines 0) with
    | exn ->
        let message = Exception.to_string exn in
        let diagnostic =
          parser_internal
            ~found:{ kind = "parser"; text = "internal" }
            ~message
            ~span:fallback_span
        in
        Some (
          [ Error_block { message = "Parser error. See diagnostics."; span = fallback_span } ],
          [ diagnostic ]
        )
  in
  let blocks, diagnostics =
    match result with
    | Some value -> value
    | None -> ([], [])
  in
  let diagnostics = List.rev_append (make_control_diagnostics source) diagnostics in
  { source; blocks; diagnostics }

let blocks = fun parsed -> parsed.blocks
