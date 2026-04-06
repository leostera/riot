open Std
open Std.Collections
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

type line = {
  text: string;
  start: int;
}

type list_marker = {
  indent: int;
  marker_len: int;
  marker_after: int;
  ordered: bool;
}

let is_space = fun char -> char = ' ' || char = '\t'

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
          IO.Buffer.add_char buffer char
        else
          ())
      source;
    IO.Buffer.contents buffer

let split_lines = fun source ->
  let length = String.length source in
  let rec loop index line_start acc =
    if index >= length then
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

let make_span = fun ~start ~len -> Ceibo.Span.make ~start ~end_:((start + len))

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
  let rec loop index count =
    if index >= len || count >= max_indent || not (is_space text.[index]) then
      count
    else
      loop (index + 1) (count + 1)
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
  let start =
    if start < 0 then
      0
    else
      start
  in
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
        let diag = unexpected_control_character
          ~found
          ~code:(Char.code char)
          ~span:(make_span ~start:index ~len:1) in
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
        let rec loop idx =
          if idx <= 0 then
            idx
          else if title.[idx - 1] = '#' then
            loop (idx - 1)
          else
            idx
        in
        loop (String.length title)
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
      if loop 0 then
        Some marker
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
      let rec count idx =
        if idx < String.length text && text.[idx] = marker then
          count (idx + 1)
        else
          idx
      in
      let after = count indent in
      let marker_len = after - indent in
      if marker_len < 3 then
        None
      else
        let info = trim (remove_prefix text after) in
        Some (marker, marker_len, indent, info)

let parse_fence_close = fun marker_len marker line ->
  let indent = count_indent line.text 3 in
  let remaining = remove_prefix line.text indent in
  if String.length remaining < marker_len then
    false
  else
    let marker_text = String.sub remaining 0 marker_len in
    if not (string_all_equal marker marker_text) then
      false
    else
      trim_left (remove_prefix remaining marker_len) = ""

let parse_fenced_code_block = fun lines start ->
  if start >= Array.length lines then
    None
  else
    let first = lines.(start) in
    match parse_fence_open first.text with
    | None -> None
    | Some (marker, marker_len, _indent, info) ->
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
              let code_lines = Array.to_list lines
              |> drop (start + 1)
              |> List.map (fun line -> line.text) in
              let content =
                if code_lines = [] then
                  ""
                else
                  String.concat "\n" code_lines ^ "\n"
              in
              let span = make_span ~start:first.start ~len:((line_end first - first.start)) in
              let found = { kind = "fence"; text = marker_text } in
              let diag = unclosed_fenced_code_block ~found ~opener:marker_text ~span in
              Some (
                Code_block { info; code = content; span; fenced = true },
                Array.length lines,
                [ diag ]
              )
          | Some close_idx ->
              let code_lines = Array.to_list lines
              |> take (close_idx - (start + 1))
              |> List.map (fun line -> line.text) in
              let content =
                if code_lines = [] then
                  ""
                else
                  String.concat "\n" code_lines ^ "\n"
              in
              let span_end =
                if close_idx < Array.length lines then
                  line_end lines.(close_idx)
                else
                  line_end lines.(Array.length lines - 1)
              in
              let span = make_span ~start:first.start ~len:((span_end - first.start)) in
              Some (Code_block { info; code = content; span; fenced = true }, close_idx + 1, [])
        )

let parse_indented_code_block = fun lines start ->
  if start >= Array.length lines then
    None
  else
    let first = lines.(start) in
    let first_indented =
      starts_with ~prefix:"    " first.text 0
      || (String.length first.text > 0 && first.text.[0] = '\t') in
    if not first_indented then
      None
    else
      let strip_line text =
        if starts_with ~prefix:"    " text 0 then
          remove_prefix text 4
        else if String.length text > 0 && text.[0] = '\t' then
          remove_prefix text 1
        else
          ""
      in
      let rec collect index acc =
        if index >= Array.length lines then
          (index, List.rev acc)
        else if
          starts_with ~prefix:"    " lines.(index).text 0
          || (String.length lines.(index).text > 0 && lines.(index).text.[0] = '\t')
        then
          collect (index + 1) (strip_line lines.(index).text :: acc)
        else if line_is_blank lines.(index).text then
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
      let span = make_span ~start:first.start ~len:((line_end end_line - first.start)) in
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
    Some (indent, content_start)

let rec parse_block_quote = fun lines start ->
  let first = lines.(start) in
  match parse_block_quote_prefix first.text with
  | None -> None
  | Some (_, content_start) ->
      let rec collect index acc =
        if index >= Array.length lines then
          (List.rev acc, index)
        else
          let line = lines.(index).text in
          if line_is_blank line then
            collect (index + 1) ({ text = ""; start = lines.(index).start } :: acc)
          else
            (
              match parse_block_quote_prefix line with
              | None -> (List.rev acc, index)
              | Some (_, nested_start) ->
                  let text = remove_prefix line nested_start in
                  let start_offset = lines.(index).start + nested_start in
                  collect (index + 1) ({ text; start = start_offset } :: acc)
            )
      in
      let quote_lines, next = collect
        (start + 1)
        [ { text = remove_prefix first.text content_start; start = first.start + content_start } ] in
      if quote_lines = [] then
        None
      else
        let quote_array = Array.of_list quote_lines in
        let nested_blocks, nested_diagnostics = parse_blocks quote_array 0 in
        let span_end =
          if Array.length quote_array = 0 then
            line_end first
          else
            line_end quote_array.(Array.length quote_array - 1)
        in
        let span = make_span ~start:first.start ~len:((span_end - first.start)) in
        Some (Block_quote { blocks = nested_blocks; span }, next, nested_diagnostics)

and parse_list_marker = fun text ->
  let len = String.length text in
  if len = 0 then
    None
  else
    let indent = count_indent text 3 in
    if indent >= len then
      None
    else
      let marker = text.[indent] in
      if marker = '-' || marker = '*' || marker = '+' then
        if indent + 1 < len && is_space text.[indent + 1] then
          Some { indent; marker_len = 1; marker_after = indent + 2; ordered = false }
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
              Some {
                indent;
                marker_len = index - indent + 1;
                marker_after = index + 3;
                ordered = true
              }
            else
              None
          else
            scan (index + 1)
        in
        scan (indent + 1)

and parse_list = fun lines start ->
  if start >= Array.length lines then
    None
  else
    match parse_list_marker lines.(start).text with
    | None -> None
    | Some first ->
        let continuation_min = first.indent + first.marker_len + 1 in
        let rec collect_item_body index acc =
          if index >= Array.length lines then
            (index, List.rev acc)
          else
            let line = lines.(index).text in
            if line_is_blank line then
              collect_item_body (index + 1) ({ text = ""; start = lines.(index).start } :: acc)
            else
              (
                match parse_list_marker line with
                | Some next when next.indent = first.indent && next.ordered = first.ordered -> (
                  index,
                  List.rev acc
                )
                | _ ->
                    let indent = count_indent line 3 in
                    if indent >= continuation_min then
                      collect_item_body
                        (index + 1)
                        ({
                          text = remove_prefix line continuation_min;
                          start = lines.(index).start + continuation_min
                        }
                        :: acc)
                    else
                      (index, List.rev acc)
              )
        in
        let rec collect_items index acc =
          if index >= Array.length lines then
            (List.rev acc, index, [])
          else
            match parse_list_marker lines.(index).text with
            | Some marker when marker.indent = first.indent && marker.ordered = first.ordered ->
                let head_text = remove_prefix lines.(index).text marker.marker_after in
                let head = { text = head_text; start = lines.(index).start + marker.marker_after } in
                let next_idx, body_lines = collect_item_body (index + 1) [ head ] in
                let body_lines = Array.of_list body_lines in
                let body_blocks, body_diagnostics = parse_blocks body_lines 0 in
                let span_end =
                  if Array.length body_lines = 0 then
                    line_end lines.(index)
                  else
                    line_end body_lines.(Array.length body_lines - 1)
                in
                let span = make_span ~start:head.start ~len:((span_end - head.start)) in
                let item = List_item { blocks = body_blocks; span } in
                let items, parsed_next, nested_diagnostics = collect_items next_idx (item :: acc) in
                (items, parsed_next, body_diagnostics @ nested_diagnostics)
            | _ -> (List.rev acc, index, [])
        in
        let items, next, diagnostics = collect_items start [] in
        if items = [] then
          None
        else
          let span_end =
            if next = start then
              line_end lines.(start)
            else
              line_end lines.(next - 1)
          in
          let span = make_span ~start:lines.(start).start ~len:((span_end - lines.(start).start)) in
          Some (List { ordered = first.ordered; items = [ items ]; span }, next, diagnostics)

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

and parse_paragraph = fun lines start ->
  if start >= Array.length lines then
    None
  else
    let len = Array.length lines in
    let is_block_start text =
      is_some (parse_heading text)
      || is_some (parse_fence_open text)
      || is_some (parse_list_marker text)
      || is_some (parse_block_quote_prefix text)
      || is_some (parse_thematic_break text)
      || is_some (parse_raw_html_line lines start) in
    let rec collect index acc =
      if index >= len || line_is_blank lines.(index).text then
        (index, List.rev acc)
      else if index > start && is_block_start lines.(index).text then
        (index, List.rev acc)
      else
        collect (index + 1) (lines.(index).text :: acc)
    in
    let next, texts = collect (start + 1) [ lines.(start).text ] in
    if texts = [] then
      None
    else
      let text = String.concat "\n" texts in
      let span = make_span ~start:lines.(start).start
        ~len:((
          line_end
            lines.(if next <= start then
              start
            else
              next - 1) - lines.(start).start
        ))
      in
      (
        match parse_heading lines.(start).text with
        | Some (level, content) -> Some (
          Heading { level; inlines = parse_inline content; span },
          next,
          []
        )
        | None -> Some (Paragraph { inlines = parse_inline text; span }, next, [])
      )

and parse_inline = fun text ->
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
            loop (close + 2) (Strong (parse_inline body) :: acc)
      )
    else if text.[index] = '*' then
      (
        match find_substring text (index + 1) "*" with
        | None -> loop (index + 1) (Text "*" :: acc)
        | Some close ->
            if close > index + 1 then
              let body = String.sub text (index + 1) (close - index - 1) in
              loop (close + 1) (Emphasis (parse_inline body) :: acc)
            else
              loop (index + 1) (Text "*" :: acc)
      )
    else if text.[index] = '`' then
      let rec count_backticks i =
        if i < len && text.[i] = '`' then
          count_backticks (i + 1)
        else
          i
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
            if close_text + 1 < len && text.[close_text + 1] = '(' then
              (
                match find_substring text (close_text + 2) ")" with
                | None -> loop
                  (close_text + 1)
                  (Text (String.sub text index (close_text - index + 1)) :: acc)
                | Some close_link ->
                    let label_text = String.sub text (index + 1) (close_text - index - 1) in
                    let destination = String.sub text (close_text + 2) (close_link - close_text - 2)
                    |> trim in
                    loop
                      (close_link + 1)
                      (Link { label = parse_inline label_text; destination } :: acc)
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
      let rec scan i =
        if i >= len then
          i
        else
          match text.[i] with
          | '\\'
          | '*'
          | '`'
          | '['
          | '<' -> i
          | _ -> scan (i + 1)
      in
      let next = scan (index + 1) in
      let piece = String.sub text index (next - index) in
      loop next (Text piece :: acc)
  in
  let parsed = loop 0 [] in
  if parsed = [] then
    [ Text text ]
  else
    parsed

and parse_blocks = fun lines start ->
  if start >= Array.length lines then
    ([], [])
  else if line_is_blank lines.(start).text then
    parse_blocks lines (start + 1)
  else
    let block =
      match parse_fenced_code_block lines start with
      | Some result -> Some result
      | None -> (
          match parse_indented_code_block lines start with
          | Some result -> Some result
          | None -> (
              match parse_block_quote lines start with
              | Some result -> Some result
              | None -> (
                  match parse_list lines start with
                  | Some result -> Some result
                  | None ->
                      if is_some (parse_thematic_break lines.(start).text) then
                        Some (
                          Horizontal_rule (make_span
                            ~start:lines.(start).start
                            ~len:(String.length lines.(start).text)),
                          start + 1,
                          []
                        )
                      else
                        (
                          match parse_raw_html_line lines start with
                          | Some result -> Some result
                          | None -> parse_paragraph lines start
                        )
                )
            )
        )
    in
    (
      match block with
      | Some (node, next, diags) ->
          let parsed_next, nested = parse_blocks lines next in
          (node :: parsed_next, diags @ nested)
      | None -> parse_blocks lines (start + 1)
    )

let rec to_green = fun ~source blocks ->
  ignore source;
  let token kind text = Ceibo.Builder.make_token ~kind ~text ~width:(String.length text) in
  let html_escape text =
    let buffer = IO.Buffer.create (String.length text) in
    String.iter
      (fun ch ->
        match ch with
        | '&' -> IO.Buffer.add_string buffer "&amp;"
        | '<' -> IO.Buffer.add_string buffer "&lt;"
        | '>' -> IO.Buffer.add_string buffer "&gt;"
        | '"' -> IO.Buffer.add_string buffer "&quot;"
        | '\'' -> IO.Buffer.add_string buffer "&#39;"
        | _ -> IO.Buffer.add_char buffer ch)
      text;
    IO.Buffer.contents buffer
  in
  let rec inline_to_children inline =
    match inline with
    | Text text ->
        [ token Commonmark_syntax_kind.Text (html_escape text) ]
    | Emphasis inlines ->
        [ token Commonmark_syntax_kind.Text "<em>" ]
        @ List.concat_map inline_to_children inlines
        @ [ token Commonmark_syntax_kind.Text "</em>" ]
    | Strong inlines ->
        [ token Commonmark_syntax_kind.Text "<strong>" ]
        @ List.concat_map inline_to_children inlines
        @ [ token Commonmark_syntax_kind.Text "</strong>" ]
    | Code_span text ->
        [
          token Commonmark_syntax_kind.Text "<code>";
          token Commonmark_syntax_kind.Text (html_escape text);
          token Commonmark_syntax_kind.Text "</code>"
        ]
    | Raw_html html ->
        [ token Commonmark_syntax_kind.Raw_html html ]
    | Link { label; destination } ->
        let label_children = List.concat_map inline_to_children label in
        [ token Commonmark_syntax_kind.Text "<a href=\"" ]
        @ [ token Commonmark_syntax_kind.Text (html_escape destination) ]
        @ [ token Commonmark_syntax_kind.Text "\">" ]
        @ label_children
        @ [ token Commonmark_syntax_kind.Text "</a>" ]
  in
  let rec block_to_node = function
    | Heading { inlines; _ } ->
        Ceibo.Builder.make_node
          ~kind:Commonmark_syntax_kind.Heading (List.concat_map inline_to_children inlines)
    | Paragraph { inlines; _ } ->
        Ceibo.Builder.make_node
          ~kind:Commonmark_syntax_kind.Paragraph (List.concat_map inline_to_children inlines)
    | Block_quote { blocks; _ } ->
        Ceibo.Builder.make_node
          ~kind:Commonmark_syntax_kind.Block_quote (List.map block_to_node blocks)
    | List { ordered=_; items; _ } ->
        let rendered_items = items
        |> List.map
          (fun item ->
            Ceibo.Builder.make_node
              ~kind:Commonmark_syntax_kind.List_item (List.map block_to_node item)) in
        Ceibo.Builder.make_node ~kind:Commonmark_syntax_kind.List rendered_items
    | List_item { blocks; _ } ->
        Ceibo.Builder.make_node
          ~kind:Commonmark_syntax_kind.List_item (List.map block_to_node blocks)
    | Code_block { info; code; _ } ->
        if info = "" then
          Ceibo.Builder.make_node
            ~kind:Commonmark_syntax_kind.Code_block [ token Commonmark_syntax_kind.Text code ]
        else
          Ceibo.Builder.make_node
            ~kind:Commonmark_syntax_kind.Code_block [
              token Commonmark_syntax_kind.Text info;
              token Commonmark_syntax_kind.Text "\n";
              token Commonmark_syntax_kind.Text code
            ]
    | Horizontal_rule _ ->
        Ceibo.Builder.make_node ~kind:Commonmark_syntax_kind.Horizontal_rule []
    | Raw_html { html; _ } ->
        Ceibo.Builder.make_node
          ~kind:Commonmark_syntax_kind.Raw_html [ token Commonmark_syntax_kind.Raw_html html ]
    | Error_block { message; _ } ->
        Ceibo.Builder.make_node
          ~kind:Commonmark_syntax_kind.Error [ token Commonmark_syntax_kind.Text message ]
  in
  Ceibo.Green.make_node
    ~kind:Commonmark_syntax_kind.Document
    ~children:(List.map block_to_node blocks)

let parse = fun source ->
  let source = normalize_newlines source in
  let lines = split_lines source in
  let fallback_span = make_span ~start:0 ~len:(String.length source) in
  let parse_result =
    try Some (parse_blocks lines 0) with
    | exn ->
        let message = Exception.to_string exn in
        let fallback_diagnostic = parser_internal
          ~found:{ kind = "parser"; text = "internal" }
          ~message
          ~span:fallback_span in
        Some (
          [ Error_block { message = "Parser error. See diagnostics."; span = fallback_span } ],
          [ fallback_diagnostic ]
        )
  in
  let blocks, diagnostics =
    match parse_result with
    | Some value -> value
    | None -> ([], [])
  in
  let diagnostics = List.rev_append (make_control_diagnostics source) diagnostics in
  { source; blocks; diagnostics }

let blocks = fun parsed -> parsed.blocks
