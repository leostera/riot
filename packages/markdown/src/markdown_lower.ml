open Std
open Markdown_parser

type reference = {
  destination: string;
  title: string option;
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

let string_of_char = fun char len ->
  if len <= 0 then
    ""
  else
    String.make len char

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

let normalize_code_span = fun text ->
  let buffer = IO.Buffer.create (String.length text) in
  String.iter
    (fun char ->
      if char = '\n' then
        IO.Buffer.add_char buffer ' '
      else
        IO.Buffer.add_char buffer char)
    text;
  let normalized = IO.Buffer.contents buffer in
  let len = String.length normalized in
  if
    len >= 2
    && normalized.[0] = ' '
    && normalized.[len - 1] = ' '
    && not (String.equal (trim normalized) "")
  then
    String.sub normalized 1 (len - 2)
  else
    normalized

let unescape_backslashes = fun text ->
  let buffer = IO.Buffer.create (String.length text) in
  let rec loop index =
    if index >= String.length text then
      IO.Buffer.contents buffer
    else if text.[index] = '\\' && index + 1 < String.length text then
      (
        IO.Buffer.add_char buffer text.[index + 1];
        loop (index + 2)
      )
    else
      (
        IO.Buffer.add_char buffer text.[index];
        loop (index + 1)
      )
  in
  loop 0

let is_escapable = function
  | '!' | '"' | '#' | '$' | '%' | '&' | '\'' | '(' | ')' | '*' | '+' | ',' | '-'
  | '.' | '/' | ':' | ';' | '<' | '=' | '>' | '?' | '@' | '[' | '\\' | ']' | '^'
  | '_' | '`' | '{' | '|' | '}' | '~' ->
      true
  | _ ->
      false

let decode_codepoint = fun code ->
  if code <= 0 then
    Unicode.Rune.to_string Unicode.Rune.replacement
  else
    match Unicode.Rune.of_int code with
    | Some rune -> Unicode.Utf8.encode_rune rune
    | None -> Unicode.Rune.to_string Unicode.Rune.replacement

let int_of_string_opt = fun text ->
  try Some (int_of_string text) with
  | _ -> None

let named_entities = [
  ("nbsp", "\u{00A0}");
  ("amp", "&");
  ("copy", "\u{00A9}");
  ("AElig", "\u{00C6}");
  ("Dcaron", "\u{010E}");
  ("frac34", "\u{00BE}");
  ("HilbertSpace", "\u{210B}");
  ("DifferentialD", "\u{2146}");
  ("ClockwiseContourIntegral", "\u{2232}");
  ("ngE", "\u{2267}\u{0338}");
  ("ouml", "\u{00F6}");
]

let decode_entity_at = fun text index ->
  let len = String.length text in
  if index >= len || not (Char.equal text.[index] '&') then
    None
  else
    let rec find_end current =
      if current >= len then
        None
      else if text.[current] = ';' then
        Some current
      else
        find_end (current + 1)
    in
    match find_end (index + 1) with
    | None -> None
    | Some end_index ->
        let body = String.sub text (index + 1) (end_index - index - 1) in
        let decoded =
          if String.length body > 1 && body.[0] = '#' then
            let numeric =
              if body.[1] = 'x' || body.[1] = 'X' then
                int_of_string_opt ("0x" ^ String.sub body 2 (String.length body - 2))
              else
                int_of_string_opt (String.sub body 1 (String.length body - 1))
            in
            Option.map decode_codepoint numeric
          else
            List.assoc_opt body named_entities
        in
        Option.map (fun value -> (value, end_index + 1)) decoded

let decode_entities = fun text ->
  let buffer = IO.Buffer.create (String.length text) in
  let rec loop index =
    if index >= String.length text then
      IO.Buffer.contents buffer
    else
      match decode_entity_at text index with
      | Some (decoded, next) ->
          IO.Buffer.add_string buffer decoded;
          loop next
      | None ->
          IO.Buffer.add_char buffer text.[index];
          loop (index + 1)
  in
  loop 0

let parse_link_target = fun raw ->
  let raw = trim raw in
  let len = String.length raw in
  if len = 0 then
    ("", None)
  else
    let dest_end, destination =
      if raw.[0] = '<' then
        match find_substring raw 1 ">" with
        | Some close ->
            (close + 1, String.sub raw 1 (close - 1))
        | None ->
            (len, raw)
      else
        let rec scan index =
          if index >= len then
            index
          else if raw.[index] = ' ' || raw.[index] = '\t' || raw.[index] = '\n' then
            index
          else
            scan (index + 1)
        in
        let dest_end = scan 0 in
        (dest_end, String.sub raw 0 dest_end)
    in
    let rest =
      if dest_end >= len then
        ""
      else
        String.sub raw dest_end (len - dest_end) |> trim
    in
    let title =
      if rest = "" then
        None
      else
        let opener = rest.[0] in
        let closer =
          match opener with
          | '"' -> Some '"'
          | '\'' -> Some '\''
          | '(' -> Some ')'
          | _ -> None
        in
        match closer with
        | None -> None
        | Some closer ->
            let rest_len = String.length rest in
            if rest_len >= 2 && rest.[rest_len - 1] = closer then
              Some (String.sub rest 1 (rest_len - 2) |> unescape_backslashes |> decode_entities)
            else
              None
    in
    (unescape_backslashes destination |> decode_entities, title)

let normalize_reference_label = fun label ->
  let label = decode_entities label |> trim |> String.lowercase_ascii in
  let buffer = IO.Buffer.create (String.length label) in
  let rec loop index previous_space =
    if index >= String.length label then
      IO.Buffer.contents buffer
    else
      let char = label.[index] in
      if char = ' ' || char = '\t' || char = '\n' then
        if previous_space then
          loop (index + 1) true
        else
          (
            IO.Buffer.add_char buffer ' ';
            loop (index + 1) true
          )
      else
        (
          IO.Buffer.add_char buffer char;
          loop (index + 1) false
        )
  in
  loop 0 false |> trim

let parse_reference_definition = fun text ->
  try
    if String.length text = 0 || not (Char.equal text.[0] '[') then
      None
    else
      match find_substring text 1 "]:" with
      | None -> None
      | Some close ->
          let label = String.sub text 1 (close - 1) in
          let remainder =
            if close + 2 >= String.length text then
              ""
            else
              String.sub text (close + 2) (String.length text - close - 2)
          in
          let destination, title = parse_link_target remainder in
          if destination = "" then
            None
          else
            Some (normalize_reference_label label, { destination; title })
  with
  | _ ->
      None

let find_reference = fun references label ->
  List.assoc_opt (normalize_reference_label label) references

let looks_like_email = fun inside ->
  let len = String.length inside in
  let at = find_substring inside 0 "@" in
  match at with
  | None -> false
  | Some at ->
      let local = String.sub inside 0 at in
      at > 0
      && at + 1 < len
      && let last = local.[String.length local - 1] in
         ((last >= 'a' && last <= 'z')
         || (last >= 'A' && last <= 'Z')
         || (last >= '0' && last <= '9'))
      && Option.is_some (find_substring inside (at + 1) ".")

let looks_like_scheme = fun inside ->
  let len = String.length inside in
  let rec scan index =
    if index >= len then
      None
    else if inside.[index] = ':' then
      Some index
    else if
      (inside.[index] >= 'a' && inside.[index] <= 'z')
      || (inside.[index] >= 'A' && inside.[index] <= 'Z')
      || (inside.[index] >= '0' && inside.[index] <= '9')
      || inside.[index] = '+'
      || inside.[index] = '-'
      || inside.[index] = '.'
    then
      scan (index + 1)
    else
      None
  in
  match scan 0 with
  | Some colon -> colon >= 2 && colon + 1 < len
  | None -> false

let autolink_destination = fun inside ->
  if has_char inside ' ' then
    None
  else if looks_like_scheme inside then
    Some inside
  else if looks_like_email inside then
    Some ("mailto:" ^ inside)
  else
    None

let looks_like_raw_html = fun inside ->
  let len = String.length inside in
  if len = 0 then
    false
  else if inside.[0] = '!' || inside.[0] = '?' then
    true
  else
    let start =
      if inside.[0] = '/' then
        1
      else
        0
    in
    if start >= len then
      false
    else
      let rec scan index =
        if index >= len then
          index
        else
          let char = inside.[index] in
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
      finish > start
      && (
        finish = len
        || inside.[finish] = ' '
        || inside.[finish] = '\t'
        || inside.[finish] = '\n'
        || inside.[finish] = '/'
        || inside.[finish] = '>'
      )

let trailing_space_count = fun text ->
  let rec loop index count =
    if index < 0 then
      count
    else if text.[index] = ' ' || text.[index] = '\t' then
      loop (index - 1) (count + 1)
    else
      count
  in
  loop (String.length text - 1) 0

let trim_trailing_spaces = fun text ->
  let count = trailing_space_count text in
  if count = 0 then
    text
  else
    String.sub text 0 (String.length text - count)

let is_gfm = fun flavor -> flavor = Gfm

let rec parse_inline = fun ~flavor ~references text ->
  try
    let len = String.length text in
    let rec loop index acc =
      if index >= len then
        List.rev acc
      else if text.[index] = '\\' then
        if index + 1 < len then
          if text.[index + 1] = '\n' then
            loop (index + 2) (Hard_break :: acc)
          else if is_escapable text.[index + 1] then
            loop (index + 2) (Text (String.make 1 text.[index + 1]) :: acc)
          else
            loop (index + 1) (Text "\\" :: acc)
        else
          loop (index + 1) (Text "\\" :: acc)
      else if text.[index] = '\n' then
        (
          match acc with
          | Text head :: tail when trailing_space_count head >= 2 ->
              let trimmed = trim_trailing_spaces head in
              let acc =
                if trimmed = "" then
                  tail
                else
                  Text trimmed :: tail
              in
              let rec skip_spaces current =
                if current < len && (text.[current] = ' ' || text.[current] = '\t') then
                  skip_spaces (current + 1)
                else
                  current
              in
              loop (skip_spaces (index + 1)) (Hard_break :: acc)
          | _ ->
              loop (index + 1) (Text "\n" :: acc)
        )
      else if starts_with ~prefix:"![" text index then
        (
          match find_substring text (index + 2) "]" with
          | None -> loop (index + 1) (Text "!" :: acc)
          | Some close_text ->
              let alt_text = String.sub text (index + 2) (close_text - index - 2) in
              if close_text + 1 < len && text.[close_text + 1] = '(' then
                (
                  match find_substring text (close_text + 2) ")" with
                  | None -> loop (index + 1) (Text "!" :: acc)
                  | Some close_link ->
                      let raw_target =
                        String.sub text (close_text + 2) (close_link - close_text - 2)
                      in
                      let destination, title = parse_link_target raw_target in
                      loop
                        (close_link + 1)
                        (Image {
                           alt = parse_inline ~flavor ~references alt_text;
                           destination;
                           title;
                         }
                         :: acc)
                )
              else
                let shortcut =
                  if close_text + 1 < len && text.[close_text + 1] = '[' then
                    match find_substring text (close_text + 2) "]" with
                    | Some close_ref ->
                        let reference_label =
                          let explicit =
                            String.sub text (close_text + 2) (close_ref - close_text - 2)
                          in
                          if explicit = "" then alt_text else explicit
                        in
                        Some (reference_label, close_ref + 1)
                    | None -> None
                  else
                    Some (alt_text, close_text + 1)
                in
                (
                  match shortcut with
                  | Some (reference_label, next_index) -> (
                      match find_reference references reference_label with
                      | Some reference ->
                          loop
                            next_index
                            (Image {
                               alt = parse_inline ~flavor ~references alt_text;
                               destination = reference.destination;
                               title = reference.title;
                             }
                             :: acc)
                      | None ->
                          loop
                            next_index
                            (Text (String.sub text index (next_index - index) |> decode_entities) :: acc)
                    )
                  | None ->
                      loop (index + 1) (Text "!" :: acc)
                )
        )
      else if starts_with ~prefix:"**" text index then
        (
          match find_substring text (index + 2) "**" with
          | None -> loop (index + 2) (Text "**" :: acc)
          | Some close ->
              let body = String.sub text (index + 2) (close - index - 2) in
              loop (close + 2) (Strong (parse_inline ~flavor ~references body) :: acc)
        )
      else if starts_with ~prefix:"~~" text index then
        (
          match find_substring text (index + 2) "~~" with
          | None -> loop (index + 2) (Text "~~" :: acc)
          | Some close ->
              let body = String.sub text (index + 2) (close - index - 2) in
              if is_gfm flavor then
                loop (close + 2) (Strikethrough (parse_inline ~flavor ~references body) :: acc)
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
                loop (close + 1) (Emphasis (parse_inline ~flavor ~references body) :: acc)
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
                  |> normalize_code_span
              in
              loop (close + marker_len) (Code_span body :: acc)
        )
      else if text.[index] = '[' then
        (
          match find_substring text (index + 1) "]" with
          | None -> loop (index + 1) (Text "[" :: acc)
          | Some close_text ->
              let label_text = String.sub text (index + 1) (close_text - index - 1) in
              if close_text + 1 < len && text.[close_text + 1] = '(' then
                (
                  match find_substring text (close_text + 2) ")" with
                  | None ->
                      loop
                        (close_text + 1)
                        (Text
                           (String.sub text index (close_text - index + 1) |> decode_entities)
                         :: acc)
                  | Some close_link ->
                      let raw_target =
                        String.sub text (close_text + 2) (close_link - close_text - 2)
                      in
                      let destination, title = parse_link_target raw_target in
                      loop
                        (close_link + 1)
                        (Link {
                           label = parse_inline ~flavor ~references label_text;
                           destination;
                           title;
                         }
                         :: acc)
                )
              else
                let shortcut =
                  if close_text + 1 < len && text.[close_text + 1] = '[' then
                    match find_substring text (close_text + 2) "]" with
                    | Some close_ref ->
                        let explicit =
                          String.sub text (close_text + 2) (close_ref - close_text - 2)
                        in
                        let reference_label = if explicit = "" then label_text else explicit in
                        Some (reference_label, close_ref + 1)
                    | None -> None
                  else
                    Some (label_text, close_text + 1)
                in
                (
                  match shortcut with
                  | Some (reference_label, next_index) -> (
                      match find_reference references reference_label with
                      | Some reference ->
                          loop
                            next_index
                            (Link {
                               label = parse_inline ~flavor ~references label_text;
                               destination = reference.destination;
                               title = reference.title;
                             }
                             :: acc)
                      | None ->
                          loop
                            next_index
                            (Text (String.sub text index (next_index - index) |> decode_entities) :: acc)
                    )
                  | None ->
                      loop
                        (close_text + 1)
                        (Text
                           (String.sub text index (close_text - index + 1) |> decode_entities)
                         :: acc)
                )
        )
      else if text.[index] = '<' then
        (
          match find_substring text (index + 1) ">" with
          | None -> loop (index + 1) (Text "<" :: acc)
          | Some close ->
              let inside = String.sub text (index + 1) (close - index - 1) in
              (
                match autolink_destination inside with
                | Some destination ->
                    loop
                      (close + 1)
                      (Link {
                         label = [ Text inside ];
                         destination;
                         title = None;
                       }
                       :: acc)
                | None ->
                    if looks_like_raw_html inside then
                      loop (close + 1) (Raw_html (String.sub text index (close - index + 1)) :: acc)
                    else
                      loop
                        (close + 1)
                        (Text (String.sub text index (close - index + 1) |> decode_entities) :: acc)
              )
        )
      else if text.[index] = '&' then
        (
          match decode_entity_at text index with
          | Some (decoded, next) -> loop next (Text decoded :: acc)
          | None -> loop (index + 1) (Text "&" :: acc)
        )
      else
        let rec scan current =
          if current >= len then
            current
          else
            match text.[current] with
            | '\\' | '*' | '~' | '`' | '[' | '<' | '&' | '\n' -> current
            | '!' when current + 1 < len && text.[current + 1] = '[' -> current
            | _ -> scan (current + 1)
        in
        let next = scan (index + 1) in
        loop next (Text (String.sub text index (next - index) |> decode_entities) :: acc)
    in
    let parsed = loop 0 [] in
    let parsed =
      match List.rev parsed with
      | Text last :: tail_rev ->
          let trimmed = trim_trailing_spaces last in
          List.rev (if trimmed = "" then tail_rev else Text trimmed :: tail_rev)
          | _ ->
              parsed
    in
    if parsed = [] then [ Text text ] else parsed
  with
  | _ ->
      [ Text text ]

let direct_token_texts = fun node ->
  Ceibo.Red.SyntaxNode.direct_tokens node |> List.map Ceibo.Red.SyntaxToken.text

let first_token_text = fun node ->
  match direct_token_texts node with
  | [] -> ""
  | head :: _ -> head

let child_nodes = fun node -> Ceibo.Red.SyntaxNode.direct_nodes node

let heading_level_of_kind = function
  | Markdown_syntax_kind.Heading_1 -> 1
  | Markdown_syntax_kind.Heading_2 -> 2
  | Markdown_syntax_kind.Heading_3 -> 3
  | Markdown_syntax_kind.Heading_4 -> 4
  | Markdown_syntax_kind.Heading_5 -> 5
  | Markdown_syntax_kind.Heading_6 -> 6
  | _ -> 1

let alignment_of_kind = function
  | Markdown_syntax_kind.Table_cell_left -> Left
  | Markdown_syntax_kind.Table_cell_center -> Center
  | Markdown_syntax_kind.Table_cell_right -> Right
  | _ -> Default

let lower_table_row = fun ~flavor ~references row_node ->
  let cells =
    child_nodes row_node
    |> List.map (fun cell_node -> parse_inline ~flavor ~references (first_token_text cell_node))
  in
  let alignments = child_nodes row_node |> List.map (fun cell_node -> alignment_of_kind (Ceibo.Red.SyntaxNode.kind cell_node)) in
  { cells; alignments }

let rec collect_references = fun references nodes ->
  List.fold_left
    (fun references node ->
      let references =
        match Ceibo.Red.SyntaxNode.kind node with
        | Markdown_syntax_kind.Paragraph -> (
            match parse_reference_definition (first_token_text node) with
            | Some (label, reference) when not (List.mem_assoc label references) ->
                (label, reference) :: references
            | _ ->
                references
          )
        | _ ->
            references
      in
      collect_references references (child_nodes node))
    references
    nodes

let is_reference_definition_node = fun node ->
  Ceibo.Red.SyntaxNode.kind node = Markdown_syntax_kind.Paragraph
  && Option.is_some (parse_reference_definition (first_token_text node))

let rec lower_list_item = fun ~flavor ~references node ->
  let span = Ceibo.Red.SyntaxNode.span node in
  let blocks =
    child_nodes node
    |> List.filter_map (lower_block_opt ~flavor ~references)
  in
  match Ceibo.Red.SyntaxNode.kind node with
  | Markdown_syntax_kind.Task_list_item_checked -> [ Task_list_item { checked = true; blocks; span } ]
  | Markdown_syntax_kind.Task_list_item_unchecked -> [ Task_list_item { checked = false; blocks; span } ]
  | _ -> [ List_item { blocks; span } ]

and lower_block_opt = fun ~flavor ~references node ->
  if is_reference_definition_node node then
    None
  else
    Some (lower_block ~flavor ~references node)

and lower_block = fun ~flavor ~references node ->
  let span = Ceibo.Red.SyntaxNode.span node in
  match Ceibo.Red.SyntaxNode.kind node with
  | Markdown_syntax_kind.Heading_1
  | Markdown_syntax_kind.Heading_2
  | Markdown_syntax_kind.Heading_3
  | Markdown_syntax_kind.Heading_4
  | Markdown_syntax_kind.Heading_5
  | Markdown_syntax_kind.Heading_6 ->
      Heading {
        level = heading_level_of_kind (Ceibo.Red.SyntaxNode.kind node);
        inlines = parse_inline ~flavor ~references (first_token_text node);
        span;
      }
  | Markdown_syntax_kind.Paragraph ->
      Paragraph { inlines = parse_inline ~flavor ~references (first_token_text node); span }
  | Markdown_syntax_kind.Block_quote ->
      Block_quote {
        blocks = child_nodes node |> List.filter_map (lower_block_opt ~flavor ~references);
        span;
      }
  | Markdown_syntax_kind.Ordered_list_tight ->
      List {
        ordered = true;
        tight = true;
        items = child_nodes node |> List.map (lower_list_item ~flavor ~references);
        span;
      }
  | Markdown_syntax_kind.Ordered_list_loose ->
      List {
        ordered = true;
        tight = false;
        items = child_nodes node |> List.map (lower_list_item ~flavor ~references);
        span;
      }
  | Markdown_syntax_kind.Unordered_list_tight ->
      List {
        ordered = false;
        tight = true;
        items = child_nodes node |> List.map (lower_list_item ~flavor ~references);
        span;
      }
  | Markdown_syntax_kind.Unordered_list_loose ->
      List {
        ordered = false;
        tight = false;
        items = child_nodes node |> List.map (lower_list_item ~flavor ~references);
        span;
      }
  | Markdown_syntax_kind.Task_list_item_checked
  | Markdown_syntax_kind.Task_list_item_unchecked
  | Markdown_syntax_kind.List_item ->
      List_item {
        blocks = child_nodes node |> List.filter_map (lower_block_opt ~flavor ~references);
        span;
      }
  | Markdown_syntax_kind.Fenced_code_block ->
      let tokens = Ceibo.Red.SyntaxNode.direct_tokens node in
      let info =
        List.find_map
          (fun token ->
            if Ceibo.Red.SyntaxToken.kind token = Markdown_syntax_kind.Info_string then
              Some (Ceibo.Red.SyntaxToken.text token)
            else
              None)
          tokens
        |> Option.unwrap_or ~default:""
        |> unescape_backslashes
        |> decode_entities
      in
      let code =
        List.find_map
          (fun token ->
            if Ceibo.Red.SyntaxToken.kind token = Markdown_syntax_kind.Text then
              Some (Ceibo.Red.SyntaxToken.text token)
            else
              None)
          tokens
        |> Option.unwrap_or ~default:""
      in
      Code_block { info; code; span; fenced = true }
  | Markdown_syntax_kind.Indented_code_block ->
      Code_block { info = ""; code = first_token_text node; span; fenced = false }
  | Markdown_syntax_kind.Horizontal_rule ->
      Horizontal_rule span
  | Markdown_syntax_kind.Raw_html_block ->
      Raw_html { html = first_token_text node; span }
  | Markdown_syntax_kind.Table ->
      let children = child_nodes node in
      let header_node = List.hd children in
      let row_nodes = List.tl children in
      Table {
        header = lower_table_row ~flavor ~references header_node;
        rows = List.map (lower_table_row ~flavor ~references) row_nodes;
        span;
      }
  | Markdown_syntax_kind.Error ->
      Error_block { message = first_token_text node; span }
  | _ ->
      Error_block { message = Markdown_syntax_kind.to_string (Ceibo.Red.SyntaxNode.kind node); span }

let lower = fun ~flavor tree ->
  let root = Ceibo.Red.new_root tree in
  let children = child_nodes root in
  let references = collect_references [] children in
  children |> List.filter_map (lower_block_opt ~flavor ~references)
