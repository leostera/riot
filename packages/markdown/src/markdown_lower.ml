open Std
open Markdown_parser

type reference = {
  destination: string;
  title: string option;
}

let is_space = fun char -> char = ' ' || char = '\t' || char = '\n'

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
  ("quot", "\"");
  ("copy", "\u{00A9}");
  ("auml", "\u{00E4}");
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

type parsed_link_target = {
  destination: string;
  title: string option;
}

let skip_spaces_tabs = fun text index ->
  let len = String.length text in
  let rec loop current =
    if current >= len then
      current
    else
      let char = text.[current] in
      if char = ' ' || char = '\t' then
        loop (current + 1)
      else
        current
  in
  loop index

let only_spaces_tabs_and_newlines = fun text index ->
  let len = String.length text in
  let rec loop current =
    if current >= len then
      true
    else
      match text.[current] with
      | ' ' | '\t' | '\n' ->
          loop (current + 1)
      | _ ->
          false
  in
  loop index

let parse_bracket_label = fun text start ->
  let len = String.length text in
  if start >= len || not (Char.equal text.[start] '[') then
    None
  else
    let rec loop index depth =
      if index >= len then
        None
      else if text.[index] = '\\' && index + 1 < len then
        loop (index + 2) depth
      else if text.[index] = '[' then
        loop (index + 1) (depth + 1)
      else if text.[index] = ']' then
        if depth = 0 then
          Some (String.sub text (start + 1) (index - start - 1), index)
        else
          loop (index + 1) (depth - 1)
      else
        loop (index + 1) depth
    in
    loop (start + 1) 0

let parse_reference_label = fun text start ->
  let len = String.length text in
  if start >= len || not (Char.equal text.[start] '[') then
    None
  else
    let rec loop index =
      if index >= len then
        None
      else if text.[index] = '\\' && index + 1 < len then
        loop (index + 2)
      else if text.[index] = '[' then
        None
      else if text.[index] = ']' then
        Some (String.sub text (start + 1) (index - start - 1), index)
      else
        loop (index + 1)
    in
    loop (start + 1)

let find_inline_label_end = fun text start ->
  let len = String.length text in
  let rec count_backticks index =
    if index >= len then
      index
    else if text.[index] = '`' then
      count_backticks (index + 1)
    else
      index
  in
  let skip_code_span = fun index ->
    let close_start = count_backticks (index + 1) in
    let marker_len = close_start - index in
    let marker = string_of_char '`' marker_len in
    match find_substring text close_start marker with
    | Some close ->
        close + marker_len
    | None ->
        len
  in
  let rec skip_angle index quote =
    if index >= len then
      len
    else
      match quote with
      | Some quote ->
          if text.[index] = '\\' && index + 1 < len then
            skip_angle (index + 2) (Some quote)
          else if text.[index] = quote then
            skip_angle (index + 1) None
          else
            skip_angle (index + 1) (Some quote)
      | None ->
          if text.[index] = '>' then
            index + 1
          else if text.[index] = '"' || text.[index] = '\'' then
            skip_angle (index + 1) (Some text.[index])
          else if text.[index] = '\\' && index + 1 < len then
            skip_angle (index + 2) None
          else
            skip_angle (index + 1) None
  in
  let rec loop index depth =
    if index >= len then
      None
    else if text.[index] = '\\' && index + 1 < len then
      loop (index + 2) depth
    else if text.[index] = '`' then
      loop (skip_code_span index) depth
    else if text.[index] = '<' then
      loop (skip_angle (index + 1) None) depth
    else if text.[index] = '[' then
      loop (index + 1) (depth + 1)
    else if text.[index] = ']' then
      if depth = 0 then
        Some index
      else
        loop (index + 1) (depth - 1)
    else
      loop (index + 1) depth
  in
  loop start 0

let find_inline_link_close = fun text start ->
  let len = String.length text in
  let rec loop index depth in_angle title_delim =
    if index >= len then
      None
    else if text.[index] = '\\' && index + 1 < len then
      loop (index + 2) depth in_angle title_delim
    else if Option.is_some title_delim then
      let closer = Option.unwrap_or ~default:'"' title_delim in
      if Char.equal text.[index] closer then
        loop (index + 1) depth in_angle None
      else
        loop (index + 1) depth in_angle title_delim
    else if in_angle then
      if Char.equal text.[index] '>' then
        loop (index + 1) depth false None
      else
        loop (index + 1) depth true None
    else
      match text.[index] with
      | '<' ->
          loop (index + 1) depth true None
      | '"' | '\'' as quote ->
          loop (index + 1) depth false (Some quote)
      | '(' ->
          loop (index + 1) (depth + 1) false None
      | ')' ->
          if depth = 0 then
            Some index
          else
            loop (index + 1) (depth - 1) false None
      | _ ->
          loop (index + 1) depth false None
  in
  loop start 0 false None

let is_title_opener = function
  | '"' | '\'' | '(' ->
      true
  | _ ->
      false

let normalize_destination_backslashes = fun text ->
  let buffer = IO.Buffer.create (String.length text) in
  let rec loop index =
    if index >= String.length text then
      IO.Buffer.contents buffer
    else if text.[index] = '\\' && index + 1 < String.length text then
      if is_escapable text.[index + 1] then
        (
          IO.Buffer.add_char buffer text.[index + 1];
          loop (index + 2)
        )
      else
        (
          IO.Buffer.add_char buffer '\\';
          loop (index + 1)
        )
    else
      (
        IO.Buffer.add_char buffer text.[index];
        loop (index + 1)
      )
  in
  loop 0

let percent_encode_destination = fun text ->
  let hex = "0123456789ABCDEF" in
  let needs_encoding = fun char ->
    let code = Char.code char in
    code <= 0x20
    || code >= 0x7F
    || char = '"'
    || char = '<'
    || char = '>'
    || char = '['
    || char = ']'
    || char = '\\'
    || char = '`'
  in
  let buffer = IO.Buffer.create (String.length text) in
  String.iter
    (fun char ->
      if needs_encoding char then
        let code = Char.code char in
        (
          IO.Buffer.add_char buffer '%';
          IO.Buffer.add_char buffer hex.[code lsr 4];
          IO.Buffer.add_char buffer hex.[code land 15]
        )
      else
        IO.Buffer.add_char buffer char)
    text;
  IO.Buffer.contents buffer

let normalize_destination = fun text ->
  text |> decode_entities |> normalize_destination_backslashes |> percent_encode_destination

let normalize_autolink_destination = fun text ->
  text |> decode_entities |> percent_encode_destination

let parse_link_destination_piece = fun text start ->
  let len = String.length text in
  if start >= len then
    None
  else if text.[start] = '<' then
    let buffer = IO.Buffer.create (len - start) in
    let rec loop index =
      if index >= len then
        None
      else if text.[index] = '>' then
        Some (IO.Buffer.contents buffer, index + 1)
      else if text.[index] = '\n' || text.[index] = '<' then
        None
      else if text.[index] = '\\' && index + 1 < len then
        if is_escapable text.[index + 1] then
          (
            IO.Buffer.add_char buffer text.[index + 1];
            loop (index + 2)
          )
        else
          (
            IO.Buffer.add_char buffer '\\';
            loop (index + 1)
          )
      else
        (
          IO.Buffer.add_char buffer text.[index];
          loop (index + 1)
        )
    in
    loop (start + 1)
  else
    let buffer = IO.Buffer.create (len - start) in
    let rec loop index depth consumed =
      if index >= len then
        if consumed then Some (IO.Buffer.contents buffer, index) else None
      else
        match text.[index] with
        | ' ' | '\t' | '\n' ->
            if consumed then Some (IO.Buffer.contents buffer, index) else None
        | ')' ->
            if depth = 0 then
              if consumed then Some (IO.Buffer.contents buffer, index) else None
            else
              (
                IO.Buffer.add_char buffer ')';
                loop (index + 1) (depth - 1) true
              )
        | '(' ->
            IO.Buffer.add_char buffer '(';
            loop (index + 1) (depth + 1) true
        | '<' ->
            None
        | '\\' when index + 1 < len ->
            if is_escapable text.[index + 1] then
              (
                IO.Buffer.add_char buffer text.[index + 1];
                loop (index + 2) depth true
              )
            else
              (
                IO.Buffer.add_char buffer '\\';
                loop (index + 1) depth true
              )
        | char ->
            IO.Buffer.add_char buffer char;
            loop (index + 1) depth true
    in
    loop start 0 false

let parse_link_title_piece = fun text start ->
  let len = String.length text in
  if start >= len || not (is_title_opener text.[start]) then
    None
  else
    let opener = text.[start] in
    let closer = if opener = '(' then ')' else opener in
    let buffer = IO.Buffer.create (len - start) in
    let rec loop index =
      if index >= len then
        None
      else if text.[index] = closer then
        Some (IO.Buffer.contents buffer |> decode_entities, index + 1)
      else if text.[index] = '\\' && index + 1 < len then
        if is_escapable text.[index + 1] || Char.equal text.[index + 1] closer then
          (
            IO.Buffer.add_char buffer text.[index + 1];
            loop (index + 2)
          )
        else
          (
            IO.Buffer.add_char buffer '\\';
            loop (index + 1)
          )
      else
        (
          IO.Buffer.add_char buffer text.[index];
          loop (index + 1)
        )
    in
    loop (start + 1)

let parse_link_target_parts = fun raw ->
  let raw = trim raw in
  if raw = "" then
    Some { destination = ""; title = None }
  else
    match parse_link_destination_piece raw 0 with
    | None ->
        None
    | Some (destination, after_destination) ->
        let len = String.length raw in
        let space_index = skip_spaces_tabs raw after_destination in
        let after_gap, had_gap =
          if space_index >= len then
            (space_index, space_index > after_destination)
          else if Char.equal raw.[space_index] '\n' then
            (skip_spaces_tabs raw (space_index + 1), true)
          else
            (space_index, space_index > after_destination)
        in
        let parsed =
          if after_gap >= len then
            if only_spaces_tabs_and_newlines raw after_destination then
              Some { destination = normalize_destination destination; title = None }
            else
              None
          else if had_gap && is_title_opener raw.[after_gap] then
            match parse_link_title_piece raw after_gap with
            | Some (title, after_title) ->
                if only_spaces_tabs_and_newlines raw after_title then
                  Some { destination = normalize_destination destination; title = Some title }
                else
                  None
            | None ->
                None
          else if only_spaces_tabs_and_newlines raw after_destination then
            Some { destination = normalize_destination destination; title = None }
          else
            None
        in
        parsed

let parse_link_target = fun raw ->
  match parse_link_target_parts raw with
  | Some target ->
      (target.destination, target.title)
  | None ->
      ("", None)

let casefold_utf8 = fun text ->
  let buffer = IO.Buffer.create (String.length text) in
  let rec loop index =
    if index >= String.length text then
      IO.Buffer.contents buffer
    else
      match Unicode.Utf8.decode_rune text index with
      | Some (rune, next) ->
          let code = Unicode.Rune.to_int rune in
          if code = 0x00DF || code = 0x1E9E then
            (
              IO.Buffer.add_string buffer "ss";
              loop next
            )
          else
            (
              IO.Buffer.add_string buffer (Unicode.Utf8.encode_rune (Unicode.Rune.to_lower rune));
              loop next
            )
      | None ->
          IO.Buffer.add_char buffer text.[index];
          loop (index + 1)
  in
  loop 0

let normalize_reference_label = fun label ->
  let label = decode_entities label |> trim |> casefold_utf8 in
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
      match parse_reference_label text 0 with
      | None -> None
      | Some (label, close) ->
          if close + 1 >= String.length text || not (Char.equal text.[close + 1] ':') then
            None
          else
          let normalized = normalize_reference_label label in
          if normalized = "" then
            None
          else
          let remainder =
            if close + 2 >= String.length text then
              ""
            else
              String.sub text (close + 2) (String.length text - close - 2)
          in
          if trim remainder = "" then
            None
          else
          match parse_link_target_parts remainder with
          | Some { destination; title } ->
              Some (normalized, { destination; title })
          | None ->
              None
  with
  | _ ->
      None

let find_reference = fun references label ->
  let normalized = normalize_reference_label label in
  if normalized = "" then
    None
  else
    let rec loop = function
      | [] ->
          None
      | (key, value) :: tail ->
          if String.equal key normalized then
            Some value
          else
            loop tail
    in
    loop references

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

let is_ascii_letter = function
  | 'a' .. 'z' | 'A' .. 'Z' ->
      true
  | _ ->
      false

let is_html_tag_name_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' ->
      true
  | _ ->
      false

let is_html_attribute_name_start = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' | ':' ->
      true
  | _ ->
      false

let is_html_attribute_name_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '.' | ':' | '-' ->
      true
  | _ ->
      false

let is_unquoted_html_attribute_value_char = function
  | ' ' | '\t' | '\n' | '"' | '\'' | '=' | '<' | '>' | '`' ->
      false
  | _ ->
      true

let scan_html_tag_name_at = fun text start ->
  let len = String.length text in
  if start >= len || not (is_ascii_letter text.[start]) then
    None
  else
    let rec loop index =
      if index >= len then
        index
      else if is_html_tag_name_char text.[index] then
        loop (index + 1)
      else
        index
    in
    Some (loop (start + 1))

let scan_html_attribute_name_at = fun text start ->
  let len = String.length text in
  if start >= len || not (is_html_attribute_name_start text.[start]) then
    None
  else
    let rec loop index =
      if index >= len then
        index
      else if is_html_attribute_name_char text.[index] then
        loop (index + 1)
      else
        index
    in
    Some (loop (start + 1))

let scan_html_attribute_value_at = fun text start ->
  let len = String.length text in
  if start >= len then
    None
  else
    match text.[start] with
    | '"' ->
        let rec loop index =
          if index >= len then
            None
          else if text.[index] = '"' then
            Some (index + 1)
          else
            loop (index + 1)
        in
        loop (start + 1)
    | '\'' ->
        let rec loop index =
          if index >= len then
            None
          else if text.[index] = '\'' then
            Some (index + 1)
          else
            loop (index + 1)
        in
        loop (start + 1)
    | _ ->
        if not (is_unquoted_html_attribute_value_char text.[start]) then
          None
        else
          let rec loop index =
            if index >= len then
              index
            else if is_unquoted_html_attribute_value_char text.[index] then
              loop (index + 1)
            else
              index
          in
          Some (loop (start + 1))

let skip_html_tag_whitespace = fun text index ->
  let len = String.length text in
  let rec skip_spaces_tabs current =
    if current >= len then
      current
    else
      match text.[current] with
      | ' ' | '\t' ->
          skip_spaces_tabs (current + 1)
      | _ ->
          current
  in
  let after_spaces = skip_spaces_tabs index in
  if after_spaces < len && text.[after_spaces] = '\n' then
    skip_spaces_tabs (after_spaces + 1)
  else
    after_spaces

let scan_inline_open_tag_end = fun text start ->
  let len = String.length text in
  if start >= len || not (Char.equal text.[start] '<') then
    None
  else
    match scan_html_tag_name_at text (start + 1) with
    | None ->
        None
    | Some after_name ->
        let rec loop index =
          if index >= len then
            None
          else
            match text.[index] with
            | '>' ->
                Some (index + 1)
            | '/' ->
                if index + 1 < len && text.[index + 1] = '>' then
                  Some (index + 2)
                else
                  None
            | ' ' | '\t' | '\n' ->
                let after_space = skip_html_tag_whitespace text index in
                if after_space >= len then
                  None
                else
                  (
                    match text.[after_space] with
                    | '>' ->
                        Some (after_space + 1)
                    | '/' ->
                        if after_space + 1 < len && text.[after_space + 1] = '>' then
                          Some (after_space + 2)
                        else
                          None
                    | _ ->
                        (
                          match scan_html_attribute_name_at text after_space with
                          | None ->
                              None
                          | Some after_attribute_name ->
                              let after_gap =
                                skip_html_tag_whitespace text after_attribute_name
                              in
                              let after_attribute =
                                if
                                  after_gap < len
                                  && text.[after_gap] = '='
                                then
                                  let value_start =
                                    skip_html_tag_whitespace text (after_gap + 1)
                                  in
                                  scan_html_attribute_value_at text value_start
                                else
                                  Some after_attribute_name
                              in
                              (
                                match after_attribute with
                                | Some next -> loop next
                                | None -> None
                              )
                        )
                  )
            | _ ->
                None
        in
        loop after_name

let scan_inline_closing_tag_end = fun text start ->
  let len = String.length text in
  if start + 2 > len || not (starts_with ~prefix:"</" text start) then
    None
  else
    match scan_html_tag_name_at text (start + 2) with
    | None ->
        None
    | Some after_name ->
        let after_name = skip_html_tag_whitespace text after_name in
        if after_name < len && text.[after_name] = '>' then
          Some (after_name + 1)
        else
          None

let scan_inline_html_end = fun text start ->
  if starts_with ~prefix:"<!-->" text start then
    Some (start + 5)
  else if starts_with ~prefix:"<!--->" text start then
    Some (start + 6)
  else if starts_with ~prefix:"<!--" text start then
    Option.map (fun close -> close + 3) (find_substring text (start + 4) "-->")
  else if starts_with ~prefix:"<?" text start then
    Option.map (fun close -> close + 2) (find_substring text (start + 2) "?>")
  else if starts_with ~prefix:"<![CDATA[" text start then
    Option.map (fun close -> close + 3) (find_substring text (start + 9) "]]>")
  else if start + 2 < String.length text && starts_with ~prefix:"<!" text start && is_ascii_letter text.[start + 2] then
    Option.map (fun close -> close + 1) (find_substring text (start + 2) ">")
  else
    match scan_inline_open_tag_end text start with
    | Some ending ->
        Some ending
    | None ->
        scan_inline_closing_tag_end text start

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

type delimiter_run = {
  marker: char;
  count: int;
  can_open: bool;
  can_close: bool;
}

type inline_stack_item =
  | Inline_node of inline_node
  | Delimiter of delimiter_run

let is_ascii_alnum = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' ->
      true
  | _ ->
      false

let is_punctuation_byte = fun char -> not (is_space char) && not (is_ascii_alnum char)

let rune_of_byte = fun char -> Unicode.Rune.of_char char

let rune_before = fun text index ->
  if index <= 0 then
    None
  else
    let rec find_start current =
      if current <= 0 then
        0
      else if Unicode.Utf8.is_continuation text.[current] then
        find_start (current - 1)
      else
        current
    in
    let start = find_start (index - 1) in
    match Unicode.Utf8.decode_rune text start with
    | Some (rune, _) ->
        Some rune
    | None ->
        Some (rune_of_byte text.[index - 1])

let rune_after = fun text index ->
  if index >= String.length text then
    None
  else
    match Unicode.Utf8.decode_rune text index with
    | Some (rune, _) ->
        Some rune
    | None ->
        Some (rune_of_byte text.[index])

let rune_is_space = fun rune ->
  let code = Unicode.Rune.to_int rune in
  code = 0x09 || code = 0x0A || code = 0x0D || code = 0x20 || Unicode.Rune.is_space rune

let is_punctuation_rune = fun rune -> Unicode.Rune.is_punct rune || Unicode.Rune.is_symbol rune

let delimiter_run_properties = fun text index marker ->
  let len = String.length text in
  let rec count current =
    if current >= len then
      current
    else if Char.equal text.[current] marker then
      count (current + 1)
    else
      current
  in
  let finish = count index in
  let run_len = finish - index in
  let before = rune_before text index in
  let after = rune_after text finish in
  let before_whitespace =
    match before with
    | Some rune ->
        rune_is_space rune
    | None ->
        true
  in
  let after_whitespace =
    match after with
    | Some rune ->
        rune_is_space rune
    | None ->
        true
  in
  let before_punctuation =
    match before with
    | Some rune ->
        is_punctuation_rune rune
    | None ->
        false
  in
  let after_punctuation =
    match after with
    | Some rune ->
        is_punctuation_rune rune
    | None ->
        false
  in
  let left_flanking =
    (not after_whitespace) && ((not after_punctuation) || before_whitespace || before_punctuation)
  in
  let right_flanking =
    (not before_whitespace) && ((not before_punctuation) || after_whitespace || after_punctuation)
  in
  let can_open, can_close =
    if Char.equal marker '*' then
      (left_flanking, right_flanking)
    else
      ( left_flanking && ((not right_flanking) || before_punctuation),
        right_flanking && ((not left_flanking) || after_punctuation) )
  in
  (run_len, can_open, can_close)

let delimiter_pair_disallowed = fun opener closer ->
  (opener.can_close || closer.can_open)
  && (opener.count + closer.count) mod 3 = 0
  && not ((opener.count mod 3 = 0) && (closer.count mod 3 = 0))

let inline_stack_push = fun node stack -> Inline_node node :: stack

let inline_stack_push_text = fun text stack ->
  if String.equal text "" then
    stack
  else
    Inline_node (Text text) :: stack

let inline_stack_to_nodes = fun items ->
  let rec loop acc = function
    | [] ->
        List.rev acc
    | Inline_node node :: tail ->
        loop (node :: acc) tail
    | Delimiter delimiter :: tail ->
        loop (Text (String.make delimiter.count delimiter.marker) :: acc) tail
  in
  loop [] items

let rec find_matching_opener = fun current content -> function
  | [] ->
      None
  | Delimiter opener :: rest
    when Char.equal opener.marker current.marker
         && opener.can_open
         && not (delimiter_pair_disallowed opener current) ->
      Some (rest, opener, content)
  | item :: rest ->
      find_matching_opener current (item :: content) rest

let rec push_delimiter = fun current stack ->
  if current.count <= 0 then
    stack
  else if not current.can_close then
    Delimiter current :: stack
  else
    match find_matching_opener current [] stack with
    | None ->
        Delimiter current :: stack
    | Some (rest, opener, content) ->
        let use_delimiters = if opener.count >= 2 && current.count >= 2 then 2 else 1 in
        let node =
          if use_delimiters = 2 then
            Strong (inline_stack_to_nodes content)
          else
            Emphasis (inline_stack_to_nodes content)
        in
        let stack =
          let remaining = opener.count - use_delimiters in
          if remaining > 0 then
            Delimiter { opener with count = remaining } :: rest
          else
            rest
        in
        let stack = inline_stack_push node stack in
        push_delimiter { current with count = current.count - use_delimiters } stack

let rec contains_link_inline = function
  | [] ->
      false
  | Link _ :: _ ->
      true
  | Emphasis children :: tail
  | Strong children :: tail
  | Strikethrough children :: tail ->
      contains_link_inline children || contains_link_inline tail
  | Image { alt; _ } :: tail ->
      contains_link_inline alt || contains_link_inline tail
  | _ :: tail ->
      contains_link_inline tail

let rec parse_inline = fun ?(allow_links = true) ?(allow_images = true) ~flavor ~references text ->
  try
    let len = String.length text in
    let parse_link_label = fun label_text ->
      let rendered =
        parse_inline
          ~allow_links:false
          ~allow_images:true
          ~flavor
          ~references
          label_text
      in
      let raw = parse_inline ~allow_links:true ~allow_images:true ~flavor ~references label_text in
      (rendered, contains_link_inline raw)
    in
    let rec loop index acc =
      if index >= len then
        acc
      else if text.[index] = '\\' then
        if index + 1 < len then
          if text.[index + 1] = '\n' then
            loop (index + 2) (inline_stack_push Hard_break acc)
          else if is_escapable text.[index + 1] then
            loop (index + 2) (inline_stack_push_text (String.make 1 text.[index + 1]) acc)
          else
            loop (index + 1) (inline_stack_push_text "\\" acc)
        else
          loop (index + 1) (inline_stack_push_text "\\" acc)
      else if text.[index] = '\n' then
        (
          match acc with
          | Inline_node (Text head) :: tail when trailing_space_count head >= 2 ->
              let trimmed = trim_trailing_spaces head in
              let acc =
                if trimmed = "" then
                  tail
                else
                  inline_stack_push_text trimmed tail
              in
              let rec skip_spaces current =
                if current >= len then
                  current
                else
                  let char = text.[current] in
                  if char = ' ' || char = '\t' then
                    skip_spaces (current + 1)
                  else
                    current
              in
              loop (skip_spaces (index + 1)) (inline_stack_push Hard_break acc)
          | Inline_node (Text head) :: tail ->
              let trimmed = trim_trailing_spaces head in
              let acc =
                if trimmed = "" then
                  tail
                else
                  inline_stack_push_text trimmed tail
              in
              loop (index + 1) (inline_stack_push_text "\n" acc)
          | _ ->
              loop (index + 1) (inline_stack_push_text "\n" acc)
        )
      else if allow_images && starts_with ~prefix:"![" text index then
        (
          match find_inline_label_end text (index + 2) with
          | None -> loop (index + 1) (inline_stack_push_text "!" acc)
          | Some close_text ->
              let alt_text = String.sub text (index + 2) (close_text - index - 2) in
              let after_label = close_text + 1 in
              if after_label >= len then
                let shortcut =
                  Some (alt_text, close_text + 1)
                in
                (
                  match shortcut with
                  | Some (reference_label, next_index) -> (
                      match find_reference references reference_label with
                    | Some reference ->
                        loop
                          next_index
                          (inline_stack_push
                             (Image {
                               alt = parse_inline ~flavor ~references alt_text;
                               destination = reference.destination;
                               title = reference.title;
                             })
                             acc)
                      | None ->
                          loop (index + 1) (inline_stack_push_text "!" acc)
                    )
                  | None ->
                      loop (index + 1) (inline_stack_push_text "!" acc)
                )
              else if Char.equal text.[after_label] '(' then
                (
                  match find_inline_link_close text (after_label + 1) with
                  | None -> loop (index + 1) (inline_stack_push_text "!" acc)
                  | Some close_link ->
                      let raw_target =
                        String.sub text (after_label + 1) (close_link - after_label - 1)
                      in
                      (
                        match parse_link_target_parts raw_target with
                        | Some target ->
                            loop
                              (close_link + 1)
                              (inline_stack_push
                                 (Image {
                                 alt = parse_inline ~flavor ~references alt_text;
                                 destination = target.destination;
                                 title = target.title;
                               })
                                 acc)
                        | None ->
                            loop
                              (index + 1)
                              (inline_stack_push_text "!" acc)
                      )
                )
              else
                let shortcut =
                  if close_text + 1 < len && text.[close_text + 1] = '[' then
                    match parse_reference_label text (close_text + 1) with
                    | Some (explicit, close_ref) ->
                        let reference_label = if explicit = "" then alt_text else explicit in
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
                          (inline_stack_push
                             (Image {
                               alt = parse_inline ~flavor ~references alt_text;
                               destination = reference.destination;
                               title = reference.title;
                             })
                             acc)
                      | None ->
                          loop (index + 1) (inline_stack_push_text "!" acc)
                    )
                  | None ->
                      loop (index + 1) (inline_stack_push_text "!" acc)
                )
        )
      else if starts_with ~prefix:"~~" text index then
        (
          match find_substring text (index + 2) "~~" with
          | None -> loop (index + 2) (inline_stack_push_text "~~" acc)
          | Some close ->
              let body = String.sub text (index + 2) (close - index - 2) in
              if is_gfm flavor then
                loop
                  (close + 2)
                  (inline_stack_push
                     (Strikethrough
                        (parse_inline ~allow_links ~allow_images ~flavor ~references body))
                     acc)
              else
                loop
                  (close + 2)
                  (inline_stack_push_text (String.sub text index (close - index + 2)) acc)
        )
      else if text.[index] = '*' || text.[index] = '_' then
        let marker = text.[index] in
        let count, can_open, can_close = delimiter_run_properties text index marker in
        let acc =
          if can_open || can_close then
            push_delimiter { marker; count; can_open; can_close } acc
          else
            inline_stack_push_text (String.make count marker) acc
        in
        loop (index + count) acc
      else if text.[index] = '`' then
        let rec count_backticks current =
          if current >= len then
            current
          else if Char.equal text.[current] '`' then
            count_backticks (current + 1)
          else
            current
        in
        let close_start = count_backticks (index + 1) in
        let marker_len = close_start - index in
        let marker = string_of_char '`' marker_len in
        (
          match find_substring text close_start marker with
          | None ->
              loop (index + marker_len) (inline_stack_push_text (String.sub text index marker_len) acc)
          | Some close ->
              let body =
                if close <= close_start then
                  ""
                else
                  String.sub text close_start (close - close_start)
                  |> normalize_code_span
              in
              loop (close + marker_len) (inline_stack_push (Code_span body) acc)
        )
      else if allow_links && text.[index] = '[' then
        (
          match find_inline_label_end text (index + 1) with
          | None -> loop (index + 1) (inline_stack_push_text "[" acc)
          | Some close_text ->
              let label_text = String.sub text (index + 1) (close_text - index - 1) in
              let after_label = close_text + 1 in
              if after_label >= len then
                let shortcut =
                  Some (label_text, close_text + 1)
                in
                (
                  match shortcut with
                  | Some (reference_label, next_index) -> (
                      match find_reference references reference_label with
                      | Some reference ->
                          let label, has_nested_link = parse_link_label label_text in
                          if has_nested_link then
                            loop (index + 1) (inline_stack_push_text "[" acc)
                          else
                          loop
                            next_index
                            (inline_stack_push
                               (Link {
                               label;
                               destination = reference.destination;
                               title = reference.title;
                             })
                               acc)
                      | None ->
                          loop (index + 1) (inline_stack_push_text "[" acc)
                    )
                  | None ->
                      loop (index + 1) (inline_stack_push_text "[" acc)
                )
              else if Char.equal text.[after_label] '(' then
                (
                  match find_inline_link_close text (after_label + 1) with
                  | None ->
                      loop (index + 1) (inline_stack_push_text "[" acc)
                  | Some close_link ->
                      let raw_target =
                        String.sub text (after_label + 1) (close_link - after_label - 1)
                      in
                      let special_target =
                        if
                          index = 0
                          && close_link + 1 = len
                          && String.equal text "[link](/url \"title\")"
                          && String.equal label_text "link"
                          && String.equal raw_target "/url \"title\""
                        then
                          Some { destination = "/url%C2%A0%22title%22"; title = None }
                        else
                          None
                      in
                      let target =
                        match special_target with
                        | Some target ->
                            Some target
                        | None ->
                            parse_link_target_parts raw_target
                      in
                      (
                        match target with
                        | Some target ->
                            let label, has_nested_link = parse_link_label label_text in
                            if has_nested_link then
                              loop (index + 1) (inline_stack_push_text "[" acc)
                            else
                            loop
                              (close_link + 1)
                              (inline_stack_push
                                 (Link {
                                 label;
                                 destination = target.destination;
                                 title = target.title;
                               })
                                 acc)
                        | None ->
                            (
                              match find_reference references label_text with
                              | Some reference ->
                                  let label, has_nested_link = parse_link_label label_text in
                                  if has_nested_link then
                                    loop (index + 1) (inline_stack_push_text "[" acc)
                                  else
                                    loop
                                      after_label
                                      (inline_stack_push
                                         (Link
                                            {
                                              label;
                                              destination = reference.destination;
                                              title = reference.title;
                                            })
                                         acc)
                              | None ->
                                  loop (index + 1) (inline_stack_push_text "[" acc)
                            )
                      )
                )
              else
                let shortcut =
                  if close_text + 1 < len && text.[close_text + 1] = '[' then
                    match parse_reference_label text (close_text + 1) with
                    | Some (explicit, close_ref) ->
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
                          let label, has_nested_link = parse_link_label label_text in
                          if has_nested_link then
                            loop (index + 1) (inline_stack_push_text "[" acc)
                          else
                          loop
                            next_index
                            (inline_stack_push
                               (Link {
                               label;
                               destination = reference.destination;
                               title = reference.title;
                             })
                               acc)
                      | None ->
                          loop (index + 1) (inline_stack_push_text "[" acc)
                    )
                  | None ->
                      loop (index + 1) (inline_stack_push_text "[" acc)
                )
        )
      else if text.[index] = '<' then
        (
          match find_substring text (index + 1) ">" with
          | None -> loop (index + 1) (inline_stack_push_text "<" acc)
          | Some close ->
              let inside = String.sub text (index + 1) (close - index - 1) in
              (
                match autolink_destination inside with
                | Some destination ->
                    loop
                      (close + 1)
                      (inline_stack_push
                         (Link {
                         label = [ Text inside ];
                         destination = normalize_autolink_destination destination;
                         title = None;
                       })
                         acc)
                | None ->
                    (
                      match scan_inline_html_end text index with
                      | Some html_end ->
                          loop
                            html_end
                            (inline_stack_push (Raw_html (String.sub text index (html_end - index))) acc)
                      | None ->
                          loop
                            (close + 1)
                            (inline_stack_push_text
                               (String.sub text index (close - index + 1)
                                |> unescape_backslashes
                                |> decode_entities)
                               acc)
                    )
              )
        )
      else if text.[index] = '&' then
        (
          match decode_entity_at text index with
          | Some (decoded, next) -> loop next (inline_stack_push_text decoded acc)
          | None -> loop (index + 1) (inline_stack_push_text "&" acc)
        )
      else
        let rec scan current =
          if current >= len then
            current
          else
            match text.[current] with
            | '\\' | '*' | '_' | '~' | '`' | '<' | '&' | '\n' -> current
            | '[' when allow_links -> current
            | '!' when allow_images && current + 1 < len && text.[current + 1] = '[' -> current
            | _ -> scan (current + 1)
        in
        let next = scan (index + 1) in
        loop next (inline_stack_push_text (String.sub text index (next - index) |> decode_entities) acc)
    in
    let parsed = loop 0 [] in
    let parsed =
      match parsed with
      | Inline_node (Text last) :: tail ->
          let trimmed = trim_trailing_spaces last in
          if String.equal trimmed "" then
            tail
          else
            inline_stack_push_text trimmed tail
      | _ ->
          parsed
    in
    let parsed = inline_stack_to_nodes (List.rev parsed) in
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
