open Std
open Std.Collections

(* ===== Rune Width Tests ===== *)

let test_rune_width_ascii = fun _ctx ->
  match Unicode.Utf8.decode_rune "A" 0 with
  | Some (r, _) ->
      if Unicode.Rune.width r = 1 then
        Ok ()
      else
        Error ("Expected width 1 for 'A', got " ^ Int.to_string (Unicode.Rune.width r))
  | None -> Error "Failed to decode 'A'"

let test_rune_width_cjk = fun _ctx ->
  match Unicode.Utf8.decode_rune "一" 0 with
  | Some (r, _) ->
      if Unicode.Rune.width r = 2 then
        Ok ()
      else
        Error ("Expected width 2 for '一', got " ^ Int.to_string (Unicode.Rune.width r))
  | None -> Error "Failed to decode '一'"

let test_rune_width_combining = fun _ctx ->
  (* Combining acute accent - extract just the combining mark from "é" *)
  match Unicode.Utf8.decode_rune "́" 0 with
  | Some (r, _) ->
      if Unicode.Rune.width r = 0 then
        Ok ()
      else
        Error ("Expected width 0 for combining acute, got " ^ Int.to_string (Unicode.Rune.width r))
  | None -> Error "Failed to decode combining accent"

let test_rune_width_emoji = fun _ctx ->
  match Unicode.Utf8.decode_rune "👍" 0 with
  | Some (r, _) ->
      if Unicode.Rune.width r = 2 then
        Ok ()
      else
        Error ("Expected width 2 for '👍', got " ^ Int.to_string (Unicode.Rune.width r))
  | None -> Error "Failed to decode '👍'"

let test_rune_width_zwj = fun _ctx ->
  (* Zero-width joiner - using the actual character *)
  match Unicode.Utf8.decode_rune "‍" 0 with
  | Some (r, _) ->
      if Unicode.Rune.width r = 0 then
        Ok ()
      else
        Error ("Expected width 0 for ZWJ, got " ^ Int.to_string (Unicode.Rune.width r))
  | None -> Error "Failed to decode ZWJ"

let test_rune_width_fullwidth = fun _ctx ->
  match Unicode.Utf8.decode_rune "Ａ" 0 with
  | Some (r, _) ->
      if Unicode.Rune.width r = 2 then
        Ok ()
      else
        Error ("Expected width 2 for 'Ａ', got " ^ Int.to_string (Unicode.Rune.width r))
  | None -> Error "Failed to decode 'Ａ'"

(* ===== String Width Tests ===== *)

let test_string_width_ascii = fun _ctx ->
  if String.width "Hello" = 5 then
    Ok ()
  else
    Error ("Expected width 5, got " ^ Int.to_string (String.width "Hello"))

let test_string_width_cjk = fun _ctx ->
  if String.width "你好" = 4 then
    Ok ()
  else
    Error ("Expected width 4, got " ^ Int.to_string (String.width "你好"))

let test_string_width_mixed = fun _ctx ->
  let width = String.width "Hi世界" in
  if width = 6 then
    Ok ()
    (* 2 + 4 *)
  else
    Error ("Expected width 6, got " ^ Int.to_string width)

let test_string_width_emoji = fun _ctx ->
  if String.width "👍" = 2 then
    Ok ()
  else
    Error ("Expected width 2, got " ^ Int.to_string (String.width "👍"))

let test_string_width_combining = fun _ctx ->
  (* "e" + combining acute = "é" *)
  if String.width "é" = 1 then
    Ok ()
  else
    Error ("Expected width 1, got " ^ Int.to_string (String.width "é"))

(* ===== Grapheme Count Tests ===== *)

let test_grapheme_count_ascii = fun _ctx ->
  if String.grapheme_count "Hello" = 5 then
    Ok ()
  else
    Error ("Expected 5 graphemes, got " ^ Int.to_string (String.grapheme_count "Hello"))

let test_grapheme_count_cjk = fun _ctx ->
  if String.grapheme_count "你好" = 2 then
    Ok ()
  else
    Error ("Expected 2 graphemes, got " ^ Int.to_string (String.grapheme_count "你好"))

let test_grapheme_count_emoji_with_modifier = fun _ctx ->
  (* Thumbs up with skin tone should be 1 grapheme *)
  if String.grapheme_count "👍🏻" = 1 then
    Ok ()
  else
    Error ("Expected 1 grapheme, got " ^ Int.to_string (String.grapheme_count "👍🏻"))

(* ===== Rune Count Tests ===== *)

let test_rune_count_ascii = fun _ctx ->
  if String.rune_count "Hello" = 5 then
    Ok ()
  else
    Error ("Expected 5 runes, got " ^ Int.to_string (String.rune_count "Hello"))

let test_rune_count_cjk = fun _ctx ->
  if String.rune_count "你好" = 2 then
    Ok ()
  else
    Error ("Expected 2 runes, got " ^ Int.to_string (String.rune_count "你好"))

let test_rune_count_emoji = fun _ctx ->
  if String.rune_count "👍" = 1 then
    Ok ()
  else
    Error ("Expected 1 rune, got " ^ Int.to_string (String.rune_count "👍"))

(* ===== UTF-8 Tests ===== *)

let test_utf8_valid = fun _ctx ->
  if Unicode.Utf8.is_valid "Hello 世界" then
    Ok ()
  else
    Error "Valid UTF-8 string should be valid"

let test_utf8_decode = fun _ctx ->
  match Unicode.Utf8.decode_rune "Hello" 0 with
  | Some (r, next_pos) ->
      let code = Unicode.Rune.to_int r in
      if code = 0x48 && next_pos = 1 then
        Ok ()
      else
        let code_hex =
          let hex_chars = "0123456789ABCDEF" in
          let rec to_hex n acc len =
            if len = 0 then
              acc
            else
              to_hex (n / 16) (String.make 1 (String.get hex_chars (n mod 16)) ^ acc) (len - 1)
          in
          "U+" ^ to_hex code "" 4
        in
        Error ("Expected U+0048 at pos 1, got " ^ code_hex ^ " at pos " ^ Int.to_string next_pos)
  | None -> Error "Failed to decode valid UTF-8"

let test_utf8_encode = fun _ctx ->
  match Unicode.Utf8.decode_rune "一" 0 with
  | Some (r, _) ->
      let encoded = Unicode.Utf8.encode_rune r in
      if encoded = "一" then
        Ok ()
      else
        Error ("Expected '一', got '" ^ encoded ^ "'")
  | None -> Error "Failed to decode '一'"

(* ===== Rune Conversion Tests ===== *)

let test_rune_of_int_valid = fun _ctx ->
  match Unicode.Rune.of_int 0x41 with
  | Some r ->
      let s = Unicode.Rune.to_string r in
      if s = "A" then
        Ok ()
      else
        Error ("Expected 'A', got '" ^ s ^ "'")
  | None -> Error "Valid code point should succeed"

let test_rune_of_int_invalid = fun _ctx ->
  match Unicode.Rune.of_int 0x11_0000 with
  | Some _ -> Error "Invalid code point should return None"
  | None -> Ok ()

let test_rune_to_int = fun _ctx ->
  match Unicode.Utf8.decode_rune "👍" 0 with
  | Some (r, _) ->
      if Unicode.Rune.to_int r = 0x1_f44d then
        Ok ()
      else
        let code = Unicode.Rune.to_int r in
        let code_hex =
          let hex_chars = "0123456789ABCDEF" in
          let rec to_hex n acc =
            if n = 0 then
              if acc = "" then
                "0"
              else
                acc
            else
              to_hex (n / 16) (String.make 1 (String.get hex_chars (n mod 16)) ^ acc)
          in
          to_hex code ""
        in
        Error ("Expected 0x1F44D for '👍', got 0x" ^ code_hex)
  | None -> Error "Failed to decode '👍'"

(* ===== Word Boundary Tests ===== *)

let test_word_boundaries_simple = fun _ctx ->
  let boundaries = String.word_boundaries "Hello world" in
  if List.length boundaries > 0 then
    Ok ()
  else
    Error "Should find word boundaries in 'Hello world'"

let test_word_split_simple = fun _ctx ->
  let words = String.split_words "Hello world" in
  if List.length words >= 2 then
    Ok ()
  else
    Error ("Expected at least 2 words, got " ^ Int.to_string (List.length words))

let test_word_split_contractions = fun _ctx ->
  let words = String.split_words "don't" in
  (* Should NOT split contractions *)
  if List.exists (fun w -> w = "don't") words then
    Ok ()
  else
    Error "Contractions should stay together"

let test_word_split_identifiers = fun _ctx ->
  let words = String.split_words "foo_bar" in
  (* Should NOT split snake_case identifiers *)
  if List.exists (fun w -> w = "foo_bar") words then
    Ok ()
  else
    Error "Identifiers with underscores should stay together"

let test_next_word_start = fun _ctx ->
  let text = "Hello world" in
  let next = Unicode.Segmentation.find_next_word_start text 0 in
  if next > 0 && next < String.length text then
    Ok ()
  else
    Error ("Expected word start between 0 and "
    ^ Int.to_string (String.length text)
    ^ ", got "
    ^ Int.to_string next)

let test_prev_word_start = fun _ctx ->
  let text = "Hello world" in
  let len = String.length text in
  let prev = Unicode.Segmentation.find_prev_word_start text len in
  if prev >= 0 && prev < len then
    Ok ()
  else
    Error ("Expected word start between 0 and " ^ Int.to_string len ^ ", got " ^ Int.to_string prev)

(* ===== Line Breaking Tests ===== *)

let test_line_breaks_newline = fun _ctx ->
  let breaks = Unicode.Segmentation.find_line_breaks "Hello\nWorld" in
  if List.length breaks > 0 then
    Ok ()
  else
    Error "Should find line break at newline"

let test_line_breaks_space = fun _ctx ->
  let breaks = Unicode.Segmentation.find_line_breaks "Hello world" in
  if List.length breaks > 0 then
    Ok ()
  else
    Error "Should find line break at space"

let test_wrap_lines_simple = fun _ctx ->
  let lines = Unicode.Segmentation.wrap_lines ~width:10 "Hello world" in
  if List.length lines >= 2 then
    Ok ()
  else
    Error ("Expected at least 2 lines when wrapping to width 10, got "
    ^ Int.to_string (List.length lines))

let test_wrap_lines_short = fun _ctx ->
  let lines = Unicode.Segmentation.wrap_lines ~width:100 "Hello" in
  if List.length lines = 1 then
    Ok ()
  else
    Error ("Expected 1 line for short text, got " ^ Int.to_string (List.length lines))

let test_wrap_lines_cjk = fun _ctx ->
  let text = "你好世界" in
  (* Width 8 *)
  let lines = Unicode.Segmentation.wrap_lines ~width:5 text in
  if List.length lines >= 2 then
    Ok ()
  else
    Error "Should wrap CJK text when width exceeded"

let test_wrap_lines_preserves_newlines = fun _ctx ->
  let lines = Unicode.Segmentation.wrap_lines ~width:100 "Line1\nLine2" in
  if List.length lines >= 2 then
    Ok ()
  else
    Error "Should preserve newlines as separate lines"

let test_wrap_lines_width_respected = fun _ctx ->
  let lines = Unicode.Segmentation.wrap_lines ~width:20 "The quick brown fox jumps" in
  let all_fit =
    List.for_all (fun line -> String.width line <= 20) lines
  in
  if all_fit then
    Ok ()
  else
    Error "All wrapped lines should fit within specified width"

(* ===== String Unicode Functions Tests ===== *)

let test_string_truncate_width = fun _ctx ->
  let s = "Hello world" in
  let truncated = String.truncate_width ~width:5 s in
  let w = String.width truncated in
  if w <= 5 then
    Ok ()
  else
    Error ("Truncated string should have width <= 5, got " ^ Int.to_string w)

let test_string_truncate_width_cjk = fun _ctx ->
  let s = "你好世界" in
  (* Width 8 *)
  let truncated = String.truncate_width ~width:5 s in
  let w = String.width truncated in
  if w <= 5 then
    Ok ()
  else
    Error ("Truncated CJK string should have width <= 5, got " ^ Int.to_string w)

(* ===== East Asian Width Configuration Tests ===== *)

let test_east_asian_width_config = fun _ctx ->
  let original = Unicode.Config.get_east_asian_width () in
  Unicode.Config.set_east_asian_width true;
  let is_set = Unicode.Config.get_east_asian_width () in
  Unicode.Config.set_east_asian_width original;
  (* Restore *)
  if is_set then
    Ok ()
  else
    Error "East Asian width configuration should work"

(* ===== Character Classification Tests ===== *)

let test_rune_is_letter = fun _ctx ->
  match Unicode.Utf8.decode_rune "A" 0 with
  | Some (r, _) ->
      if Unicode.Rune.is_letter r then
        Ok ()
      else
        Error "'A' should be detected as letter"
  | None -> Error "Failed to decode 'A'"

let test_rune_is_digit = fun _ctx ->
  match Unicode.Utf8.decode_rune "0" 0 with
  | Some (r, _) ->
      if Unicode.Rune.is_digit r then
        Ok ()
      else
        Error "'0' should be detected as digit"
  | None -> Error "Failed to decode '0'"

let test_rune_is_space = fun _ctx ->
  match Unicode.Utf8.decode_rune " " 0 with
  | Some (r, _) ->
      if Unicode.Rune.is_space r then
        Ok ()
      else
        Error "' ' should be detected as space"
  | None -> Error "Failed to decode ' '"

let test_rune_is_control = fun _ctx ->
  match Unicode.Utf8.decode_rune "\x00" 0 with
  | Some (r, _) ->
      if Unicode.Rune.is_control r then
        Ok ()
      else
        Error "NUL should be detected as control"
  | None -> Error "Failed to decode NUL"

let test_rune_case_conversion = fun _ctx ->
  match Unicode.Utf8.decode_rune "a" 0 with
  | Some (lower, _) ->
      let upper = Unicode.Rune.to_upper lower in
      let upper_str = Unicode.Rune.to_string upper in
      if upper_str = "A" then
        Ok ()
      else
        Error ("Expected 'A' from to_upper('a'), got '" ^ upper_str ^ "'")
  | None -> Error "Failed to decode 'a'"

(* ===== Extended Character Classification Tests ===== *)

let test_greek_letter_classification = fun _ctx ->
  match Unicode.Utf8.decode_rune "α" 0 with
  | Some (r, _) ->
      if Unicode.Rune.is_letter r then
        Ok ()
      else
        Error "Greek α should be detected as letter"
  | None -> Error "Failed to decode Greek α"

let test_greek_lowercase = fun _ctx ->
  match Unicode.Utf8.decode_rune "α" 0 with
  | Some (r, _) ->
      if Unicode.Rune.is_lower r then
        Ok ()
      else
        Error "Greek α should be detected as lowercase"
  | None -> Error "Failed to decode Greek α"

let test_greek_uppercase = fun _ctx ->
  match Unicode.Utf8.decode_rune "Α" 0 with
  | Some (r, _) ->
      if Unicode.Rune.is_upper r then
        Ok ()
      else
        Error "Greek Α should be detected as uppercase"
  | None -> Error "Failed to decode Greek Α"

let test_cyrillic_letter = fun _ctx ->
  match Unicode.Utf8.decode_rune "А" 0 with
  | Some (r, _) ->
      if Unicode.Rune.is_letter r then
        Ok ()
      else
        Error "Cyrillic А should be detected as letter"
  | None -> Error "Failed to decode Cyrillic А"

let test_cyrillic_uppercase = fun _ctx ->
  match Unicode.Utf8.decode_rune "А" 0 with
  | Some (r, _) ->
      if Unicode.Rune.is_upper r then
        Ok ()
      else
        Error "Cyrillic А should be detected as uppercase"
  | None -> Error "Failed to decode Cyrillic А"

let test_cyrillic_lowercase = fun _ctx ->
  match Unicode.Utf8.decode_rune "а" 0 with
  | Some (r, _) ->
      if Unicode.Rune.is_lower r then
        Ok ()
      else
        Error "Cyrillic а should be detected as lowercase"
  | None -> Error "Failed to decode Cyrillic а"

let test_cjk_letter = fun _ctx ->
  match Unicode.Utf8.decode_rune "中" 0 with
  | Some (r, _) ->
      if Unicode.Rune.is_letter r then
        Ok ()
      else
        Error "CJK 中 should be detected as letter"
  | None -> Error "Failed to decode CJK 中"

let test_arabic_digit = fun _ctx ->
  match Unicode.Utf8.decode_rune "٥" 0 with
  | Some (r, _) ->
      if Unicode.Rune.is_digit r then
        Ok ()
      else
        Error "Arabic ٥ should be detected as digit"
  | None -> Error "Failed to decode Arabic ٥"

let test_hebrew_letter = fun _ctx ->
  match Unicode.Utf8.decode_rune "א" 0 with
  | Some (r, _) ->
      if Unicode.Rune.is_letter r then
        Ok ()
      else
        Error "Hebrew א should be detected as letter"
  | None -> Error "Failed to decode Hebrew א"

(* ===== Category Coverage Tests ===== *)

let test_combining_mark_detection = fun _ctx ->
  (* U+0301 - Combining acute accent *)
  match Unicode.Utf8.decode_rune "́" 0 with
  | Some (r, _) ->
      if Unicode.Rune.is_mark r then
        Ok ()
      else
        Error "Combining acute accent should be detected as mark"
  | None -> Error "Failed to decode combining accent"

let test_math_symbol_detection = fun _ctx ->
  (* U+2211 - N-ary summation ∑ *)
  match Unicode.Utf8.decode_rune "∑" 0 with
  | Some (r, _) ->
      if Unicode.Rune.is_symbol r then
        Ok ()
      else
        Error "∑ should be detected as symbol"
  | None -> Error "Failed to decode ∑"

let test_currency_symbol_detection = fun _ctx ->
  (* U+20AC - Euro sign € *)
  match Unicode.Utf8.decode_rune "€" 0 with
  | Some (r, _) ->
      if Unicode.Rune.is_symbol r then
        Ok ()
      else
        Error "€ should be detected as symbol"
  | None -> Error "Failed to decode €"

let test_roman_numeral_as_number = fun _ctx ->
  (* U+216B - Roman numeral twelve Ⅻ *)
  match Unicode.Utf8.decode_rune "Ⅻ" 0 with
  | Some (r, _) ->
      if Unicode.Rune.is_number r then
        Ok ()
      else
        Error "Ⅻ should be detected as number"
  | None -> Error "Failed to decode Ⅻ"

let test_em_dash_punctuation = fun _ctx ->
  (* U+2014 - Em dash — *)
  match Unicode.Utf8.decode_rune "—" 0 with
  | Some (r, _) ->
      if Unicode.Rune.is_punct r then
        Ok ()
      else
        Error "— should be detected as punctuation"
  | None -> Error "Failed to decode —"

let test_fraction_as_number = fun _ctx ->
  (* U+00BD - Vulgar fraction one half ½ *)
  match Unicode.Utf8.decode_rune "½" 0 with
  | Some (r, _) ->
      if Unicode.Rune.is_number r then
        Ok ()
      else
        Error "½ should be detected as number"
  | None -> Error "Failed to decode ½"

(* ===== Edge Cases Tests ===== *)

let test_invalid_code_point_beyond_unicode = fun _ctx ->
  match Unicode.Rune.of_int 0x11_0000 with
  | Some _ -> Error "Code point beyond Unicode range should return None"
  | None -> Ok ()

let test_invalid_negative_code_point = fun _ctx ->
  match Unicode.Rune.of_int (-1) with
  | Some _ -> Error "Negative code point should return None"
  | None -> Ok ()

let test_surrogate_pair_invalid = fun _ctx ->
  (* U+D800 - High surrogate (invalid in UTF-8) *)
  match Unicode.Rune.of_int 0xd800 with
  | Some _ -> Error "Surrogate pair code point should return None"
  | None -> Ok ()

let test_cjk_has_no_case = fun _ctx ->
  (* CJK ideographs have no case mapping *)
  match Unicode.Utf8.decode_rune "中" 0 with
  | Some (r, _) ->
      let upper = Unicode.Rune.to_upper r in
      let lower = Unicode.Rune.to_lower r in
      if r = upper && r = lower then
        Ok ()
      else
        Error "CJK character should not change case"
  | None -> Error "Failed to decode 中"

let test_titlecase_letter_detection = fun _ctx ->
  (* U+01C5 - Latin capital letter D with small letter z with caron (ǅ) *)
  match Unicode.Utf8.decode_rune "ǅ" 0 with
  | Some (r, _) ->
      if Unicode.Rune.is_title r then
        Ok ()
      else
        Error "ǅ should be detected as titlecase"
  | None -> Error "Failed to decode ǅ"

let test_max_unicode_code_point = fun _ctx ->
  (* U+10FFFF - Maximum valid Unicode code point *)
  match Unicode.Rune.of_int 0x10_ffff with
  | Some r ->
      let code = Unicode.Rune.to_int r in
      if code = 0x10_ffff then
        Ok ()
      else
        let code_hex =
          let hex_chars = "0123456789ABCDEF" in
          let rec to_hex n acc =
            if n = 0 then
              if acc = "" then
                "0"
              else
                acc
            else
              to_hex (n / 16) (String.make 1 (String.get hex_chars (n mod 16)) ^ acc)
          in
          to_hex code ""
        in
        Error ("Expected U+10FFFF, got U+" ^ code_hex)
  | None -> Error "Maximum Unicode code point should be valid"

(* ===== Round-Trip Tests ===== *)

let test_uppercase_roundtrip = fun _ctx ->
  match Unicode.Utf8.decode_rune "A" 0 with
  | Some (r, _) ->
      let lower = Unicode.Rune.to_lower r in
      let upper = Unicode.Rune.to_upper lower in
      if r = upper then
        Ok ()
      else
        Error "to_upper(to_lower('A')) should equal 'A'"
  | None -> Error "Failed to decode 'A'"

let test_lowercase_roundtrip = fun _ctx ->
  match Unicode.Utf8.decode_rune "a" 0 with
  | Some (r, _) ->
      let upper = Unicode.Rune.to_upper r in
      let lower = Unicode.Rune.to_lower upper in
      if r = lower then
        Ok ()
      else
        Error "to_lower(to_upper('a')) should equal 'a'"
  | None -> Error "Failed to decode 'a'"

let test_greek_roundtrip = fun _ctx ->
  match Unicode.Utf8.decode_rune "α" 0 with
  | Some (r, _) ->
      let upper = Unicode.Rune.to_upper r in
      let lower = Unicode.Rune.to_lower upper in
      if r = lower then
        Ok ()
      else
        Error "Greek roundtrip should preserve original"
  | None -> Error "Failed to decode α"

let test_utf8_encode_decode_roundtrip = fun _ctx ->
  match Unicode.Utf8.decode_rune "世" 0 with
  | Some (r, _) ->
      let encoded = Unicode.Utf8.encode_rune r in
      if encoded = "世" then
        Ok ()
      else
        Error ("Expected '世', got '" ^ encoded ^ "'")
  | None -> Error "Failed to decode 世"

let test_rune_to_int_of_int_roundtrip = fun _ctx ->
  let code = 0x1_f44d in
  (* 👍 *)
  match Unicode.Rune.of_int code with
  | Some r ->
      let code2 = Unicode.Rune.to_int r in
      if code = code2 then
        Ok ()
      else
        let hex_chars = "0123456789ABCDEF" in
        let to_hex n =
          let rec helper n acc =
            if n = 0 then
              if acc = "" then
                "0"
              else
                acc
            else
              helper (n / 16) (String.make 1 (String.get hex_chars (n mod 16)) ^ acc)
          in
          helper n ""
        in
        Error ("Expected U+" ^ to_hex code ^ ", got U+" ^ to_hex code2)
  | None -> Error "Failed to create rune from valid code point"

let test_ascii_roundtrip_regression = fun _ctx ->
  (* Ensure ASCII still works after Unicode tables *)
  let test_char c =
    match Unicode.Utf8.decode_rune (String.make 1 c) 0 with
    | Some (r, _) ->
        let encoded = Unicode.Utf8.encode_rune r in
        encoded = String.make 1 c
    | None -> false
  in
  if test_char 'A' && test_char 'z' && test_char '0' && test_char '!' then
    Ok ()
  else
    Error "ASCII roundtrip failed"

(* ===== Case Conversion Tests ===== *)

let test_greek_case_conversion = fun _ctx ->
  match Unicode.Utf8.decode_rune "α" 0 with
  | Some (lower, _) ->
      let upper = Unicode.Rune.to_upper lower in
      let upper_code = Unicode.Rune.to_int upper in
      if upper_code = 0x0391 then
        Ok ()
        (* Α *)
      else
        let hex_chars = "0123456789ABCDEF" in
        let rec to_hex n acc len =
          if len = 0 then
            acc
          else
            to_hex (n / 16) (String.make 1 (String.get hex_chars (n mod 16)) ^ acc) (len - 1)
        in
        Error ("Expected U+0391 (Α), got U+" ^ to_hex upper_code "" 4)
  | None -> Error "Failed to decode Greek α"

let test_cyrillic_case_conversion = fun _ctx ->
  match Unicode.Utf8.decode_rune "а" 0 with
  | Some (lower, _) ->
      let upper = Unicode.Rune.to_upper lower in
      let upper_code = Unicode.Rune.to_int upper in
      if upper_code = 0x0410 then
        Ok ()
        (* А *)
      else
        let hex_chars = "0123456789ABCDEF" in
        let rec to_hex n acc len =
          if len = 0 then
            acc
          else
            to_hex (n / 16) (String.make 1 (String.get hex_chars (n mod 16)) ^ acc) (len - 1)
        in
        Error ("Expected U+0410 (А), got U+" ^ to_hex upper_code "" 4)
  | None -> Error "Failed to decode Cyrillic а"

let test_greek_uppercase_to_lowercase = fun _ctx ->
  match Unicode.Utf8.decode_rune "Α" 0 with
  | Some (upper, _) ->
      let lower = Unicode.Rune.to_lower upper in
      let lower_code = Unicode.Rune.to_int lower in
      if lower_code = 0x03b1 then
        Ok ()
        (* α *)
      else
        let hex_chars = "0123456789ABCDEF" in
        let rec to_hex n acc len =
          if len = 0 then
            acc
          else
            to_hex (n / 16) (String.make 1 (String.get hex_chars (n mod 16)) ^ acc) (len - 1)
        in
        Error ("Expected U+03B1 (α), got U+" ^ to_hex lower_code "" 4)
  | None -> Error "Failed to decode Greek Α"

let test_latin_extended_uppercase = fun _ctx ->
  (* Test Latin Extended-A character: Ā (U+0100) -> ā (U+0101) *)
  match Unicode.Utf8.decode_rune "Ā" 0 with
  | Some (upper, _) ->
      let lower = Unicode.Rune.to_lower upper in
      let lower_code = Unicode.Rune.to_int lower in
      if lower_code = 0x0101 then
        Ok ()
        (* ā *)
      else
        let hex_chars = "0123456789ABCDEF" in
        let rec to_hex n acc len =
          if len = 0 then
            acc
          else
            to_hex (n / 16) (String.make 1 (String.get hex_chars (n mod 16)) ^ acc) (len - 1)
        in
        Error ("Expected U+0101 (ā), got U+" ^ to_hex lower_code "" 4)
  | None -> Error "Failed to decode Ā"

(* ===== Integration Tests ===== *)

let test_integration_mixed_content = fun _ctx ->
  let text = "Hello 世界! How are you?" in
  let width = String.width text in
  let graphemes = String.grapheme_count text in
  let runes = String.rune_count text in
  if width > 0 && graphemes > 0 && runes > 0 then
    Ok ()
  else
    Error "Mixed content should have positive width, grapheme count, and rune count"

let test_integration_wrap_and_width = fun _ctx ->
  let text = "Hello world, this is a test" in
  let lines = Unicode.Segmentation.wrap_lines ~width:15 text in
  let all_valid =
    List.for_all
      (fun line ->
        let w = String.width line in
        w <= 15)
      lines
  in
  if all_valid then
    Ok ()
  else
    Error "All wrapped lines should respect width constraint"

(* ===== Test Suite ===== *)

let tests =
  Test.[
    case "rune width ascii" test_rune_width_ascii;
    case "rune width cjk" test_rune_width_cjk;
    case "rune width combining" test_rune_width_combining;
    case "rune width emoji" test_rune_width_emoji;
    case "rune width zwj" test_rune_width_zwj;
    case "rune width fullwidth" test_rune_width_fullwidth;
    case "string width ascii" test_string_width_ascii;
    case "string width cjk" test_string_width_cjk;
    case "string width mixed" test_string_width_mixed;
    case "string width emoji" test_string_width_emoji;
    case "string width combining" test_string_width_combining;
    case "grapheme count ascii" test_grapheme_count_ascii;
    case "grapheme count cjk" test_grapheme_count_cjk;
    case "grapheme count emoji with modifier" test_grapheme_count_emoji_with_modifier;
    case "rune count ascii" test_rune_count_ascii;
    case "rune count cjk" test_rune_count_cjk;
    case "rune count emoji" test_rune_count_emoji;
    case "utf8 is valid" test_utf8_valid;
    case "utf8 decode rune" test_utf8_decode;
    case "utf8 encode rune" test_utf8_encode;
    case "rune of_int valid" test_rune_of_int_valid;
    case "rune of_int invalid" test_rune_of_int_invalid;
    case "rune to_int" test_rune_to_int;
    case "word boundaries simple" test_word_boundaries_simple;
    case "word split simple" test_word_split_simple;
    case "word split contractions" test_word_split_contractions;
    case "word split identifiers" test_word_split_identifiers;
    case "next word start" test_next_word_start;
    case "previous word start" test_prev_word_start;
    case "line breaks at newline" test_line_breaks_newline;
    case "line breaks at space" test_line_breaks_space;
    case "wrap lines simple" test_wrap_lines_simple;
    case "wrap lines short text" test_wrap_lines_short;
    case "wrap lines cjk" test_wrap_lines_cjk;
    case "wrap lines preserves newlines" test_wrap_lines_preserves_newlines;
    case "wrap lines width respected" test_wrap_lines_width_respected;
    case "string truncate width" test_string_truncate_width;
    case "string truncate width cjk" test_string_truncate_width_cjk;
    case "east asian width config" test_east_asian_width_config;
    case "rune is letter" test_rune_is_letter;
    case "rune is digit" test_rune_is_digit;
    case "rune is space" test_rune_is_space;
    case "rune is control" test_rune_is_control;
    case "rune case conversion" test_rune_case_conversion;
    case "combining mark detection" test_combining_mark_detection;
    case "math symbol detection" test_math_symbol_detection;
    case "currency symbol detection" test_currency_symbol_detection;
    case "roman numeral as number" test_roman_numeral_as_number;
    case "em dash punctuation" test_em_dash_punctuation;
    case "fraction as number" test_fraction_as_number;
    case "invalid code point beyond unicode" test_invalid_code_point_beyond_unicode;
    case "invalid negative code point" test_invalid_negative_code_point;
    case "surrogate pair invalid" test_surrogate_pair_invalid;
    case "cjk has no case" test_cjk_has_no_case;
    case "titlecase letter detection" test_titlecase_letter_detection;
    case "max unicode code point" test_max_unicode_code_point;
    case "uppercase roundtrip" test_uppercase_roundtrip;
    case "lowercase roundtrip" test_lowercase_roundtrip;
    case "greek roundtrip" test_greek_roundtrip;
    case "utf8 encode decode roundtrip" test_utf8_encode_decode_roundtrip;
    case "rune to_int of_int roundtrip" test_rune_to_int_of_int_roundtrip;
    case "ascii roundtrip regression" test_ascii_roundtrip_regression;
    case "greek letter classification" test_greek_letter_classification;
    case "greek lowercase detection" test_greek_lowercase;
    case "greek uppercase detection" test_greek_uppercase;
    case "cyrillic letter detection" test_cyrillic_letter;
    case "cyrillic uppercase detection" test_cyrillic_uppercase;
    case "cyrillic lowercase detection" test_cyrillic_lowercase;
    case "cjk letter detection" test_cjk_letter;
    case "arabic digit detection" test_arabic_digit;
    case "hebrew letter detection" test_hebrew_letter;
    case "greek case conversion" test_greek_case_conversion;
    case "cyrillic case conversion" test_cyrillic_case_conversion;
    case "greek uppercase to lowercase" test_greek_uppercase_to_lowercase;
    case "latin extended uppercase" test_latin_extended_uppercase;
    case "integration mixed content" test_integration_mixed_content;
    case "integration wrap and width" test_integration_wrap_and_width;
  ]

let () =
  Miniriot.run ~main:(fun ~args -> Test.Cli.main ~name:"unicode" ~tests ~args) ~args:Env.args ()
