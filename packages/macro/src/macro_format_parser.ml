open Std

(* The format-literal parser is intentionally small and explicit. It is the
   first place where [format!] keeps semantic structure instead of immediately
   lowering everything to string concatenation. *)
type hole =
  | Next_arg_to_string
  | Var_to_string of string

type item =
  | String of string
  | Hole of hole

type t = item list

let char_at = fun text index ->
  if index < 0 then
    None
  else
    try Some text.[index] with
    | Invalid_argument _ -> None

let contains_char = fun text expected ->
  let rec loop index =
    match char_at text index with
    | None -> false
    | Some ch ->
        if ch = expected then
          true
        else
          loop (index + 1)
  in
  loop 0

let is_ident_start = function
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '_' -> true
  | _ -> false

let is_ident_continue = function
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '0' .. '9'
  | '_'
  | '\'' -> true
  | _ -> false

let is_capture_name = fun text ->
  let rec consume_segment index =
    match char_at text index with
    | Some ch when is_ident_continue ch -> consume_segment (index + 1)
    | _ -> index
  in
  let rec loop index =
    match char_at text index with
    | Some ch when is_ident_start ch ->
        let next = consume_segment (index + 1) in
        (
          match char_at text next with
          | None -> true
          | Some '.' -> loop (next + 1)
          | Some _ -> false
        )
    | _ -> false
  in
  loop 0

let parse_placeholder = fun placeholder_text ~span ->
  if String.equal placeholder_text "" then
    Ok Next_arg_to_string
  else if contains_char placeholder_text ':' then
    Error (Macro_error.make ~span "format! currently supports only {} and {name} placeholders")
  else if is_capture_name placeholder_text then
    Ok (Var_to_string placeholder_text)
  else
    Error (Macro_error.make ~span "format! currently supports only {} and {name} placeholders")

let parse_literal = fun ~literal_text ~span ->
  let literal_len = String.length literal_text in
  if
    literal_len < 2
    || char_at literal_text 0 != Some '"'
    || char_at literal_text (literal_len - 1) != Some '"'
  then
    Error (Macro_error.make ~span "format! currently requires an ordinary string literal format string")
  else
    let current = IO.Buffer.create literal_len in
    let flush acc =
      let text = IO.Buffer.contents current in
      IO.Buffer.clear current;
      if String.equal text "" then
        acc
      else
        String text :: acc
    in
    let rec find_closing_brace index =
      if index >= literal_len - 1 then
        None
      else if char_at literal_text index = Some '}' then
        Some index
      else
        find_closing_brace (index + 1)
    in
    let rec loop index items_rev =
      if index >= literal_len - 1 then
        Ok (List.rev (flush items_rev))
      else
        match char_at literal_text index with
        | None ->
            Ok (List.rev (flush items_rev))
        | Some '\\' ->
            if index + 1 < literal_len - 1 then
              (
                IO.Buffer.add_char current '\\';
                (
                  match char_at literal_text (index + 1) with
                  | Some next -> IO.Buffer.add_char current next
                  | None -> ()
                );
                loop (index + 2) items_rev
              )
            else (
              IO.Buffer.add_char current '\\';
              loop (index + 1) items_rev
            )
        | Some '{' ->
            if index + 1 < literal_len - 1 && char_at literal_text (index + 1) = Some '{' then
              (
                IO.Buffer.add_char current '{';
                loop (index + 2) items_rev
              )
            else
              (
                match find_closing_brace (index + 1) with
                | None -> Error (Macro_error.make ~span "format! found an unmatched '{' in the format string")
                | Some closing_index ->
                    let placeholder_text = String.sub
                      literal_text
                      (index + 1)
                      (closing_index - index - 1) in
                    match parse_placeholder placeholder_text ~span with
                    | Error _ as err -> err
                    | Ok hole ->
                        let items_rev = flush items_rev in
                        loop (closing_index + 1) (Hole hole :: items_rev)
              )
        | Some '}' ->
            if index + 1 < literal_len - 1 && char_at literal_text (index + 1) = Some '}' then
              (
                IO.Buffer.add_char current '}';
                loop (index + 2) items_rev
              )
            else
              Error (Macro_error.make ~span "format! found an unmatched '}' in the format string")
        | Some ch ->
            IO.Buffer.add_char current ch;
            loop (index + 1) items_rev
    in
    loop 1 []
