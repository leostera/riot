open Std
open Std.Result.Syntax

let rand_int = fun rng bound ->
  Random.int ~rng bound
  |> Result.map_err ~fn:(fun err -> Error.Random_error (Random.error_to_string err))

let random_byte = fun rng ->
  rand_int rng 256
  |> Result.map ~fn:Char.from_int_unchecked

let truncate_to_max = fun ~max_len input ->
  if String.length input <= max_len then
    input
  else
    String.sub input ~offset:0 ~len:max_len

let string_slice = fun input ~offset ~len ->
  if len <= 0 then
    ""
  else
    String.sub input ~offset ~len

let default_dictionary = [
  "";
  " ";
  "\n";
  "\000";
  "let";
  "let x = 0\n";
  "type t = A\n";
  "module M = struct end\n";
  "match x with | _ -> x";
  "fun x -> x";
  "->";
  "=";
  "(";
  ")";
  "[";
  "]";
  "{";
  "}";
  "\"";
  "'";
  "(*";
  "*)";
]

let interesting_chunks = fun dictionary -> default_dictionary @ dictionary

let choose_interesting_chunk = fun rng ~dictionary ->
  let chunks = interesting_chunks dictionary in
  let* idx = rand_int rng (List.length chunks) in
  Ok (
    List.get chunks ~at:idx
    |> Option.unwrap_or ~default:""
  )

let insert_chunk = fun ~max_len input ~at chunk ->
  let len = String.length input in
  let available = max_len - len in
  if available <= 0 then
    truncate_to_max ~max_len input
  else
    let chunk =
      if String.length chunk <= available then
        chunk
      else
        String.sub chunk ~offset:0 ~len:available
    in
    string_slice input ~offset:0 ~len:at ^ chunk ^ string_slice input ~offset:at ~len:(len - at)

let mutate_flip = fun rng input ->
  let len = String.length input in
  if Int.equal len 0 then
    Ok input
  else
    let* at = rand_int rng len in
    let* char = random_byte rng in
    let bytes = IO.Bytes.from_string input in
    IO.Bytes.set_unchecked bytes ~at ~char;
  Ok (IO.Bytes.to_string bytes)

let mutate_overwrite = fun rng ~dictionary input ->
  let len = String.length input in
  if Int.equal len 0 then
    Ok input
  else
    let* at = rand_int rng len in
    let* chunk = choose_interesting_chunk rng ~dictionary in
    if String.equal chunk "" then
      mutate_flip rng input
    else
      let write_len = Int.min (String.length chunk) (len - at) in
      let bytes = IO.Bytes.from_string input in
      for idx = 0 to write_len - 1 do
        IO.Bytes.set_unchecked
          bytes
          ~at:(at + idx)
          ~char:(String.get_unchecked chunk ~at:idx)
      done;
  Ok (IO.Bytes.to_string bytes)

let mutate_insert = fun rng ~max_len ~dictionary input ->
  let len = String.length input in
  if len >= max_len then
    mutate_flip rng input
  else
    let* at = rand_int rng (len + 1) in
    let* use_token = rand_int rng 4 in
    if Int.equal use_token 0 then
      let* chunk = choose_interesting_chunk rng ~dictionary in
      Ok (insert_chunk ~max_len input ~at chunk)
    else
      let* char = random_byte rng in
      Ok (insert_chunk
        ~max_len
        input
        ~at
        (String.make ~len:1 ~char))

let mutate_delete = fun rng input ->
  let len = String.length input in
  if len <= 1 then
    Ok ""
  else
    let* at = rand_int rng len in
    let* delete_len = rand_int rng (Int.min 16 (len - at)) in
    let delete_len = delete_len + 1 in
    Ok (string_slice input ~offset:0 ~len:at
    ^ string_slice input ~offset:(at + delete_len) ~len:(len - at - delete_len))

let mutate_duplicate = fun rng ~max_len ~dictionary input ->
  let len = String.length input in
  if Int.equal len 0 || len >= max_len then
    mutate_insert rng ~max_len ~dictionary input
  else
    let* at = rand_int rng len in
    let* copy_len = rand_int rng (Int.min 32 (len - at)) in
    let copy_len = copy_len + 1 in
    let chunk = string_slice input ~offset:at ~len:copy_len in
    let* insert_at = rand_int rng (len + 1) in
    Ok (insert_chunk ~max_len input ~at:insert_at chunk)

let choose_corpus_input = fun rng corpus ->
  match corpus with
  | [] -> Ok ""
  | _ ->
      let* idx = rand_int rng (List.length corpus) in
      Ok (
        List.get corpus ~at:idx
        |> Option.unwrap_or ~default:""
      )

let mutate_splice = fun rng ~max_len ~corpus ~dictionary input ->
  match corpus with
  | [] -> mutate_insert rng ~max_len ~dictionary input
  | _ ->
      let len = String.length input in
      let* other = choose_corpus_input rng corpus in
      let other_len = String.length other in
      if Int.equal len 0 || Int.equal other_len 0 then
        mutate_insert rng ~max_len ~dictionary input
      else
        let* left_len = rand_int rng (len + 1) in
        let* right_offset = rand_int rng other_len in
        Ok (
          string_slice input ~offset:0 ~len:left_len
          ^ string_slice other ~offset:right_offset ~len:(other_len - right_offset)
          |> truncate_to_max ~max_len
        )

let mutate_simple = fun rng ~max_len ~corpus ~dictionary ~splicing input ->
  if String.length input = 0 then
    mutate_insert rng ~max_len ~dictionary input
  else
    let operation_count =
      if splicing then
        6
      else
        5
    in
    let* op = rand_int rng operation_count in
    match op with
    | 0 -> mutate_flip rng input
    | 1 -> mutate_overwrite rng ~dictionary input
    | 2 -> mutate_insert rng ~max_len ~dictionary input
    | 3 -> mutate_delete rng input
    | 4 -> mutate_duplicate rng ~max_len ~dictionary input
    | _ -> mutate_splice rng ~max_len ~corpus ~dictionary input

let mutate = fun rng ~max_len ~corpus ~dictionary ~splicing input ->
  let* op_count = rand_int rng 8 in
  let rec loop remaining input =
    if remaining <= 0 then
      Ok (truncate_to_max ~max_len input)
    else
      let* next = mutate_simple rng ~max_len ~corpus ~dictionary ~splicing input in
      loop (remaining - 1) next
  in
  loop (op_count + 1) input
