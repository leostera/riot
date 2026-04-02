open Std

let diff_strings = fun left right ->
  let max_len = max (String.length left) (String.length right) in
  let rec loop idx acc =
    if idx >= max_len then
      List.rev acc
    else
      let next =
        let left_char =
          if idx < String.length left then
            Some left.[idx]
          else
            None
        in
        let right_char =
          if idx < String.length right then
            Some right.[idx]
          else
            None
        in
        match (left_char, right_char) with
        | Some x, Some y when x = y -> acc
        | None, Some y -> { Diff.path = [ Diff.Index idx ]; kind = Diff.Added y } :: acc
        | Some x, None -> { Diff.path = [ Diff.Index idx ]; kind = Diff.Removed x } :: acc
        | Some x, Some y -> { Diff.path = [ Diff.Index idx ]; kind = Diff.Changed (x, y) } :: acc
        | None, None -> acc
      in
      loop (idx + 1) next
  in
  loop 0 []

let test_diff_identical_strings = fun _ctx ->
  let diffs = diff_strings "riot" "riot" in
  if diffs = [] then
    Ok ()
  else
    Error "Identical strings should produce no changes"

let test_diff_different_strings = fun _ctx ->
  let diffs = diff_strings "ab" "cd" in
  if List.length (Diff.changes diffs) = 2 then
    Ok ()
  else
    Error "Expected every character to change"

let test_diff_empty_strings = fun _ctx ->
  let diffs = diff_strings "" "" in
  if not (Diff.has_changes diffs) then
    Ok ()
  else
    Error "Empty strings should produce no changes"

let test_diff_one_empty = fun _ctx ->
  let diffs = diff_strings "" "abc" in
  if List.length (Diff.additions diffs) = 3 then
    Ok ()
  else
    Error "Expected all characters to be additions"

let test_diff_char_by_char = fun _ctx ->
  let diffs = diff_strings "abc" "axc" in
  match Diff.changes diffs with
  | [ { path=[ Diff.Index 1 ]; kind=Diff.Changed ('b', 'x') } ] -> Ok ()
  | _ -> Error "Expected a change at the middle character"

let test_diff_inserted_chars = fun _ctx ->
  let diffs = diff_strings "ab" "abc" in
  match Diff.additions diffs with
  | [ { path=[ Diff.Index 2 ]; kind=Diff.Added 'c' } ] -> Ok ()
  | _ -> Error "Expected one inserted character"

let test_diff_deleted_chars = fun _ctx ->
  let diffs = diff_strings "abc" "ab" in
  match Diff.removals diffs with
  | [ { path=[ Diff.Index 2 ]; kind=Diff.Removed 'c' } ] -> Ok ()
  | _ -> Error "Expected one deleted character"

let test_diff_replaced_chars = fun _ctx ->
  let diffs = diff_strings "cat" "cut" in
  match Diff.changes diffs with
  | [ { path=[ Diff.Index 1 ]; kind=Diff.Changed ('a', 'u') } ] -> Ok ()
  | _ -> Error "Expected one replaced character"

let test_diff_case_change = fun _ctx ->
  let diffs = diff_strings "Riot" "riot" in
  match Diff.changes diffs with
  | [ { path=[ Diff.Index 0 ]; kind=Diff.Changed ('R', 'r') } ] -> Ok ()
  | _ -> Error "Expected one case-only change"

let test_diff_whitespace_changes = fun _ctx ->
  let diffs = diff_strings "a b" "a\tb" in
  match Diff.changes diffs with
  | [ { path=[ Diff.Index 1 ]; kind=Diff.Changed (' ', '\t') } ] -> Ok ()
  | _ -> Error "Expected whitespace change at index 1"

let tests =
  Test.[
    case "identical strings" test_diff_identical_strings;
    case "different strings" test_diff_different_strings;
    case "empty strings" test_diff_empty_strings;
    case "one empty" test_diff_one_empty;
    case "char by char" test_diff_char_by_char;
    case "inserted chars" test_diff_inserted_chars;
    case "deleted chars" test_diff_deleted_chars;
    case "replaced chars" test_diff_replaced_chars;
    case "case change" test_diff_case_change;
    case "whitespace changes" test_diff_whitespace_changes;
  ]

let () =
  Miniriot.run ~main:(fun ~args -> Test.Cli.main ~name:"string-diff" ~tests ~args) ~args:Env.args ()
