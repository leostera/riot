open Std

let diff_lists = fun left right ->
  let max_len = max (List.length left) (List.length right) in
  let rec loop idx acc =
    if idx >= max_len then
      List.rev acc
    else
      let next =
        match (List.nth_opt left idx, List.nth_opt right idx) with
        | Some x, Some y when x = y -> acc
        | None, Some y -> { Diff.path = [ Diff.Index idx ]; kind = Diff.Added y } :: acc
        | Some x, None -> { Diff.path = [ Diff.Index idx ]; kind = Diff.Removed x } :: acc
        | Some x, Some y -> { Diff.path = [ Diff.Index idx ]; kind = Diff.Changed (x, y) } :: acc
        | None, None -> acc
      in
      loop (idx + 1) next
  in
  loop 0 []

let test_diff_identical_lists = fun _ctx ->
  let diffs = diff_lists [ 1; 2 ] [ 1; 2 ] in
  if diffs = [] then
    Ok ()
  else
    Error "Identical lists should produce no changes"

let test_diff_empty_lists = fun _ctx ->
  let diffs = diff_lists [] [] in
  if not (Diff.has_changes diffs) then
    Ok ()
  else
    Error "Empty lists should produce no changes"

let test_diff_added_elements = fun _ctx ->
  let diffs = diff_lists [ 1 ] [ 1; 2 ] in
  match Diff.additions diffs with
  | [ { path=[ Diff.Index 1 ]; kind=Diff.Added 2 } ] -> Ok ()
  | _ -> Error "Expected one added element"

let test_diff_removed_elements = fun _ctx ->
  let diffs = diff_lists [ 1; 2 ] [ 1 ] in
  match Diff.removals diffs with
  | [ { path=[ Diff.Index 1 ]; kind=Diff.Removed 2 } ] -> Ok ()
  | _ -> Error "Expected one removed element"

let test_diff_changed_elements = fun _ctx ->
  let diffs = diff_lists [ 1; 2 ] [ 1; 3 ] in
  match Diff.changes diffs with
  | [ { path=[ Diff.Index 1 ]; kind=Diff.Changed (2, 3) } ] -> Ok ()
  | _ -> Error "Expected one changed element"

let test_diff_reordered_elements = fun _ctx ->
  let diffs = diff_lists [ 1; 2 ] [ 2; 1 ] in
  if List.length (Diff.changes diffs) = 2 then
    Ok ()
  else
    Error "Expected both positions to change when reordered"

let test_diff_nested_lists = fun _ctx ->
  let diffs = [ { Diff.path = [ Diff.Index 0 ]; kind = Diff.Changed ([ 1 ], [ 2 ]) } ] in
  match Diff.changes diffs with
  | [ { kind=Diff.Changed ([ 1 ], [ 2 ]); _ } ] -> Ok ()
  | _ -> Error "Expected changed nested list payload"

let test_diff_one_empty = fun _ctx ->
  let diffs = diff_lists [] [ 1; 2 ] in
  if List.length (Diff.additions diffs) = 2 then
    Ok ()
  else
    Error "Expected all elements to be additions"

let test_diff_different_lengths = fun _ctx ->
  let diffs = diff_lists [ 1 ] [ 1; 2; 3 ] in
  if List.length diffs = 2 then
    Ok ()
  else
    Error "Expected two additions"

let test_diff_mixed_changes = fun _ctx ->
  let diffs = diff_lists [ 1; 2; 3 ] [ 1; 4; 5; 6 ] in
  if List.length (Diff.changes diffs) = 2 && List.length (Diff.additions diffs) = 1 then
    Ok ()
  else
    Error "Expected mixed list diff"

let tests =
  Test.[
    case "identical lists" test_diff_identical_lists;
    case "empty lists" test_diff_empty_lists;
    case "added elements" test_diff_added_elements;
    case "removed elements" test_diff_removed_elements;
    case "changed elements" test_diff_changed_elements;
    case "reordered elements" test_diff_reordered_elements;
    case "nested lists" test_diff_nested_lists;
    case "one empty" test_diff_one_empty;
    case "different lengths" test_diff_different_lengths;
    case "mixed changes" test_diff_mixed_changes;
  ]

let () =
  Miniriot.run ~main:(fun ~args -> Test.Cli.main ~name:"list-diff" ~tests ~args) ~args:Env.args ()
