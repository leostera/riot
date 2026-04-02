open Std
open Std.Collections

let diff_hashmaps = fun left right ->
  let keys = (HashMap.to_list left |> List.map fst) @ (HashMap.to_list right |> List.map fst)
  |> List.sort_uniq String.compare in
  List.filter_map
    (fun key ->
      match (HashMap.get left key, HashMap.get right key) with
      | Some x, Some y when x = y -> None
      | None, Some y -> Some { Diff.path = [ Diff.Key key ]; kind = Diff.Added y }
      | Some x, None -> Some { Diff.path = [ Diff.Key key ]; kind = Diff.Removed x }
      | Some x, Some y -> Some { Diff.path = [ Diff.Key key ]; kind = Diff.Changed (x, y) }
      | None, None -> None)
    keys

let make_map = fun items -> HashMap.of_list items

let test_diff_identical_hashmaps = fun _ctx ->
  let diffs = diff_hashmaps (make_map [ ("a", 1) ]) (make_map [ ("a", 1) ]) in
  if not (Diff.has_changes diffs) then
    Ok ()
  else
    Error "Identical maps should produce no changes"

let test_diff_empty_hashmaps = fun _ctx ->
  let diffs = diff_hashmaps (make_map []) (make_map []) in
  if diffs = [] then
    Ok ()
  else
    Error "Empty maps should produce no changes"

let test_diff_added_keys = fun _ctx ->
  let diffs = diff_hashmaps (make_map [ ("a", 1) ]) (make_map [ ("a", 1); ("b", 2) ]) in
  match Diff.additions diffs with
  | [ { path=[ Diff.Key "b" ]; kind=Diff.Added 2 } ] -> Ok ()
  | _ -> Error "Expected one added key"

let test_diff_removed_keys = fun _ctx ->
  let diffs = diff_hashmaps (make_map [ ("a", 1); ("b", 2) ]) (make_map [ ("a", 1) ]) in
  match Diff.removals diffs with
  | [ { path=[ Diff.Key "b" ]; kind=Diff.Removed 2 } ] -> Ok ()
  | _ -> Error "Expected one removed key"

let test_diff_changed_values = fun _ctx ->
  let diffs = diff_hashmaps (make_map [ ("a", 1) ]) (make_map [ ("a", 2) ]) in
  match Diff.changes diffs with
  | [ { path=[ Diff.Key "a" ]; kind=Diff.Changed (1, 2) } ] -> Ok ()
  | _ -> Error "Expected one changed value"

let test_diff_mixed_changes = fun _ctx ->
  let diffs = diff_hashmaps (make_map [ ("a", 1); ("b", 2) ]) (make_map [ ("a", 3); ("c", 4) ]) in
  if
    List.length (Diff.additions diffs) = 1
    && List.length (Diff.removals diffs) = 1
    && List.length (Diff.changes diffs) = 1
  then
    Ok ()
  else
    Error "Expected one addition, one removal, and one change"

let test_diff_nested_hashmaps = fun _ctx ->
  let before = make_map [ ("x", 1); ("y", 2) ] in
  let after = make_map [ ("x", 1); ("y", 3) ] in
  let nested_change = { Diff.path = [ Diff.Key "outer" ]; kind = Diff.Changed (before, after) } in
  match Diff.changes [ nested_change ] with
  | [ { kind=Diff.Changed (left, right); _ } ] when HashMap.get left "y" = Some 2
  && HashMap.get right "y" = Some 3 -> Ok ()
  | _ -> Error "Expected changed nested hashmap payload"

let test_diff_one_empty = fun _ctx ->
  let diffs = diff_hashmaps (make_map []) (make_map [ ("a", 1); ("b", 2) ]) in
  if List.length (Diff.additions diffs) = 2 then
    Ok ()
  else
    Error "Expected additions for every key in the non-empty map"

let test_diff_different_sizes = fun _ctx ->
  let diffs = diff_hashmaps (make_map [ ("a", 1) ]) (make_map [ ("a", 1); ("b", 2); ("c", 3) ]) in
  if List.length diffs = 2 then
    Ok ()
  else
    Error "Expected two additions from size mismatch"

let test_diff_all_different = fun _ctx ->
  let diffs = diff_hashmaps (make_map [ ("a", 1); ("b", 2) ]) (make_map [ ("c", 3); ("d", 4) ]) in
  if List.length (Diff.additions diffs) = 2 && List.length (Diff.removals diffs) = 2 then
    Ok ()
  else
    Error "Expected all keys to be removed or added"

let tests =
  Test.[
    case "identical hashmaps" test_diff_identical_hashmaps;
    case "empty hashmaps" test_diff_empty_hashmaps;
    case "added keys" test_diff_added_keys;
    case "removed keys" test_diff_removed_keys;
    case "changed values" test_diff_changed_values;
    case "mixed changes" test_diff_mixed_changes;
    case "nested hashmaps" test_diff_nested_hashmaps;
    case "one empty" test_diff_one_empty;
    case "different sizes" test_diff_different_sizes;
    case "all different" test_diff_all_different;
  ]

let () =
  Miniriot.run ~main:(fun ~args -> Test.Cli.main ~name:"hashmap-diff" ~tests ~args) ~args:Env.args ()
