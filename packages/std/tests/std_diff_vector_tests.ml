open Std
open Std.Collections

let diff_vectors = fun left right ->
  let max_len = max (Vector.len left) (Vector.len right) in
  let rec loop idx acc =
    if idx >= max_len then
      List.rev acc
    else
      let next =
        match (Vector.get left idx, Vector.get right idx) with
        | Some x, Some y when x = y -> acc
        | None, Some y -> { Diff.path = [ Diff.Index idx ]; kind = Diff.Added y } :: acc
        | Some x, None -> { Diff.path = [ Diff.Index idx ]; kind = Diff.Removed x } :: acc
        | Some x, Some y -> { Diff.path = [ Diff.Index idx ]; kind = Diff.Changed (x, y) } :: acc
        | None, None -> acc
      in
      loop (idx + 1) next
  in
  loop 0 []

let make_vec = fun items -> Vector.of_list items

let test_diff_identical_vectors = fun _ctx ->
  let diffs = diff_vectors (make_vec [ 1; 2 ]) (make_vec [ 1; 2 ]) in
  if diffs = [] then
    Ok ()
  else
    Error "Identical vectors should produce no changes"

let test_diff_empty_vectors = fun _ctx ->
  let diffs = diff_vectors (make_vec []) (make_vec []) in
  if not (Diff.has_changes diffs) then
    Ok ()
  else
    Error "Empty vectors should produce no changes"

let test_diff_added_elements = fun _ctx ->
  let diffs = diff_vectors (make_vec [ 1 ]) (make_vec [ 1; 2 ]) in
  match Diff.additions diffs with
  | [ { path=[ Diff.Index 1 ]; kind=Diff.Added 2 } ] -> Ok ()
  | _ -> Error "Expected one added element"

let test_diff_removed_elements = fun _ctx ->
  let diffs = diff_vectors (make_vec [ 1; 2 ]) (make_vec [ 1 ]) in
  match Diff.removals diffs with
  | [ { path=[ Diff.Index 1 ]; kind=Diff.Removed 2 } ] -> Ok ()
  | _ -> Error "Expected one removed element"

let test_diff_changed_elements = fun _ctx ->
  let diffs = diff_vectors (make_vec [ 1; 2 ]) (make_vec [ 1; 3 ]) in
  match Diff.changes diffs with
  | [ { path=[ Diff.Index 1 ]; kind=Diff.Changed (2, 3) } ] -> Ok ()
  | _ -> Error "Expected one changed element"

let test_diff_different_lengths = fun _ctx ->
  let diffs = diff_vectors (make_vec [ 1 ]) (make_vec [ 1; 2; 3 ]) in
  if List.length diffs = 2 then
    Ok ()
  else
    Error "Expected two additions"

let test_diff_one_empty = fun _ctx ->
  let diffs = diff_vectors (make_vec []) (make_vec [ 1; 2 ]) in
  if List.length (Diff.additions diffs) = 2 then
    Ok ()
  else
    Error "Expected all elements to be additions"

let test_diff_all_different = fun _ctx ->
  let diffs = diff_vectors (make_vec [ 1; 2 ]) (make_vec [ 3; 4 ]) in
  if List.length (Diff.changes diffs) = 2 then
    Ok ()
  else
    Error "Expected every element to be changed"

let test_diff_nested_vectors = fun _ctx ->
  let left = make_vec [ make_vec [ 1 ] ] in
  let right = make_vec [ make_vec [ 2 ] ] in
  let diffs = [ { Diff.path = [ Diff.Index 0 ]; kind = Diff.Changed (left, right) } ] in
  match Diff.changes diffs with
  | [ { kind=Diff.Changed (l, r); _ } ] when Vector.len l = 1 && Vector.len r = 1 -> Ok ()
  | _ -> Error "Expected changed nested vector payload"

let test_diff_mixed_changes = fun _ctx ->
  let diffs = diff_vectors (make_vec [ 1; 2; 3 ]) (make_vec [ 1; 4; 5; 6 ]) in
  if List.length (Diff.changes diffs) = 2 && List.length (Diff.additions diffs) = 1 then
    Ok ()
  else
    Error "Expected mixed vector diff"

let tests =
  Test.[
    case "identical vectors" test_diff_identical_vectors;
    case "empty vectors" test_diff_empty_vectors;
    case "added elements" test_diff_added_elements;
    case "removed elements" test_diff_removed_elements;
    case "changed elements" test_diff_changed_elements;
    case "different lengths" test_diff_different_lengths;
    case "one empty" test_diff_one_empty;
    case "all different" test_diff_all_different;
    case "nested vectors" test_diff_nested_vectors;
    case "mixed changes" test_diff_mixed_changes;
  ]

let () =
  Miniriot.run ~main:(fun ~args -> Test.Cli.main ~name:"vector-diff" ~tests ~args) ~args:Env.args ()
