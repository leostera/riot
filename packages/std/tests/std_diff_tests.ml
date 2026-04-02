open Std
open Std.Data
open Std.Collections

let test_path_component_key = Test.case "create Key path component" @@ fun _ctx ->
  let component = Diff.Key "user" in
  match component with
  | Diff.Key "user" -> Ok ()
  | _ -> Error "Failed to create Key component"

let test_path_component_index = Test.case "create Index path component" @@ fun _ctx ->
  let component = Diff.Index 5 in
  match component with
  | Diff.Index 5 -> Ok ()
  | _ -> Error "Failed to create Index component"

let test_kind_added = Test.case "create Added kind" @@ fun _ctx ->
  let kind = Diff.Added 42 in
  match kind with
  | Diff.Added 42 -> Ok ()
  | _ -> Error "Failed to create Added kind"

let test_kind_removed = Test.case "create Removed kind" @@ fun _ctx ->
  let kind = Diff.Removed "test" in
  match kind with
  | Diff.Removed "test" -> Ok ()
  | _ -> Error "Failed to create Removed kind"

let test_kind_changed = Test.case "create Changed kind" @@ fun _ctx ->
  let kind = Diff.Changed (1, 2) in
  match kind with
  | Diff.Changed (1, 2) -> Ok ()
  | _ -> Error "Failed to create Changed kind"

let test_change_empty_path = Test.case "change with empty path" @@ fun _ctx ->
  let result = { Diff.path = []; kind = Diff.Added 42 } in
  match result.path with
  | [] -> Ok ()
  | _ -> Error "Expected empty path"

let test_change_nested_path = Test.case "change with nested path" @@ fun _ctx ->
  let result = {
    Diff.path = [ Diff.Key "user"; Diff.Key "address"; Diff.Key "city" ];
    kind = Diff.Changed ("NYC", "SF")
  } in
  match result.path with
  | [Diff.Key "user";Diff.Key "address";Diff.Key "city"] -> Ok ()
  | _ -> Error "Path doesn't match"

let test_change_mixed_path = Test.case "change with mixed Key and Index path" @@ fun _ctx ->
  let result = {
    Diff.path = [ Diff.Key "users"; Diff.Index 0; Diff.Key "name" ];
    kind = Diff.Changed ("Alice", "Bob")
  } in
  match result.path with
  | [Diff.Key "users";Diff.Index 0;Diff.Key "name"] -> Ok ()
  | _ -> Error "Path doesn't match"

let test_has_changes_empty = Test.case "has_changes on empty list" @@ fun _ctx ->
  let result = Diff.has_changes [] in
  if not result then
    Ok ()
  else
    Error "Expected false for empty list"

let test_has_changes_with_changes = Test.case "has_changes with actual changes" @@ fun _ctx ->
  let changes = [ { Diff.path = []; kind = Diff.Added 1 } ] in
  let result = Diff.has_changes changes in
  if result then
    Ok ()
  else
    Error "Expected true for non-empty list"

let test_additions_filter = Test.case "additions filters only Added changes" @@ fun _ctx ->
  let changes = [
    { Diff.path = [ Diff.Key "a" ]; kind = Diff.Added 1 };
    { Diff.path = [ Diff.Key "b" ]; kind = Diff.Removed 2 };
    { Diff.path = [ Diff.Key "c" ]; kind = Diff.Changed (3, 4) };
    { Diff.path = [ Diff.Key "d" ]; kind = Diff.Added 5 };
  ] in
  let added = Diff.additions changes in
  match added with
  | [{ path=[ Diff.Key "a" ]; kind=Diff.Added 1 };{ path=[ Diff.Key "d" ]; kind=Diff.Added 5 };] -> Ok ()
  | _ -> Error ("Expected 2 specific additions, got " ^ Int.to_string (List.length added))

let test_removals_filter = Test.case "removals filters only Removed changes" @@ fun _ctx ->
  let changes = [
    { Diff.path = [ Diff.Key "a" ]; kind = Diff.Added 1 };
    { Diff.path = [ Diff.Key "b" ]; kind = Diff.Removed 2 };
    { Diff.path = [ Diff.Key "c" ]; kind = Diff.Removed 3 };
  ] in
  let removed = Diff.removals changes in
  match removed with
  | [{ path=[ Diff.Key "b" ]; kind=Diff.Removed 2 };{ path=[ Diff.Key "c" ]; kind=Diff.Removed 3 };] -> Ok ()
  | _ -> Error ("Expected 2 specific removals, got " ^ Int.to_string (List.length removed))

let test_changes_filter = Test.case "changes filters only Changed changes" @@ fun _ctx ->
  let changes = [
    { Diff.path = [ Diff.Key "a" ]; kind = Diff.Added 1 };
    { Diff.path = [ Diff.Key "b" ]; kind = Diff.Changed (2, 3) };
    { Diff.path = [ Diff.Key "c" ]; kind = Diff.Removed 4 };
    { Diff.path = [ Diff.Key "d" ]; kind = Diff.Changed (5, 6) };
  ] in
  let changed = Diff.changes changes in
  match changed with
  | [
    { path=[ Diff.Key "b" ]; kind=Diff.Changed (2, 3) };
    { path=[ Diff.Key "d" ]; kind=Diff.Changed (5, 6) };

  ] -> Ok ()
  | _ -> Error ("Expected 2 specific changes, got " ^ Int.to_string (List.length changed))

let test_at_path_exact_match = Test.case "at_path with exact match" @@ fun _ctx ->
  let diffs = [
    { Diff.path = [ Diff.Key "user"; Diff.Key "name" ]; kind = Diff.Added "Alice" };
    { Diff.path = [ Diff.Key "user"; Diff.Key "age" ]; kind = Diff.Added "30" };
    { Diff.path = [ Diff.Key "config"; Diff.Key "port" ]; kind = Diff.Added "8080" };
  ] in
  let at_user_name = Diff.at_path [ Diff.Key "user"; Diff.Key "name" ] diffs in
  match at_user_name with
  | [ { path=[Diff.Key "user";Diff.Key "name"]; kind=Diff.Added "Alice" };  ] -> Ok ()
  | _ -> Error ("Expected 1 match at user.name, got " ^ Int.to_string (List.length at_user_name))

let test_at_path_no_match = Test.case "at_path with no match" @@ fun _ctx ->
  let diffs = [ { Diff.path = [ Diff.Key "user"; Diff.Key "name" ]; kind = Diff.Added "Alice" }; ] in
  let at_config = Diff.at_path [ Diff.Key "config" ] diffs in
  if List.length at_config = 0 then
    Ok ()
  else
    Error "Expected no matches"

let () =
  Miniriot.run
    ~main:(fun ~args ->
      let all_tests = [
        test_path_component_key;
        test_path_component_index;
        test_kind_added;
        test_kind_removed;
        test_kind_changed;
        test_change_empty_path;
        test_change_nested_path;
        test_change_mixed_path;
        test_has_changes_empty;
        test_has_changes_with_changes;
        test_additions_filter;
        test_removals_filter;
        test_changes_filter;
        test_at_path_exact_match;
        test_at_path_no_match;
      ]
      in
      Test.Cli.main ~name:"diff" ~tests:all_tests ~args)
    ~args:Env.args
    ()
