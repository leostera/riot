open Global
open Collections
module Bytes = IO.Bytes

type snapshot_paths = {
  approved: Path.t;
  pending: Path.t;
}

let append_path_suffix = fun path suffix ->
  Path.to_string path ^ suffix |> Path.of_string |> Result.expect ~msg:"snapshot path should stay valid UTF-8"

let is_safe_snapshot_char = function
  | 'a' .. 'z'
  | '0' .. '9'
  | '_'
  | '-' -> true
  | _ -> false

let sanitize_path_component = fun value ->
  let lower = String.lowercase_ascii value in
  let buf = Bytes.create (String.length lower) in
  let length = ref 0 in
  let push_char = fun ch ->
    Bytes.set buf !length ch;
    length := !length + 1
  in
  let push_dash = fun () ->
    if !length > 0 && Bytes.get buf (!length - 1) != '-' then
      push_char '-'
  in
  String.iter
    (fun ch ->
      if is_safe_snapshot_char ch then
        push_char ch
      else
        push_dash ())
    lower;
  let rendered = Bytes.sub_string buf 0 !length in
  let rec trim_left idx =
    if idx >= String.length rendered then
      idx
    else if rendered.[idx] = '-' then
      trim_left (idx + 1)
    else
      idx
  in
  let rec trim_right idx =
    if idx < 0 then
      idx
    else if rendered.[idx] = '-' then
      trim_right (idx - 1)
    else
      idx
  in
  let start_idx = trim_left 0 in
  let end_idx = trim_right (String.length rendered - 1) in
  let trimmed =
    if start_idx > end_idx then
      ""
    else
      String.sub rendered start_idx (end_idx - start_idx + 1)
  in
  if String.equal trimmed "" then
    "snapshot"
  else
    trimmed

let ensure_parent_dir = fun path ->
  match Path.parent path with
  | Some parent -> Fs.create_dir_all parent
  | None -> Ok ()

let write_pending_snapshot = fun path content ->
  match ensure_parent_dir path with
  | Error err -> Error err
  | Ok () -> Fs.write content path

let canonicalize_json =
  let rec loop = function
    | Data.Json.Object fields ->
        Data.Json.Object
          (fields
           |> List.map (fun (key, value) -> (key, loop value))
           |> List.sort (fun (left, _) (right, _) -> String.compare left right))
    | Data.Json.Array items ->
        Data.Json.Array (List.map loop items)
    | other ->
        other
  in
  loop

let resolve_paths = fun ~(ctx: Test_context.t) ->
  match ctx.fixture with
  | Some fixture ->
      let approved = Path.add_extension (Path.remove_extension fixture.path) ~ext:"expected" in
      Ok { approved; pending = append_path_suffix approved ".new" }
  | None -> (
      match (ctx.workspace_root, ctx.package_name) with
      | (Some workspace_root, Some package_name) ->
          let suite_dir = sanitize_path_component ctx.suite_name in
          let test_name = sanitize_path_component ctx.test_name in
          let base =
            Path.(
              workspace_root
              / Path.v ".tusk"
              / Path.v "snapshots"
              / Path.v package_name
              / Path.v suite_dir
              / Path.v test_name)
          in
          let approved = Path.add_extension base ~ext:"expected" in
          Ok { approved; pending = append_path_suffix approved ".new" }
      | (None, _) ->
          Error "Snapshot assertions require ctx.workspace_root to resolve external snapshot storage."
      | (_, None) ->
          Error "Snapshot assertions require ctx.package_name to resolve external snapshot storage."
    )

let mismatch_message = fun ~kind ~approved ~pending ~expected ~actual ->
  String.concat "\n"
    [
      kind;
      "Approved: " ^ Path.to_string approved;
      "Pending: " ^ Path.to_string pending;
      "";
      "Expected:";
      expected;
      "";
      "Actual:";
      actual;
    ]

let pending_exists_message = fun ~approved ~pending ->
  String.concat "\n"
    [
      "Snapshot has a pending candidate awaiting review.";
      "Approved: " ^ Path.to_string approved;
      "Pending: " ^ Path.to_string pending;
    ]

let assert_text = fun ~ctx ~actual ->
  match resolve_paths ~ctx with
  | Error msg -> Error msg
  | Ok paths ->
      let pending_exists = Fs.exists paths.pending |> Result.unwrap_or ~default:false in
      let approved_exists = Fs.exists paths.approved |> Result.unwrap_or ~default:false in
      if approved_exists then
        match Fs.read paths.approved with
        | Error err -> Error (IO.error_message err)
        | Ok expected ->
            if String.equal expected actual then
              if pending_exists then
                Error (pending_exists_message ~approved:paths.approved ~pending:paths.pending)
              else
                Ok ()
            else
              match write_pending_snapshot paths.pending actual with
              | Error err -> Error (IO.error_message err)
              | Ok () ->
                  Error
                    (mismatch_message
                       ~kind:"Snapshot mismatch."
                       ~approved:paths.approved
                       ~pending:paths.pending
                       ~expected
                       ~actual)
      else
        match write_pending_snapshot paths.pending actual with
        | Error err -> Error (IO.error_message err)
        | Ok () ->
            Error
              (String.concat "\n"
                 [
                   "Missing approved snapshot.";
                   "Approved: " ^ Path.to_string paths.approved;
                   "Pending: " ^ Path.to_string paths.pending;
                 ])

let assert_with = fun ~ctx ~render ~actual -> assert_text ~ctx ~actual:(render actual)

let assert_json = fun ~ctx ~actual ->
  let rendered =
    actual
    |> canonicalize_json
    |> Data.Json.to_string
  in
  assert_text ~ctx ~actual:rendered

let assert_inline_text = fun ~ctx:_ ~actual ~expected ->
  if String.equal actual expected then
    Ok ()
  else
    Error
      (String.concat "\n"
         [
           "Inline snapshot mismatch.";
           "";
           "Expected:";
           expected;
           "";
           "Actual:";
           actual;
         ])
