open Std
open Collections

type pending_snapshot = {
  approved: Path.t;
  pending: Path.t;
}

let command =
  let open ArgParser in
  let open Arg in
  let make_subcommand = fun name about_text ->
    command name
    |> about about_text
    |> args
      [
        positional "query"
        |> required false
        |> help "Optional path substring used to filter pending snapshots";
      ]
  in
  command "snapshots"
  |> about "Review and manage pending snapshot candidates"
  |> subcommands
    [
      make_subcommand "review" "Show pending snapshot diffs";
      make_subcommand "approve" "Promote pending snapshots to approved files";
      make_subcommand "reject" "Delete pending snapshot candidates";
    ]

let pending_suffix = ".expected.new"

let ensure_trailing_new_removed = fun path ->
  let rendered = Path.to_string path in
  if String.ends_with ~suffix:".new" rendered then
    let len = String.length rendered - 4 in
    String.sub rendered 0 len
    |> Path.of_string
    |> Result.expect ~msg:"pending snapshot path should stay valid UTF-8"
  else
    path

let is_pending_snapshot = fun path ->
  Path.basename path
  |> String.ends_with ~suffix:pending_suffix

let should_skip_dir = fun path ->
  match Path.basename path with
  | "_build"
  | ".git" -> true
  | _ -> false

let display_path = fun ~workspace_root path ->
  match Path.strip_prefix path ~prefix:workspace_root with
  | Ok rel -> "./" ^ Path.to_string rel
  | Error _ -> Path.to_string path

let matches_query = fun ?query snapshot ->
  match query with
  | None -> true
  | Some query ->
      String.contains (Path.to_string snapshot.pending) query
      || String.contains (Path.to_string snapshot.approved) query

let discover_pending_snapshots = fun ~workspace_root ?query () ->
  let rec visit = fun path ->
    match Fs.is_dir path with
    | Error err -> Error err
    | Ok true ->
        if should_skip_dir path then
          Ok []
        else (
          match Fs.read_dir path with
          | Error err -> Error err
          | Ok entries ->
              let children = Iter.MutIterator.to_list entries in
              let rec loop acc = function
                | [] -> Ok acc
                | child :: rest -> (
                    let child_path = Path.join path child in
                    match visit child_path with
                    | Error err -> Error err
                    | Ok found -> loop (List.rev_append found acc) rest
                  )
              in
              loop [] children
        )
    | Ok false ->
        if is_pending_snapshot path then
          Ok [ { approved = ensure_trailing_new_removed path; pending = path } ]
        else
          Ok []
  in
  match visit workspace_root with
  | Error err -> Error err
  | Ok snapshots ->
      Ok
        (snapshots
         |> List.filter (matches_query ?query)
         |> List.sort (fun left right ->
           String.compare (Path.to_string left.pending) (Path.to_string right.pending)))

let ensure_parent_dir = fun path ->
  match Path.parent path with
  | Some parent -> Fs.create_dir_all parent
  | None -> Ok ()

let approve_pending_snapshot = fun snapshot ->
  match Fs.read snapshot.pending with
  | Error err -> Error err
  | Ok content -> (
      match ensure_parent_dir snapshot.approved with
      | Error err -> Error err
      | Ok () -> (
          match Fs.write content snapshot.approved with
          | Error err -> Error err
          | Ok () -> Fs.remove_file snapshot.pending
        )
    )

let approve_pending_snapshots = fun snapshots ->
  let rec loop = function
    | [] -> Ok ()
    | snapshot :: rest -> (
        match approve_pending_snapshot snapshot with
        | Ok () -> loop rest
        | Error err -> Error err
      )
  in
  loop snapshots

let reject_pending_snapshots = fun snapshots ->
  let rec loop = function
    | [] -> Ok ()
    | snapshot :: rest -> (
        match Fs.remove_file snapshot.pending with
        | Ok () -> loop rest
        | Error err -> Error err
      )
  in
  loop snapshots

let print_blob = fun value ->
  print value;
  if not (String.ends_with ~suffix:"\n" value) then
    print "\n"

let review_pending_snapshot = fun ~workspace_root snapshot ->
  println "";
  println ("Approved: " ^ display_path ~workspace_root snapshot.approved);
  println ("Pending:  " ^ display_path ~workspace_root snapshot.pending);
  match Fs.exists snapshot.approved with
  | Error err -> Error err
  | Ok false -> (
      match Fs.read snapshot.pending with
      | Error err -> Error err
      | Ok content ->
          println "Missing approved snapshot.";
          println "--- pending";
          print_blob content;
          Ok ()
    )
  | Ok true -> (
      let diff_cmd =
        Command.make "git"
          ~cwd:(Path.to_string workspace_root)
          ~args:
            [
              "diff";
              "--no-index";
              "--color=always";
              "--";
              Path.to_string snapshot.approved;
              Path.to_string snapshot.pending;
            ]
      in
      match Command.output diff_cmd with
      | Error (Command.SystemError msg) -> Error (IO.Unknown_error msg)
      | Ok { stdout; stderr; status } ->
          if status = 0 || status = 1 then
            (
              if not (String.equal stdout "") then
                print_blob stdout;
              if not (String.equal stderr "") then
                eprintln stderr;
              Ok ()
            )
          else
            Error
              (IO.Unknown_error
                 ("git diff failed for "
                 ^ Path.to_string snapshot.pending
                 ^ " with status "
                 ^ Int.to_string status))
    )

let review_pending_snapshots = fun ~workspace_root snapshots ->
  let rec loop = function
    | [] -> Ok ()
    | snapshot :: rest -> (
        match review_pending_snapshot ~workspace_root snapshot with
        | Ok () -> loop rest
        | Error err -> Error err
      )
  in
  loop snapshots

let print_no_pending = fun () ->
  println "No pending snapshots."

let print_review_summary = fun count ->
  println ("Found " ^ Int.to_string count ^ " pending snapshot(s).")

let print_approved = fun ~workspace_root snapshot ->
  println ("Approved " ^ display_path ~workspace_root snapshot.approved)

let print_rejected = fun ~workspace_root snapshot ->
  println ("Rejected " ^ display_path ~workspace_root snapshot.pending)

let run_action = fun ~workspace_root ?query action ->
  match discover_pending_snapshots ~workspace_root ?query () with
  | Error err -> Error (Failure (IO.error_message err))
  | Ok [] ->
      print_no_pending ();
      Ok ()
  | Ok snapshots -> (
      match action with
      | `Review ->
          print_review_summary (List.length snapshots);
          (
            match review_pending_snapshots ~workspace_root snapshots with
            | Ok () -> Ok ()
            | Error err -> Error (Failure (IO.error_message err))
          )
      | `Approve ->
          (
            match approve_pending_snapshots snapshots with
            | Error err -> Error (Failure (IO.error_message err))
            | Ok () ->
                List.iter (print_approved ~workspace_root) snapshots;
                Ok ()
          )
      | `Reject ->
          (
            match reject_pending_snapshots snapshots with
            | Error err -> Error (Failure (IO.error_message err))
            | Ok () ->
                List.iter (print_rejected ~workspace_root) snapshots;
                Ok ()
          )
    )

let run = fun ~(workspace: Tusk_model.Workspace.t) matches ->
  let open ArgParser in
  match get_subcommand matches with
  | Some ("review", sub_matches) ->
      run_action ~workspace_root:workspace.root ?query:(get_one sub_matches "query") `Review
  | Some ("approve", sub_matches) ->
      run_action ~workspace_root:workspace.root ?query:(get_one sub_matches "query") `Approve
  | Some ("reject", sub_matches) ->
      run_action ~workspace_root:workspace.root ?query:(get_one sub_matches "query") `Reject
  | _ ->
      Error (Failure "Unknown snapshots subcommand")
