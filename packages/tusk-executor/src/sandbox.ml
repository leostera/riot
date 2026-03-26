open Std
open Std.Collections
open Tusk_model

type t = { dir : Path.t; workspace : Workspace.t }

let sandbox_id ~package_name =
  let nanos =
    Time.SystemTime.duration_since_epoch () |> Time.Duration.to_nanos
  in
  let hash = Crypto.hash_int64 nanos in
  let hex = Crypto.Digest.hex hash in
  let truncated_hash = String.sub hex 0 8 in
  let id = package_name ^ "-" ^ truncated_hash in
  Path.v id

let create ~workspace ~package_name =
  let sandbox_dir =
    Path.(Tusk_model.Tusk_dirs.sandbox_dir ~workspace_root:workspace.Workspace.root / sandbox_id ~package_name)
  in
  Fs.create_dir_all sandbox_dir
  |> Result.expect
       ~msg:
         ("Failed to create sandbox dir: " ^
            (Path.to_string sandbox_dir));
  { dir = sandbox_dir; workspace }

let get_dir t = t.dir

let copy_object_files ~store ~sandbox ~package ~depset =
  let _ = store in
  let _ = package in
  let depset = Tusk_planner.Dependency.transitive_closure depset in
  List.iter
    (fun dep ->
      match Fs.read_dir dep.Tusk_planner.Dependency.artifact_dir with
      | Error _ -> ()
      | Ok reader ->
          let entries = Std.Iter.MutIterator.to_list reader in
          entries
          |> List.filter (fun path ->
                 String.ends_with ~suffix:".o" (Path.to_string path))
          |> List.iter (fun abs_path ->
                 let dest = Path.(sandbox.dir / v (Path.basename abs_path)) in
                 Fs.copy ~src:abs_path ~dst:dest
                 |> Result.expect
                      ~msg:
                        ("Failed to copy .o file " ^ Path.to_string abs_path)))
    depset

let copy_inputs ~sandbox ~package ~inputs =
  List.iter
    (fun rel_path ->
      let src =
        Path.(
          sandbox.workspace.Workspace.root / package.Package.relative_path
          / rel_path)
      in
      let dest = Path.(sandbox.dir / rel_path) in
      let dest_parent = Path.dirname dest in
      Fs.create_dir_all dest_parent
      |> Result.expect
           ~msg:
             ("Failed to create parent dir: " ^
                (Path.to_string dest_parent));
      Fs.copy ~src ~dst:dest
      |> Result.expect
           ~msg:
             ("Failed to copy input " ^ Path.to_string src ^ " to " ^
                (Path.to_string dest)))
    inputs

let cleanup sandbox =
  let _ = Fs.remove_dir_all sandbox.dir in
  ()

let with_sandbox ~workspace ~package ~inputs ~depset ~store ~expected_outputs f
    =
  let sandbox = create ~workspace ~package_name:package.Package.name in
  (* No longer copy dependencies - they're accessed via -I flags pointing to cache *)
  copy_inputs ~sandbox ~package ~inputs;
  copy_object_files ~store ~sandbox ~package ~depset;
  let result = f sandbox in
  result
