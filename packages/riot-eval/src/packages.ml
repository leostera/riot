open Std
open Std.Result.Syntax

let artifact_dirs = fun store build_result ->
  Riot_build.Build_result.packages build_result
  |> Std.List.flat_map
    ~fn:(fun package_result ->
      let artifacts =
        Std.Option.to_list (Riot_build.Build_result.package_artifact package_result)
        @ Riot_build.Build_result.package_artifacts package_result
      in
      artifacts
      |> Std.List.map ~fn:(Riot_store.Store.get_artifact_dir store))
  |> Context.unique_paths

let manifest_name = fun (manifest: Riot_model.Package_manifest.t) -> manifest.name

let package_result_name = Riot_build.Build_result.package_name

let package_name_in = fun package_name package_names ->
  Std.List.any package_names ~fn:(Riot_model.Package_name.equal package_name)

let new_package_names = fun ~previous requested ->
  requested
  |> Std.List.filter ~fn:(fun package_name ->
    not (package_name_in package_name previous))

let find_manifest = fun workspace package_name ->
  Std.List.find
    workspace.Riot_model.Workspace.packages
    ~fn:(fun manifest -> Riot_model.Package_name.equal (manifest_name manifest) package_name)

let sort_package_results = fun workspace package_results ->
  let find_result package_name =
    Std.List.find
      package_results
      ~fn:(fun result ->
        Riot_model.Package_name.equal (package_result_name result) package_name)
  in
  let rec visit visited acc package_name =
    if Std.List.any visited ~fn:(Riot_model.Package_name.equal package_name) then
      (visited, acc)
    else
      let visited = package_name :: visited in
      let dependencies =
        match find_manifest workspace package_name with
        | None -> []
        | Some manifest ->
            Riot_model.Package_manifest.all_dependencies manifest
            |> Std.List.map ~fn:(fun (dependency: Riot_model.Package_manifest.dependency) ->
              dependency.name)
            |> Std.List.filter ~fn:(fun dependency_name ->
              Std.Option.is_some (find_result dependency_name))
      in
      let (visited, acc) =
        Std.List.fold_left
          dependencies
          ~init:(visited, acc)
          ~fn:(fun (visited, acc) dependency_name ->
            visit visited acc dependency_name)
      in
      match find_result package_name with
      | None -> (visited, acc)
      | Some result -> (visited, acc @ [ result ])
  in
  package_results
  |> Std.List.fold_left
    ~init:([], [])
    ~fn:(fun (visited, acc) result ->
      visit visited acc (package_result_name result))
  |> fun (_visited, sorted) -> sorted

let public_root_of_package_name = fun package_name ->
  Riot_model.Module_name.(
    from_string (Riot_model.Package_name.to_string package_name)
    |> to_string)

let cmxa_entry = fun (entry: Riot_store.Manifest.export_entry) ->
  Std.String.ends_with ~suffix:".cmxa" entry.name

let compiled_root_of_archive = fun archive ->
  let basename = Std.Path.basename archive in
  let suffix = ".cmxa" in
  if Std.String.ends_with ~suffix basename then
    Std.String.sub basename ~offset:0 ~len:(Std.String.length basename - Std.String.length suffix)
  else
    basename

let archive_path_of_export_entry = fun store artifact (entry: Riot_store.Manifest.export_entry) ->
  let path = Std.Path.(Riot_store.Store.get_artifact_dir store artifact / Std.Path.v entry.name) in
  match Std.Fs.exists path with
  | Ok true -> Some path
  | Ok false
  | Error _ -> None

let first_some = fun fn items ->
  let rec loop = fun __tmp1 ->
    match __tmp1 with
    | [] -> None
    | item :: rest -> (
        match fn item with
        | Some _ as value -> value
        | None -> loop rest
      )
  in
  loop items

let archive_path_of_artifact = fun store artifact ->
  artifact.Riot_store.Artifact.exports
  |> first_some
    (fun entry ->
      if cmxa_entry entry then
        archive_path_of_export_entry store artifact entry
      else
        None)

let archive_path_of_package_result = fun store package_result ->
  Riot_build.Build_result.package_artifacts package_result
  |> first_some (archive_path_of_artifact store)

let library_archives = fun workspace store build_result ->
  let libraries =
    Riot_build.Build_result.packages build_result
    |> sort_package_results workspace
    |> Std.List.filter_map
    ~fn:(fun package_result ->
      let package_name = Riot_build.Build_result.package_name package_result in
      archive_path_of_package_result store package_result
      |> Std.Option.map
        ~fn:(fun archive -> Context.{
          package_name;
          public_root = public_root_of_package_name package_name;
          compiled_root = compiled_root_of_archive archive;
          archive;
        }))
  in
  let take package_name =
    Std.List.filter
      libraries
      ~fn:(fun (library: Context.library_archive) ->
        Riot_model.Package_name.equal library.package_name package_name)
  in
  let runtime = take Context.kernel_package_name @ take Context.std_package_name in
  let runtime_package package_name =
    Riot_model.Package_name.equal package_name Context.kernel_package_name
    || Riot_model.Package_name.equal package_name Context.std_package_name
  in
  runtime
  @ Std.List.filter
    libraries
    ~fn:(fun (library: Context.library_archive) ->
      not (runtime_package library.package_name))

let is_detached = fun (session: Context.session) ->
  Std.Option.is_some session.detached

let ready_message = fun session package_count ->
  let label =
    if is_detached session then
      "runtime ready"
    else
      "workspace ready"
  in
  label ^ ": " ^ Std.Int.to_string package_count ^ " packages"

let build_packages = fun (session: Context.session) ~package_names ->
  let build_package_names = Context.package_request_for_session session ~package_names in
  Riot_build.build
    ~on_event:session.on_event
    (
      Riot_build.Request.make
        ~workspace:session.workspace
        ~packages:build_package_names
        ~targets:Riot_model.Target.Host
        ~scope:Riot_build.Request.Runtime
        ~profile:Context.profile
        ()
    )
  |> Std.Result.map_err ~fn:Riot_build.error_message

let protect = fun label fn ->
  try fn () with exn -> Error (label ^ ": " ^ Std.Exception.to_string exn)

let session_includes = fun (session: Context.session) includes ~merge ->
  let includes = Context.base_includes session.session_dir (includes @ session.host_includes) in
  if merge then
    Context.unique_paths (includes @ session.includes)
  else
    includes

let refresh = fun ?(merge_includes = false) (session: Context.session) ~package_names ~load ->
  let* () = Context.ensure_session_packages session ~packages:package_names in
  let* build_result =
    protect "package build for eval raised" (fun () -> build_packages session ~package_names)
  in
  let store =
    Riot_store.Store.create_for_lane
      ~workspace:session.workspace
      ~profile:Context.profile.name
      ~target:Context.host_target
  in
  let includes = artifact_dirs store build_result in
  session.includes <- session_includes session includes ~merge:merge_includes;
  let package_count =
    Riot_build.Build_result.packages build_result
    |> Std.List.length
  in
  let* () =
    if load then
      let* libraries =
        protect "collecting libraries for eval raised" (fun () ->
          Ok (library_archives session.workspace store build_result))
      in
      let* (loaded, skipped) = Loader.load_library_archives session libraries in
      eprintln
        (
          "loaded "
          ^ Std.Int.to_string loaded
          ^ " libraries"
          ^ (
            if skipped = 0 then
              ""
            else
              ", " ^ Std.Int.to_string skipped ^ " already present"
          )
        );
      Ok ()
    else
      Ok ()
  in
  eprintln (ready_message session package_count);
  Ok ()

let refresh_workspace = fun session ->
  refresh session ~package_names:[] ~load:false

let refresh_eval = fun session ~packages ->
  let package_names = Context.unique_package_names (Context.std_package_name :: packages) in
  refresh session ~package_names ~load:(not (Std.List.is_empty packages)) ~merge_includes:true

let load = fun (session: Context.session) ~packages ->
  let previous = session.requested_packages in
  let requested = Context.unique_package_names (previous @ packages) in
  let newly_requested = new_package_names ~previous requested in
  session.requested_packages <- requested;
  if Std.List.is_empty newly_requested then
    Ok ()
  else
    refresh_eval session ~packages:requested
