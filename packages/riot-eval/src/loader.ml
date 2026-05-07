open Std
open Std.Result.Syntax

let already_loaded_error = fun message ->
  Std.String.contains message "already" && Std.String.contains message "loaded"

let load_cmxs = fun cmxs_path ->
  try
    Dynlink.loadfile (Std.Path.to_string cmxs_path);
    Ok `Loaded
  with
  | exn ->
      let message =
        match exn with
        | Dynlink.Error error -> Dynlink.error_message error
        | _ -> Std.Exception.to_string exn
      in
      if already_loaded_error message then
        Ok (`Skipped message)
      else
        Error message

let library_set_key = fun libraries ->
  libraries
  |> Std.List.map
    ~fn:(fun (library: Context.library_archive) ->
      Riot_model.Package_name.to_string library.package_name
      ^ ":"
      ^ Std.Path.to_string library.archive)
  |> Std.String.concat "\n"
  |> Std.Crypto.hash_string
  |> Std.Crypto.Digest.hex
  |> fun hash ->
    if Std.String.length hash <= 16 then
      hash
    else
      Std.String.sub hash ~offset:0 ~len:16

let library_plugin_path = fun (session: Context.session) libraries ->
  Std.Path.(session.session_dir
  / Std.Path.v (session.module_prefix ^ "_libs_" ^ library_set_key libraries ^ ".cmxs"))

let library_object_dir = fun (session: Context.session) libraries ->
  Std.Path.(session.session_dir
  / Std.Path.v (session.module_prefix ^ "_objs_" ^ library_set_key libraries))

let unique_strings = fun values -> Std.List.unique values ~compare:Std.String.compare

let library_key = fun (library: Context.library_archive) ->
  Riot_model.Package_name.to_string library.package_name
  ^ ":"
  ^ library.compiled_root
  ^ ":"
  ^ Std.Path.to_string library.archive

let fresh_libraries = fun (session: Context.session) libraries ->
  Std.List.filter
    libraries
    ~fn:(fun (library: Context.library_archive) ->
      let key = library_key library in
      not (Std.List.any session.loaded_libraries ~fn:(Std.String.equal key)))

let loadable_libraries = fun libraries ->
  Std.List.filter
    libraries
    ~fn:(fun (library: Context.library_archive) ->
      not
        (Context.is_runtime_package_name library.package_name))

let remember_alias = fun aliases (library: Context.library_archive) ->
  let aliases =
    Std.List.filter
      aliases
      ~fn:(fun (alias: Context.package_alias) ->
        not
          (Std.String.equal alias.public_root library.public_root))
  in
  aliases @ [ Context.{ public_root = library.public_root; compiled_root = library.compiled_root } ]

let remember_libraries = fun (session: Context.session) libraries ->
  let loaded = session.loaded_libraries @ Std.List.map libraries ~fn:library_key in
  session.loaded_libraries <- unique_strings loaded;
  session.package_aliases <- Std.List.fold_left
    libraries
    ~init:session.package_aliases
    ~fn:remember_alias

let object_files_in_dir = fun dir ->
  match Std.Fs.read_dir dir with
  | Error _ -> []
  | Ok entries ->
      entries
      |> Std.Iter.MutIterator.to_list
      |> Std.List.map ~fn:(Std.Path.join dir)
      |> Std.List.filter
        ~fn:(fun path -> Std.String.ends_with ~suffix:".o" (Std.Path.basename path))

let stage_object_file = fun ~link_dir object_file ->
  let dst = Std.Path.(link_dir / Std.Path.v (Std.Path.basename object_file)) in
  match Std.Fs.exists dst with
  | Ok true -> Ok ()
  | Ok false ->
      Std.Fs.copy ~src:object_file ~dst
      |> Std.Result.map_err
        ~fn:(fun error ->
          "failed to stage native object "
          ^ Std.Path.to_string object_file
          ^ ": "
          ^ Std.IO.error_message error)
  | Error error ->
      Error ("failed to check staged native object "
      ^ Std.Path.to_string dst
      ^ ": "
      ^ Std.IO.error_message error)

let stage_library_objects = fun session libraries ->
  let link_dir = library_object_dir session libraries in
  let* () =
    Std.Fs.create_dir_all link_dir
    |> Std.Result.map_err
      ~fn:(fun error ->
        "failed to create eval library link directory: " ^ Std.IO.error_message error)
  in
  let object_files =
    libraries
    |> Std.List.filter_map
      ~fn:(fun (library: Context.library_archive) -> Std.Path.parent library.archive)
    |> Context.unique_paths
    |> Std.List.flat_map ~fn:object_files_in_dir
  in
  let* () =
    Std.List.fold_left
      object_files
      ~init:(Ok ())
      ~fn:(fun acc object_file ->
        let* () = acc in
        stage_object_file ~link_dir object_file)
  in
  Ok link_dir

let load_library_archives = fun session libraries ->
  let loadable = loadable_libraries libraries in
  let fresh = fresh_libraries session loadable in
  let skipped = Std.List.length libraries - Std.List.length fresh in
  if Std.List.is_empty fresh then
    Ok (0, skipped)
  else
    let plugin_path = library_plugin_path session fresh in
    let libs = Std.List.map fresh ~fn:(fun (library: Context.library_archive) -> library.archive) in
    let* link_dir = stage_library_objects session fresh in
    match Compile.link_shared_library ~cwd:link_dir session ~output:plugin_path ~libs [] with
    | Riot_toolchain.Ocamlc.Success _ -> (
        match load_cmxs plugin_path with
        | Ok `Loaded ->
            remember_libraries session fresh;
            Ok (Std.List.length fresh, skipped)
        | Ok (`Skipped _) -> Ok (0, skipped + Std.List.length fresh)
        | Error message -> Error ("failed to load libraries for eval:\n" ^ message)
      )
    | Riot_toolchain.Ocamlc.Failed _ as result ->
        Error ("failed to link libraries for eval:\n" ^ Compile.compiler_output result)
