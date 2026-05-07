open Std
open Std.Result.Syntax

type plan = {
  hash: string;
  package_name: Riot_model.Package_name.t;
  binary_name: string;
  generated_dir: Std.Path.t;
  package_dir: Std.Path.t;
  src_dir: Std.Path.t;
  main_path: Std.Path.t;
  binary_path: Std.Path.t;
  package: Riot_model.Package.t;
  args: string list;
}

type source_kind =
  | Eval_expression
  | Script_file

let generator_version = "v4"

let short_hash = fun hash ->
  if Std.String.length hash <= 16 then
    hash
  else
    Std.String.sub hash ~offset:0 ~len:16

let package_name = fun hash ->
  let name = "eval-runner-" ^ short_hash hash in
  Riot_model.Package_name.from_string name
  |> Std.Result.expect ~msg:("expected generated eval package name to be valid: " ^ name)

let relative_path_for_package = fun ~workspace_root package_dir ->
  match Std.Path.strip_prefix package_dir ~prefix:workspace_root with
  | Ok relative -> relative
  | Error _ -> package_dir

let bool_key = fun value ->
  if value then
    "true"
  else
    "false"

let dependency_key = fun (dependency: Riot_model.Package.dependency) ->
  let source = dependency.source in
  Std.String.concat
    ":"
    [
      Riot_model.Package_name.to_string dependency.name;
      bool_key source.workspace;
      bool_key source.builtin;
      (
        match source.path with
        | Some path -> Std.Path.to_string path
        | None -> ""
      );
      Std.Option.unwrap_or ~default:"" source.source_locator;
      Std.Option.unwrap_or ~default:"" source.ref_;
      (
        match source.version with
        | Some requirement -> Std.Version.requirement_to_string requirement
        | None -> ""
      );
    ]

let source_kind_key = fun __tmp1 ->
  match __tmp1 with
  | Eval_expression -> "eval-expression"
  | Script_file -> "script-file"

let hash_input = fun ~source_kind ~source ~dependencies ~args ~include_args ->
  Std.String.concat
    "\n"
    (
      [ generator_version; source_kind_key source_kind; source; ]
      @ (
        dependencies
        |> Std.List.map ~fn:dependency_key
        |> Std.List.sort ~compare:Std.String.compare
      )
      @ (
        if include_args then
          args
        else
          []
      )
    )
  |> Std.Crypto.hash_string
  |> Std.Crypto.Digest.hex

let find_package = fun workspace package_name ->
  Std.List.find
    workspace.Riot_model.Workspace.packages
    ~fn:(fun (manifest: Riot_model.Package_manifest.t) ->
      Riot_model.Package_name.equal manifest.name package_name)

let dependency_for_package = fun workspace package_name ->
  match find_package workspace package_name with
  | None ->
      Error ("package not found: " ^ Riot_model.Package_name.to_string package_name)
  | Some manifest ->
      Ok Riot_model.Package.{
        name = package_name;
        source =
          {
            workspace = false;
            builtin = false;
            path = Some manifest.path;
            source_locator = None;
            ref_ = None;
            version = Some Std.Version.any;
          };
      }

let package_dependencies = fun workspace packages ->
  packages
  |> Std.List.fold_left
    ~init:(Ok [])
    ~fn:(fun acc package_name ->
      let* acc = acc in
      let* dependency = dependency_for_package workspace package_name in
      Ok (acc @ [ dependency ]))

let dependency_name_in = fun package_name dependencies ->
  Std.List.any dependencies ~fn:(fun (dependency: Riot_model.Package.dependency) ->
    Riot_model.Package_name.equal dependency.name package_name)

let workspace_member_dependency = fun (manifest: Riot_model.Package_manifest.t) ->
  Riot_model.Package.{
    name = manifest.name;
    source = {
      workspace = true;
      builtin = false;
      path = None;
      source_locator = None;
      ref_ = None;
      version = None;
    };
  }

let inherited_workspace_dependencies = fun workspace explicit_dependencies ->
  workspace.Riot_model.Workspace.packages
  |> Std.List.filter ~fn:Riot_model.Package_manifest.is_workspace_member
  |> Std.List.filter ~fn:(fun (manifest: Riot_model.Package_manifest.t) ->
    not (dependency_name_in manifest.name explicit_dependencies))
  |> Std.List.map ~fn:workspace_member_dependency

let strip_final_phrase_delimiters = fun source ->
  let rec loop source =
    let source = Std.String.trim source in
    if Std.String.ends_with ~suffix:";;" source then
      loop (Std.String.sub source ~offset:0 ~len:(Std.String.length source - 2))
    else
      source
  in
  loop source

let source_slice = fun source ->
  Std.IO.IoVec.IoSlice.from_string source
  |> Std.Result.expect ~msg:"failed to create eval source slice"

let span_text = fun source (span: Syn.Span.t) ->
  Std.String.sub source ~offset:span.start ~len:(span.end_ - span.start)

let structure_items = fun source ->
  let result = Syn.parse_implementation (source_slice source) in
  let source_file = Syn.Ast.SourceFile.make result.Syn.Parser.tree in
  match Syn.Ast.SourceFile.view source_file with
  | Syn.Ast.SourceFile.Interface _ -> []
  | Syn.Ast.SourceFile.Implementation implementation ->
      Syn.Ast.Implementation.fold_item
        implementation
        ~init:[]
        ~fn:(fun item items -> Syn.Ast.Continue (items @ [ item ]))

let script_structure_item_source = fun source item ->
  let text = span_text source (Syn.Ast.StructureItem.span item) in
  match Syn.Ast.StructureItem.view item with
  | Syn.Ast.StructureItem.Expr _ ->
      "let _ = (" ^ strip_final_phrase_delimiters text ^ ")"
  | _ -> text

let script_module_body = fun source ->
  structure_items source
  |> Std.List.map ~fn:(script_structure_item_source source)
  |> Std.String.concat "\n"

let expression_main_source = fun body ->
  Std.String.concat
    "\n"
    [
      "open Std;;";
      "";
      "let main ~args =";
      "  let _ = args in";
      (
        if Std.String.equal (Std.String.trim body) "" then
          "  Ok ()"
        else
          Std.String.concat
            "\n"
            [
              "  let _ = (";
              body;
              "  ) in";
              "  Ok ()";
            ]
      );
      "";
      "let () =";
      "  let args =";
      "    match Std.Env.args with";
      "    | _ :: args -> args";
      "    | [] -> []";
      "  in";
      "  Std.Runtime.run ~main ~args ();;";
      "";
    ]

let script_main_source = fun body ->
  let body = script_module_body body in
  Std.String.concat
    "\n"
    [
      "let main ~args =";
      "  let module Script = struct";
      "  let args = args";
      body;
      "  end in";
      "  Std.Result.ok ()";
      "";
      "let () =";
      "  let args =";
      "    match Std.Env.args with";
      "    | _ :: args -> args";
      "    | [] -> []";
      "  in";
      "  Std.Runtime.run ~main ~args ();;";
      "";
    ]

let main_source = fun source_kind source ->
  match source_kind with
  | Eval_expression ->
      let body = strip_final_phrase_delimiters source in
      expression_main_source body
  | Script_file -> script_main_source source

let make = fun
  workspace
  ~source_kind
  ~dependencies
  ~source
  ~args
  ~include_args_in_hash ->
  let hash = hash_input ~source_kind ~source ~dependencies ~args ~include_args:include_args_in_hash in
  let package_name = package_name hash in
  let binary_name = "eval-runner" in
  let generated_base =
    match Std.Path.strip_prefix workspace.Riot_model.Workspace.target_dir_root ~prefix:workspace.root with
    | Ok _ -> workspace.target_dir_root
    | Error _ -> workspace.root
  in
  let generated_dir =
    Std.Path.(generated_base / Std.Path.v "eval" / Std.Path.v "runners" / Std.Path.v hash)
  in
  let package_dir = Std.Path.(generated_dir / Std.Path.v "package") in
  let src_dir = Std.Path.(package_dir / Std.Path.v "src") in
  let main_path = Std.Path.(src_dir / Std.Path.v "main.ml") in
  let binary_path =
    Std.Path.(workspace.target_dir_root
    / Std.Path.v Context.profile.name
    / Std.Path.v (Riot_model.Target.to_string Context.host_target)
    / Std.Path.v "out"
    / Std.Path.v (Riot_model.Package_name.to_string package_name)
    / Std.Path.v binary_name)
  in
  let package =
    Riot_model.Package.make
      ~name:package_name
      ~path:package_dir
      ~relative_path:(relative_path_for_package ~workspace_root:workspace.root package_dir)
      ~dependencies
      ~binaries:[ Riot_model.Package.{ name = binary_name; path = Std.Path.v "src/main.ml" } ]
      ~sources:Riot_model.Package.{
        src = [ Std.Path.v "src/main.ml" ];
        native = [];
        tests = [];
        examples = [];
        bench = [];
      }
      ()
  in
  Ok {
    hash;
    package_name;
    binary_name;
    generated_dir;
    package_dir;
    src_dir;
    main_path;
    binary_path;
    package;
    args;
  }

let attach_to_workspace = fun workspace plan ->
  let packages =
    workspace.Riot_model.Workspace.packages
    |> Std.List.filter
      ~fn:(fun (manifest: Riot_model.Package_manifest.t) ->
        not (Riot_model.Package_name.equal manifest.name plan.package_name))
  in
  {
    workspace with
    packages = packages @ [ Riot_model.Package_manifest.from_package plan.package ];
  }

let materialize = fun plan ~source_kind ~source ->
  let* () =
    Std.Fs.create_dir_all plan.src_dir
    |> Std.Result.map_err ~fn:(Context.fs_error_message "failed to create eval runner directory")
  in
  Std.Fs.write (main_source source_kind source) plan.main_path
  |> Std.Result.map_err ~fn:(Context.fs_error_message "failed to write eval runner source")

let build = fun ~on_event workspace plan ->
  Riot_build.build
    ~on_event
    (
      Riot_build.Request.make
        ~workspace:(attach_to_workspace workspace plan)
        ~packages:[ plan.package_name ]
        ~targets:Riot_model.Target.Host
        ~scope:Riot_build.Request.Runtime
        ~profile:Context.profile
        ()
    )
  |> Std.Result.map_err ~fn:Riot_build.error_message
  |> Std.Result.map ~fn:(fun _ -> ())

let run_binary = fun plan ->
  let command =
    Std.Command.make
      (Std.Path.to_string plan.binary_path)
      ~cwd:(Std.Path.to_string plan.package_dir)
      ~args:plan.args
  in
  match Std.Command.status command with
  | Ok 0 -> Ok ()
  | Ok status -> Error ("eval exited with status " ^ Std.Int.to_string status)
  | Error (Std.Command.SystemError message) -> Error message

let run = fun ~on_event workspace ~packages ~source ~args ->
  let package_names = Context.unique_package_names (Context.std_package_name :: packages) in
  let* dependencies = package_dependencies workspace package_names in
  let* plan =
    make
      workspace
      ~source_kind:Eval_expression
      ~dependencies
      ~source
      ~args
      ~include_args_in_hash:true
  in
  let* () = materialize plan ~source_kind:Eval_expression ~source in
  let* () = build ~on_event workspace plan in
  run_binary plan

let run_script = fun ~on_event workspace ~inherit_workspace ~dependencies ~source ~args ->
  let dependencies =
    if inherit_workspace then
      inherited_workspace_dependencies workspace dependencies @ dependencies
    else
      dependencies
  in
  let* plan =
    make
      workspace
      ~source_kind:Script_file
      ~dependencies
      ~source
      ~args
      ~include_args_in_hash:false
  in
  let* () = materialize plan ~source_kind:Script_file ~source in
  let* () = build ~on_event workspace plan in
  run_binary plan
