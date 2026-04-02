open Std
module Test = Std.Test

let make_warning = fun path ->
  String.concat
    "\n"
    [
      "File \"" ^ path ^ "\", line 25, characters 8-26:";
      "25 |     let displayed_packages = HashMap.create () in";
      "             ^^^^^^^^^^^^^^^^^^";
      "Warning 26 [unused-var]: unused variable displayed_packages.";
    ]

let make_package = fun ~root ~name ->
  let path = Path.(root / Path.v "packages" / Path.v name) in
  Tusk_model.Package.{
    name;
    path;
    relative_path = Path.v ("packages/" ^ name);
    dependencies = [];
    dev_dependencies = [];
    build_dependencies = [];
    foreign_dependencies = [];
    binaries = [];
    library = Some { path = Path.v "src/lib.ml" };
    sources =
      {
        src = [];
        native = [];
        tests = [];
        examples = [];
        bench = [];
      };
    compiler = { profile_overrides = []; target_overrides = [] };
    commands = [];
    fix_providers = [];
    publish = { version = None; description = None; license = None; is_public = None };
  }

let test_rewrite_ocamlc_result_rewrites_workspace_paths = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"diagnostic_rewrite_test"
      (fun tmpdir ->
        let package = make_package ~root:tmpdir ~name:"tusk-cli" in
        let package_src = Path.(package.path / Path.v "src") in
        let sandbox_dir =
          Path.(tmpdir / Path.v "_build" / Path.v "sandbox" / Path.v "tusk-cli-a9441944") in
        let _ = Fs.create_dir_all package_src |> Result.expect ~msg:"failed to create package src" in
        let _ = Fs.write
          "let displayed_packages = HashMap.create ()"
          Path.(package_src / Path.v "install.ml")
        |> Result.expect ~msg:"failed to write package source" in
        let diagnostic = make_warning
          (Path.to_string Path.(sandbox_dir / Path.v "src" / Path.v "install.ml"))
        |> Tusk_toolchain.Ocamlc.Diagnostic.parse
        |> List.hd in
        let result = Tusk_toolchain.Ocamlc.Success { message = ""; diagnostics = [ diagnostic ] } in
        let rewritten = Tusk_executor.Diagnostic_rewrite.rewrite_ocamlc_result
          ~package
          ~sandbox_dir
          result in
        let warnings = Tusk_toolchain.Ocamlc.get_ocamlc_warnings rewritten in
        match warnings with
        | [ warning ] when String.contains warning "./packages/tusk-cli/src/install.ml" -> Ok ()
        | [ warning ] -> Error ("expected rewritten workspace path, got: " ^ warning)
        | _ -> Error "expected exactly one rewritten warning")
  with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_rewrite_ocamlc_result_leaves_generated_paths_alone = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"diagnostic_rewrite_generated_test"
      (fun tmpdir ->
        let package = make_package ~root:tmpdir ~name:"tusk-cli" in
        let sandbox_dir =
          Path.(tmpdir / Path.v "_build" / Path.v "sandbox" / Path.v "tusk-cli-a9441944") in
        let diagnostic = make_warning
          (Path.to_string Path.(sandbox_dir / Path.v "Tusk_cli__Aliases.ml.gen"))
        |> Tusk_toolchain.Ocamlc.Diagnostic.parse
        |> List.hd in
        let rewritten = Tusk_executor.Diagnostic_rewrite.rewrite_ocamlc_result
          ~package
          ~sandbox_dir
          (Tusk_toolchain.Ocamlc.Failed {
            message = "Command failed with status 2";
            diagnostics = [ diagnostic ]
          }) in
        let rendered = Tusk_toolchain.Ocamlc.get_output rewritten in
        if
          String.contains
            rendered
            (Path.to_string Path.(sandbox_dir / Path.v "Tusk_cli__Aliases.ml.gen"))
        then
          Ok ()
        else
          Error ("expected generated sandbox path to stay unchanged, got: " ^ rendered))
  with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let name = "diagnostic_rewrite_tests"

let tests =
  Test.[
    case "rewrite ocamlc result rewrites workspace paths" test_rewrite_ocamlc_result_rewrites_workspace_paths;
    case "rewrite ocamlc result leaves generated paths alone" test_rewrite_ocamlc_result_leaves_generated_paths_alone;
  ]

let () = Miniriot.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
