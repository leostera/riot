open Std
open Std.Collections
module Test = Std.Test

let test_toolchain = Riot_toolchain.init ~config:Riot_model.Toolchain_config.default
|> Result.expect ~msg:"Failed to initialize test toolchain"

let stdlib_dependency =
  Tusk_model.Package.{
    name = "stdlib";
    source = {
      workspace = false;
      builtin = true;
      path = None;
      version = None;
    };
  }

let test_collect_source_files = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"pkg_test"
      (fun tmpdir ->
        let src_dir = Path.(tmpdir / Path.v "src") in
        let _ = Fs.create_dir_all src_dir in
        let ml_file = Path.(src_dir / Path.v "foo.ml") in
        let mli_file = Path.(src_dir / Path.v "bar.mli") in
        let c_file = Path.(src_dir / Path.v "native.c") in
        let txt_file = Path.(src_dir / Path.v "readme.txt") in
        let _ = Fs.write "let x = 1" ml_file |> Result.expect ~msg:"Write failed" in
        let _ = Fs.write "val x : int" mli_file |> Result.expect ~msg:"Write failed" in
        let _ = Fs.write "int main() {}" c_file |> Result.expect ~msg:"Write failed" in
        let _ = Fs.write "readme" txt_file |> Result.expect ~msg:"Write failed" in
        let package = Riot_model.Package.make ~name:"test" ~path:tmpdir ~relative_path:(Path.v ".") () in
        let files = Riot_executor.Package_builder.collect_source_files package in
        let has_ml =
          List.exists (fun p -> Path.to_string p = Path.to_string ml_file) files
        in
        let has_mli =
          List.exists (fun p -> Path.to_string p = Path.to_string mli_file) files
        in
        let has_c =
          List.exists (fun p -> Path.to_string p = Path.to_string c_file) files
        in
        let has_txt =
          List.exists (fun p -> Path.to_string p = Path.to_string txt_file) files
        in
        let count = List.length files in
        if has_ml && has_mli && has_c && (not has_txt) && count = 3 then
          Ok ()
        else
          Error ("Expected exactly .ml, .mli, .c files. Got "
          ^ Int.to_string count
          ^ " files: has_ml="
          ^ Bool.to_string has_ml
          ^ " has_mli="
          ^ Bool.to_string has_mli
          ^ " has_c="
          ^ Bool.to_string has_c
          ^ " has_txt="
          ^ Bool.to_string has_txt
          ^ ". Files: "
          ^ String.concat ", " (List.map (fun p -> Path.basename p) files)))
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let test_build_result_status_variants = fun _ctx ->
  let package = Riot_model.Package.make ~name:"test" ~path:(Path.v ".") ~relative_path:(Path.v ".") () in
  let artifact =
    Riot_store.Artifact.{
      hash = Crypto.hash_string "test";
      files = [];
      ocamlc_warnings = [];
      exports = []
    } in
  let result_cached =
    Riot_executor.Package_builder.{
      package_key = Riot_planner.Package_graph.package_key
        ~package_name:package.name
        Riot_planner.Package_graph.Runtime;
      package;
      status = Cached artifact;
      ocamlc_warnings = [];
      duration = Time.Duration.from_millis 5;
    }
  in
  let result_built =
    Riot_executor.Package_builder.{
      package_key = Riot_planner.Package_graph.package_key
        ~package_name:package.name
        Riot_planner.Package_graph.Runtime;
      package;
      status = Built artifact;
      ocamlc_warnings = [];
      duration = Time.Duration.from_millis 100;
    }
  in
  let result_failed =
    Riot_executor.Package_builder.{
      package_key = Riot_planner.Package_graph.package_key
        ~package_name:package.name
        Riot_planner.Package_graph.Runtime;
      package;
      status = Failed (ExecutionFailed { message = "compilation error" });
      ocamlc_warnings = [];
      duration = Time.Duration.from_millis 50;
    }
  in
  match (result_cached.status, result_built.status, result_failed.status) with
  | Cached _, Built _, Failed _ -> Ok ()
  | _ -> Error "Status variants don't match expected types"

let test_package_error_variants = fun _ctx ->
  let planning_error = Riot_planner.Planning_error.Exception { exn = Failure "test" } in
  let error1 = Riot_executor.Package_builder.PlanningFailed planning_error in
  let error2 = Riot_executor.Package_builder.ExecutionFailed { message = "build failed" } in
  match (error1, error2) with
  | PlanningFailed _, ExecutionFailed _ -> Ok ()
  | _ -> Error "Error variants don't match expected types"

let test_build_writes_hash_manifest_with_exports = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"pkg_builder_export_manifest_test"
      (fun tmpdir ->
        let package_dir = Path.(tmpdir / Path.v "pkg") in
        let src_dir = Path.(package_dir / Path.v "src") in
        let _ = Fs.create_dir_all src_dir |> Result.expect ~msg:"create src dir failed" in
        let _ = Fs.write "let answer = 42\n" Path.(src_dir / Path.v "lib.ml") |> Result.expect ~msg:"write source failed" in
        let package = Riot_model.Package.make ~name:"pkg" ~path:package_dir ~relative_path:(Path.v "pkg") ~library:{
          path = Path.v "src/lib.ml"
        }
          ~sources:{
            src = [ Path.v "src/lib.ml" ];
            native = [];
            tests = [];
            examples = [];
            bench = [];
          }
          ()
        in
        let workspace =
          Riot_model.Workspace.{
            root = tmpdir;
            target_dir_root =
              Path.(tmpdir / Path.v "target");
            packages = [ package ];
            dependencies = [];
            dev_dependencies = [];
            build_dependencies = [];
            profile_overrides = [];
          }
        in
        let store = Riot_store.Store.create ~workspace in
        let package_graph = Riot_planner.Package_graph.create
          ~scope:Riot_planner.Package_graph.Runtime workspace
        |> Result.unwrap in
        let build_ctx =
          let session_id = Riot_model.Session_id.make () in
          Riot_model.Build_ctx.make ~session_id ~profile:Riot_model.Profile.debug ()
        in
        let result = Riot_executor.Package_builder.build
          ~workspace
          ~toolchain:test_toolchain
          ~store
          ~package_graph
          ~package_key:(Riot_planner.Package_graph.package_key
            ~package_name:package.name
            Riot_planner.Package_graph.Runtime)
          ~package
          ~build_ctx in
        match result.status with
        | Riot_executor.Package_builder.Failed err ->
            Error ("build failed: " ^ Riot_executor.Package_builder.package_error_to_string err)
        | Riot_executor.Package_builder.Skipped { reason } ->
            Error ("build skipped: " ^ reason)
        | Riot_executor.Package_builder.Built artifact
        | Riot_executor.Package_builder.Cached artifact ->
            match Riot_store.Store.load_manifest store ~hash:artifact.hash with
            | None -> Error "expected package hash manifest to be saved"
            | Some manifest ->
                if List.length manifest.Riot_store.Manifest.exports > 0 then
                  Ok ()
                else
                  Error "expected hash manifest to include exported outputs")
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let test_build_expands_format_macro_in_binary_end_to_end = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"pkg_builder_macro_test"
      (fun tmpdir ->
        let package_dir = Path.(tmpdir / Path.v "pkg") in
        let src_dir = Path.(package_dir / Path.v "src") in
        let _ = Fs.create_dir_all src_dir |> Result.expect ~msg:"create src dir failed" in
        let _ = Fs.write "let message = format! \"hello {}\" \"riot\"\n" Path.(src_dir / Path.v "main.ml")
        |> Result.expect ~msg:"write source failed" in
        let package =
          Tusk_model.Package.{
            name = "pkg";
            path = package_dir;
            relative_path = Path.v "pkg";
            dependencies = [ stdlib_dependency ];
            dev_dependencies = [];
            build_dependencies = [];
            foreign_dependencies = [];
            binaries = [ { name = "pkg"; path = Path.v "src/main.ml" } ];
            library = None;
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
        in
        let workspace =
          Tusk_model.Workspace.{
            root = tmpdir;
            target_dir_root =
              Path.(tmpdir / Path.v "target");
            packages = [ package ];
            dependencies = [];
            dev_dependencies = [];
            build_dependencies = [];
            profile_overrides = [];
          }
        in
        let store = Tusk_store.Store.create ~workspace in
        let package_graph = Tusk_planner.Package_graph.create
          ~scope:Tusk_planner.Package_graph.Runtime workspace
        |> Result.unwrap in
        let build_ctx =
          let session_id = Tusk_model.Session_id.make () in
          Tusk_model.Build_ctx.make ~session_id ~profile:Tusk_model.Profile.debug ()
        in
        let result = Tusk_executor.Package_builder.build
          ~workspace
          ~toolchain:test_toolchain
          ~store
          ~package_graph
          ~package_key:(Tusk_planner.Package_graph.package_key
            ~package_name:package.name
            Tusk_planner.Package_graph.Runtime)
          ~package
          ~build_ctx in
        match result.status with
        | Tusk_executor.Package_builder.Built _
        | Tusk_executor.Package_builder.Cached _ -> Ok ()
        | Tusk_executor.Package_builder.Failed err ->
            Error ("macro build failed: " ^ Tusk_executor.Package_builder.package_error_to_string err)
        | Tusk_executor.Package_builder.Skipped { reason } ->
            Error ("macro build skipped: " ^ reason))
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let test_build_rejects_macro_provider_export_drift = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"pkg_builder_macro_export_drift"
      (fun tmpdir ->
        let std_package = make_test_std_package ~tmpdir in
        let package_dir = Path.(tmpdir / Path.v "pkg") in
        let macro_dir = Path.(tmpdir / Path.v "macro") in
        let src_dir = Path.(package_dir / Path.v "src") in
        let macro_src_dir = Path.(macro_dir / Path.v "src") in
        let _ = Fs.create_dir_all src_dir |> Result.expect ~msg:"create src dir failed" in
        let _ = Fs.create_dir_all macro_src_dir |> Result.expect ~msg:"create macro src dir failed" in
        let _ = Fs.write
          "let message = Macro.format! \"hello {}\" \"riot\"\n"
          Path.(src_dir / Path.v "main.ml")
        |> Result.expect ~msg:"write source failed" in
        let _ = Fs.write
          "let provider () = Macro.Provider.v ~module_path:[ \"Macro\" ] [ Macro.Provider.fn \"other\" Macro.Format.expand ]\n"
          Path.(macro_src_dir / Path.v "macro.ml")
        |> Result.expect ~msg:"write macro source failed" in
        let macro_package =
          Riot_model.Package.{
            name = "macro";
            path = macro_dir;
            relative_path = Path.v "macro";
            dependencies = [ stdlib_dependency ];
            dev_dependencies = [];
            build_dependencies = [];
            foreign_dependencies = [];
            binaries = [];
            library = None;
            sources =
              {
                src = [ Path.v "src/macro.ml" ];
                native = [];
                tests = [];
                examples = [];
                bench = [];
              };
            compiler = { profile_overrides = []; target_overrides = [] };
            commands = [];
            fix_providers = [];
            macro_providers =
              [
                Riot_model.Macro_provider.make
                  ~package_name:"macro"
                  ~package_path:macro_dir
                  ~source_path:(Path.(macro_dir / Path.v "src" / Path.v "macro.ml"))
                  ~module_path:[ "Macro" ]
                  ~macros:[ "format" ]
                  ();
              ];
            publish = { version = None; description = None; license = None; is_public = None };
          }
        in
        let package =
          Riot_model.Package.{
            name = "pkg";
            path = package_dir;
            relative_path = Path.v "pkg";
            dependencies = [ workspace_dependency "std" ];
            dev_dependencies = [];
            build_dependencies = [ workspace_dependency "macro" ];
            foreign_dependencies = [];
            binaries = [ { name = "pkg"; path = Path.v "src/main.ml" } ];
            library = None;
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
            macro_providers = [];
            publish = { version = None; description = None; license = None; is_public = None };
          }
        in
        let workspace =
          Riot_model.Workspace.{
            root = tmpdir;
            target_dir_root = Path.(tmpdir / Path.v "target");
            packages = [ std_package; macro_package; package ];
            dependencies = [];
            dev_dependencies = [];
            build_dependencies = [];
            profile_overrides = [];
          }
        in
        let store = Riot_store.Store.create ~workspace in
        let package_graph = Riot_planner.Package_graph.create
          ~scope:Riot_planner.Package_graph.Runtime workspace
        |> Result.unwrap in
        let build_ctx =
          let session_id = Riot_model.Session_id.make () in
          Riot_model.Build_ctx.make ~session_id ~profile:Riot_model.Profile.debug ()
        in
        let package_key = Riot_planner.Package_graph.package_key
          ~package_name:package.name
          Riot_planner.Package_graph.Runtime in
        let build_package = Riot_model.Package.for_scope Riot_model.Package.Build package in
        let build_package_key = Riot_planner.Package_graph.package_key
          ~package_name:package.name
          Riot_planner.Package_graph.Build in
        let std_package_key = Riot_planner.Package_graph.package_key
          ~package_name:std_package.name
          Riot_planner.Package_graph.Runtime in
        let build_result = Riot_executor.Package_builder.build
          ~workspace
          ~toolchain:test_toolchain
          ~store
          ~package_graph
          ~package_key:build_package_key
          ~package:build_package
          ~build_ctx in
        let std_result = Riot_executor.Package_builder.build
          ~workspace
          ~toolchain:test_toolchain
          ~store
          ~package_graph
          ~package_key:std_package_key
          ~package:std_package
          ~build_ctx in
        let build_ok result =
          match result.Riot_executor.Package_builder.status with
          | Riot_executor.Package_builder.Built _
          | Riot_executor.Package_builder.Cached _ -> Ok ()
          | Riot_executor.Package_builder.Failed err -> Error (Riot_executor.Package_builder.package_error_to_string err)
          | Riot_executor.Package_builder.Skipped { reason } -> Error reason
        in
        match build_ok build_result, build_ok std_result with
        | Error message, _
        | _, Error message -> Error ("setup build failed: " ^ message)
        | Ok (), Ok () ->
            let result = Riot_executor.Package_builder.build
              ~workspace
              ~toolchain:test_toolchain
              ~store
              ~package_graph
              ~package_key
              ~package
              ~build_ctx in
            (
              match result.Riot_executor.Package_builder.status with
              | Riot_executor.Package_builder.Failed err ->
                  let message = Riot_executor.Package_builder.package_error_to_string err in
                  if
                    String.contains message "manifest declares: Macro.format!"
                    && String.contains message "runner exports: Macro.other!"
                  then
                    Ok ()
                  else
                    Error ("unexpected export-drift message: " ^ message)
              | Riot_executor.Package_builder.Built _
              | Riot_executor.Package_builder.Cached _ ->
                  Error "expected build to fail when provider runtime exports drift from the manifest"
              | Riot_executor.Package_builder.Skipped { reason } ->
                  Error ("expected build failure, got skipped: " ^ reason)
            ))
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let tests =
  Test.[
    case "collect_source_files: filters by extension" test_collect_source_files;
    case "build_result: status variants" test_build_result_status_variants;
    case "package_error: variants" test_package_error_variants;
    case "build writes hash manifest with exports" test_build_writes_hash_manifest_with_exports;
    case "build expands format! in binary end-to-end" test_build_expands_format_macro_in_binary_end_to_end;
    case "build rejects macro provider export drift" test_build_rejects_macro_provider_export_drift;
  ]

let name = "Package Builder Tests"

let () = Actors.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
