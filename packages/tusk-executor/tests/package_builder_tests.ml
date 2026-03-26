open Std
open Std.Collections
module Test = Std.Test

let test_toolchain =
  Tusk_toolchain.init ~config:Tusk_model.Toolchain_config.default
  |> Result.expect ~msg:"Failed to initialize test toolchain"

let test_collect_source_files () =
  match
    Fs.with_tempdir ~prefix:"pkg_test" (fun tmpdir ->
        let src_dir = Path.(tmpdir / Path.v "src") in
        let _ = Fs.create_dir_all src_dir in

        let ml_file = Path.(src_dir / Path.v "foo.ml") in
        let mli_file = Path.(src_dir / Path.v "bar.mli") in
        let c_file = Path.(src_dir / Path.v "native.c") in
        let txt_file = Path.(src_dir / Path.v "readme.txt") in

        let _ =
          Fs.write "let x = 1" ml_file |> Result.expect ~msg:"Write failed"
        in
        let _ =
          Fs.write "val x : int" mli_file |> Result.expect ~msg:"Write failed"
        in
        let _ =
          Fs.write "int main() {}" c_file |> Result.expect ~msg:"Write failed"
        in
        let _ =
          Fs.write "readme" txt_file |> Result.expect ~msg:"Write failed"
        in

        let package =
          Tusk_model.Package.
          {
            name = "test";
            path = tmpdir;
            relative_path = Path.v ".";
            dependencies = [];
            dev_dependencies = [];
            build_dependencies = [];
            foreign_dependencies = [];
            binaries = [];
            library = None;
            sources = { src = []; native = []; tests = []; examples = []; bench = [] };
            compiler = { profile_overrides = []; target_overrides = [] };
            commands = [];
            fix_providers = [];
          }
        in

        let files =
          Tusk_executor.Package_builder.collect_source_files package
        in

        let has_ml =
          List.exists (fun p -> Path.to_string p = Path.to_string ml_file) files
        in
        let has_mli =
          List.exists
            (fun p -> Path.to_string p = Path.to_string mli_file)
            files
        in
        let has_c =
          List.exists (fun p -> Path.to_string p = Path.to_string c_file) files
        in
        let has_txt =
          List.exists
            (fun p -> Path.to_string p = Path.to_string txt_file)
            files
        in

        let count = List.length files in

        if has_ml && has_mli && has_c && (not has_txt) && count = 3 then Ok ()
        else
          Error
            ("Expected exactly .ml, .mli, .c files. Got "
            ^ Int.to_string count ^ " files: has_ml="
            ^ Bool.to_string has_ml ^ " has_mli=" ^ Bool.to_string has_mli
            ^ " has_c=" ^ Bool.to_string has_c ^ " has_txt="
            ^ Bool.to_string has_txt ^ ". Files: "
            ^ String.concat ", " (List.map (fun p -> Path.basename p) files)))
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let test_build_result_status_variants () =
  let package =
    Tusk_model.Package.
      {
        name = "test";
        path = Path.v ".";
        relative_path = Path.v ".";
        dependencies = [];
        dev_dependencies = [];
        build_dependencies = [];
        foreign_dependencies = [];
        binaries = [];
        library = None;
        sources = { src = []; native = []; tests = []; examples = []; bench = [] };
        compiler = { profile_overrides = []; target_overrides = [] };
        commands = [];
        fix_providers = [];
      }
  in

  let artifact =
    Tusk_store.Artifact.{ hash = Crypto.hash_string "test"; files = [] }
  in

  let result_cached =
    Tusk_executor.Package_builder.
      {
        package_key =
          Tusk_planner.Package_graph.package_key ~package_name:package.name
            Tusk_planner.Package_graph.Runtime;
        package;
        status = Cached artifact;
        duration = Time.Duration.from_millis 5;
      }
  in

  let result_built =
    Tusk_executor.Package_builder.
      {
        package_key =
          Tusk_planner.Package_graph.package_key ~package_name:package.name
            Tusk_planner.Package_graph.Runtime;
        package;
        status = Built artifact;
        duration = Time.Duration.from_millis 100;
      }
  in

  let result_failed =
    Tusk_executor.Package_builder.
      {
        package_key =
          Tusk_planner.Package_graph.package_key ~package_name:package.name
            Tusk_planner.Package_graph.Runtime;
        package;
        status = Failed (ExecutionFailed { message = "compilation error" });
        duration = Time.Duration.from_millis 50;
      }
  in

  match (result_cached.status, result_built.status, result_failed.status) with
  | Cached _, Built _, Failed _ -> Ok ()
  | _ -> Error "Status variants don't match expected types"

let test_package_error_variants () =
  let planning_error =
    Tusk_planner.Planning_error.Exception { exn = Failure "test" }
  in

  let error1 = Tusk_executor.Package_builder.PlanningFailed planning_error in
  let error2 =
    Tusk_executor.Package_builder.ExecutionFailed { message = "build failed" }
  in

  match (error1, error2) with
  | PlanningFailed _, ExecutionFailed _ -> Ok ()
  | _ -> Error "Error variants don't match expected types"

let test_build_writes_package_export_manifest () =
  match
    Fs.with_tempdir ~prefix:"pkg_builder_export_manifest_test" (fun tmpdir ->
        let package_dir = Path.(tmpdir / Path.v "pkg") in
        let src_dir = Path.(package_dir / Path.v "src") in
        let _ = Fs.create_dir_all src_dir |> Result.expect ~msg:"create src dir failed" in
        let _ =
          Fs.write "let answer = 42\n" Path.(src_dir / Path.v "lib.ml")
          |> Result.expect ~msg:"write source failed"
        in
        let package =
          Tusk_model.Package.
            {
              name = "pkg";
              path = package_dir;
              relative_path = Path.v "pkg";
              dependencies = [];
              dev_dependencies = [];
              build_dependencies = [];
              foreign_dependencies = [];
              binaries = [];
              library = Some { path = Path.v "src/lib.ml" };
              sources = { src = []; native = []; tests = []; examples = []; bench = [] };
              compiler = { profile_overrides = []; target_overrides = [] };
              commands = [];
              fix_providers = [];
            }
        in
        let workspace =
          Tusk_model.Workspace.
            {
              root = tmpdir;
              target_dir_root = Path.(tmpdir / Path.v "target");
              packages = [ package ];
              profile_overrides = [];
            }
        in
        let store = Tusk_store.Store.create ~workspace in
        let package_graph =
          Tusk_planner.Package_graph.create ~scope:Tusk_planner.Package_graph.Runtime workspace
          |> Result.unwrap
        in
        let build_ctx =
          let session_id = Tusk_model.Session_id.make () in
          Tusk_model.Build_ctx.make ~session_id ~profile:Tusk_model.Profile.debug ()
        in
        let result =
          Tusk_executor.Package_builder.build ~workspace ~toolchain:test_toolchain
            ~store ~package_graph
            ~package_key:
              (Tusk_planner.Package_graph.package_key ~package_name:package.name
                 Tusk_planner.Package_graph.Runtime)
            ~package ~build_ctx
        in
        match result.status with
        | Tusk_executor.Package_builder.Failed err ->
            Error
              ("build failed: "
              ^ Tusk_executor.Package_builder.package_error_to_string err)
        | Tusk_executor.Package_builder.Built _
        | Tusk_executor.Package_builder.Cached _ ->
            let target =
              Kernel.System.Host.to_string
                (Tusk_model.Build_ctx.target_triplet build_ctx)
            in
            (match
               Tusk_store.Store.load_package_exports store ~package:package.name
                 ~profile:"debug" ~target
             with
            | None -> Error "expected package export manifest to be saved"
            | Some exports ->
                if List.length exports > 0 then Ok ()
                else Error "expected at least one exported output entry"))
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let tests =
  Test.
    [
      case "collect_source_files: filters by extension"
        test_collect_source_files;
      case "build_result: status variants" test_build_result_status_variants;
      case "package_error: variants" test_package_error_variants;
      case "build writes package export manifest"
        test_build_writes_package_export_manifest;
    ]

let name = "Package Builder Tests"
let () = Miniriot.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
