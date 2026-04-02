open Std
open Macro

let snapshot_ctx = fun (ctx: Test.ctx) ->
  let workspace_root =
    match ctx.workspace_root with
    | Some _ -> ctx.workspace_root
    | None -> Env.current_dir () |> Std.Result.to_option
  in
  { ctx with package_name = Some "macro"; workspace_root }

let with_temp_provider_result = fun ~prefix ~source fn ->
  match
    Fs.with_tempdir ~prefix
      (fun tmpdir ->
        let package_root = Path.(tmpdir / Path.v "packages" / Path.v "demo-macro") in
        let src_dir = Path.(package_root / Path.v "src") in
        let source_path = Path.(src_dir / Path.v "macro.ml") in
        let _ = Fs.create_dir_all src_dir |> Std.Result.expect ~msg:"create provider src dir failed" in
        let _ = Fs.write source source_path |> Std.Result.expect ~msg:"write provider source failed" in
        let provider = Riot_model.Macro_provider.make
          ~package_name:"demo-macro"
          ~package_path:package_root
          ~source_path:(Path.v "src/macro.ml") in
        fn provider)
  with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let assert_provider_valid = fun ~source ->
  with_temp_provider_result
    ~prefix:"macro_provider_valid"
    ~source
    (fun provider ->
      match Provider_contract.validate provider with
      | Ok () -> Ok ()
      | Error err -> Error ("expected provider contract validation to succeed: " ^ error_message err))

let assert_provider_error_contains = fun ~source ~expected_substring ->
  with_temp_provider_result
    ~prefix:"macro_provider_invalid"
    ~source
    (fun provider ->
      match Provider_contract.validate provider with
      | Ok () -> Error "expected provider contract validation to fail"
      | Error err ->
          if String.contains err.message expected_substring then
            Ok ()
          else
            Error ("expected provider error containing '" ^ expected_substring ^ "', got '" ^ err.message ^ "'"))

let snapshot_provider_error = fun ~ctx ~source ->
  with_temp_provider_result
    ~prefix:"macro_provider_invalid"
    ~source
    (fun provider ->
      match Provider_contract.validate provider with
      | Ok () -> Error "expected provider contract validation to fail"
      | Error err -> Test.Snapshot.assert_text ~ctx:(snapshot_ctx ctx) ~actual:(error_message err ^ "\n"))

let tests = [
  Test.case
    "provider contract accepts top-level let provider entrypoints"
    (fun _ctx ->
      assert_provider_valid
        ~source:"let provider () = Macro.Provider.v ~module_path:[ \"Macro\" ] [ Macro.Provider.fn \"format\" Macro.Format.expand ]\n");
  Test.case
    "provider contract snapshots missing provider entrypoints"
    (fun ctx ->
      snapshot_provider_error
        ~ctx
        ~source:"let expand tokens = { Macro.Result.output = tokens; diagnostics = [] }\n");
  Test.case
    "provider contract snapshots parse errors"
    (fun ctx ->
      snapshot_provider_error
        ~ctx
        ~source:"let provider () =\n");
  Test.case
    "provider contract rejects non-callable provider bindings"
    (fun _ctx ->
      assert_provider_error_contains
        ~source:"let provider = Macro.Provider.v ~module_path:[ \"Macro\" ] []\n"
        ~expected_substring:"let provider () = ...");
  Test.case
    "provider contract rejects provider functions without a unit parameter"
    (fun _ctx ->
      assert_provider_error_contains
        ~source:"let provider value = Macro.Provider.v ~module_path:[ \"Macro\" ] []\n"
        ~expected_substring:"let provider () = ...");
]

let () =
  Actors.run ~main:(fun ~args -> Test.Cli.main ~name:"macro:provider_contract" ~tests ~args) ~args:Env.args ()
