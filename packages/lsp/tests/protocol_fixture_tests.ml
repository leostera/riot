open Std
open Std.Data

let ( let* ) = Result.and_then

let fixture_root = Path.v "packages/lsp/tests/protocol_fixtures"

let keep_json = fun path ->
  match Path.extension path with
  | Some ".json" -> `keep
  | _ -> `skip

let decode_fixture_json = fun path ->
  let* source = Fs.read path |> Result.map_error IO.error_message in
  Json.of_string source |> Result.map_error Json.error_to_string

let render_json = fun json -> Json.to_string_pretty json ^ "\n"

let roundtrip_request:
  type params res.
  (params, res) Lsp.Method.request -> Json.t -> (Json.t, string) result =
  fun method_ -> fun json ->
  let* id, params = Lsp.request_of_json method_ json in
  Ok (Lsp.request_to_json ~id method_ params)

let roundtrip_notification:
  type params.
  params Lsp.Method.notification -> Json.t -> (Json.t, string) result =
  fun method_ -> fun json ->
  let* params = Lsp.notification_of_json method_ json in
  Ok (Lsp.notification_to_json method_ params)

let roundtrip_response:
  type params res.
  (params, res) Lsp.Method.request -> Json.t -> (Json.t, string) result =
  fun method_ -> fun json ->
  let* id, result = Lsp.response_of_json method_ json in
  Ok (Lsp.response_to_json ~id method_ result)

let roundtrip_error_response = fun json ->
  let* id, error = Lsp.error_response_of_json json in
  Ok (Lsp.error_response_to_json ~id error)

let roundtrip_fixture = fun relpath -> fun json ->
  match Path.to_string relpath with
  | "requests/initialize.json" -> roundtrip_request Lsp.Initialize.request json
  | "requests/shutdown.json" -> roundtrip_request Lsp.Shutdown.request json
  | "requests/formatting.json" ->
      roundtrip_request Lsp.Text_document_methods.Formatting.request json
  | "requests/code_action.json" ->
      roundtrip_request Lsp.Text_document_methods.Code_action.request json
  | "notifications/initialized.json" ->
      roundtrip_notification Lsp.Initialized.notification json
  | "notifications/exit.json" -> roundtrip_notification Lsp.Exit.notification json
  | "notifications/did_open.json" ->
      roundtrip_notification Lsp.Text_document_methods.Did_open.notification json
  | "notifications/did_change.json" ->
      roundtrip_notification Lsp.Text_document_methods.Did_change.notification json
  | "notifications/did_close.json" ->
      roundtrip_notification Lsp.Text_document_methods.Did_close.notification json
  | "notifications/publish_diagnostics.json" ->
      roundtrip_notification Lsp.Text_document_methods.Publish_diagnostics.notification json
  | "responses/initialize.json" -> roundtrip_response Lsp.Initialize.request json
  | "responses/shutdown.json" -> roundtrip_response Lsp.Shutdown.request json
  | "responses/formatting.json" ->
      roundtrip_response Lsp.Text_document_methods.Formatting.request json
  | "responses/code_action.json" ->
      roundtrip_response Lsp.Text_document_methods.Code_action.request json
  | "responses/error.json" -> roundtrip_error_response json
  | other -> Error ("unsupported lsp protocol fixture: " ^ other)

let test_fixture = fun ~(ctx:Test.FixtureRunner.ctx) ->
  let* json = decode_fixture_json ctx.fixture_path in
  let* actual = roundtrip_fixture ctx.fixture_relpath json in
  Test.Snapshot.assert_text ~ctx:ctx.test ~actual:(render_json actual)

let () =
  Actors.run
    ~main:(fun ~args ->
      let tests =
        Test.FixtureRunner.cases
          ()
          ~dir:fixture_root
          ~filter:keep_json
          ~run:(fun ctx -> test_fixture ~ctx)
      in
      Test.Cli.main ~name:"protocol-fixtures" ~tests ~args)
    ~args:Env.args
    ()
