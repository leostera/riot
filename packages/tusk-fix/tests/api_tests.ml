open Std

let with_tempdir = fun prefix fn ->
  match Fs.with_tempdir ~prefix fn with
  | Ok result -> result
  | Error err -> Error (IO.error_message err)

let with_cwd = fun path fn ->
  let original = Env.current_dir () |> Result.expect ~msg:"failed to get cwd" in
  Env.set_current_dir path |> Result.expect ~msg:"failed to chdir into test dir";
  try
    let result = fn () in
    Env.set_current_dir original |> Result.expect ~msg:"failed to restore cwd";
    result
  with
  | exn ->
      Env.set_current_dir original |> Result.expect ~msg:"failed to restore cwd after exception";
      raise exn

let with_env_var = fun ~name ~value fn ->
  let _ = Env.set_var ~name ~value in
  try
    let result = fn () in
    let _ = Env.set_var ~name ~value:"0" in
    result
  with
  | exn ->
      let _ = Env.set_var ~name ~value:"0" in
      raise exn

let parse_matches = fun argv ->
  match ArgParser.get_matches Tusk_fix.Cli.command ("fix" :: argv) with
  | Error err -> Error (ArgParser.error_message err)
  | Ok matches -> Ok matches

let tests = [ Test.case "fix_request_of_matches parses explicit check requests"
    (fun _ctx ->
      with_tempdir "tusk_fix_api"
        (fun tmpdir ->
          with_cwd tmpdir
            (fun () ->
              match parse_matches [ "--json"; "--check"; "sample.ml" ] with
              | Error _ as err -> err
              | Ok matches -> (
                  match Tusk_fix.fix_request_of_matches matches with
                  | Error err -> Error (Exception.to_string err)
                  | Ok request -> (
                      match request.action with
                      | Tusk_fix.Run { mode; target; output_mode; _ } ->
                          Test.assert_equal ~expected:Tusk_fix.Runner.Check ~actual:mode;
                          Test.assert_equal ~expected:(Path.v "sample.ml") ~actual:target;
                          Test.assert_equal
                            ~expected:(Tusk_fix.Report Tusk_fix.Reporter.Json)
                            ~actual:output_mode;
                          Ok ()
                      | _ -> Error "expected run request"
                    )
                )))); Test.case "fix list-rules returns structured output"
    (fun _ctx ->
      with_tempdir "tusk_fix_api"
        (fun tmpdir ->
          with_cwd tmpdir
            (fun () ->
              match parse_matches [ "--list-rules" ] with
              | Error _ as err -> err
              | Ok matches -> (
                  match Tusk_fix.fix_request_of_matches matches with
                  | Error err -> Error (Exception.to_string err)
                  | Ok request -> (
                      match Tusk_fix.fix request with
                      | Error err -> Error (Exception.to_string err)
                      | Ok response ->
                          match Tusk_fix.response_output response with
                          | Some output ->
                              Test.assert_true (String.contains output "snake-case-type-names");
                              Ok ()
                          | None -> Error "expected list-rules output"
                    )
                )))); Test.case "event to_json encodes progress events"
    (fun _ctx ->
      let event = Tusk_fix.Event.FileProgress {
        file = Path.v "sample.ml";
        progress = {
          phase = Fixme.Source_runner.RuleStarted { rule_id = "riot:snake-case-type-names" };
          timestamp_ms = 42
        }
      } in
      match Tusk_fix.Event.to_json event with
      | Data.Json.Object fields ->
          Test.assert_equal
            ~expected:(Some (Data.Json.String "progress"))
            ~actual:(List.assoc_opt "type" fields);
          Test.assert_equal
            ~expected:(Some (Data.Json.String "rule_started"))
            ~actual:(List.assoc_opt "stage" fields);
          Ok ()
      | _ -> Error "expected JSON object"); Test.case "fix check emits events through the top-level api"
    (fun _ctx ->
      with_tempdir "tusk_fix_api"
        (fun tmpdir ->
          let sample = Path.(tmpdir / Path.v "sample.ml") in
          Fs.write "type userProfile = int\n" sample |> Result.expect ~msg:"failed to write sample";
          let seen = ref [] in
          let request : Tusk_fix.fix_request = {
            cwd = tmpdir;
            scope = None;
            action =
              Tusk_fix.Run {
                mode = Tusk_fix.Runner.Check;
                limit = None;
                target = sample;
                forwarded_args = [ "--check"; Path.to_string sample ];
                output_mode = Tusk_fix.Silent;
                use_generated_runner = false;
              };
          }
          in
          match Tusk_fix.fix ~on_event:(fun event -> seen := event :: !seen) request with
          | Ok _ -> Error "expected issues to remain"
          | Error err ->
              Test.assert_true
                (String.contains (Exception.to_string err) "Issues remain after tusk fix");
              Test.assert_true (List.length !seen > 0);
              Ok ())); Test.case "fix_request_of_matches disables generated runner when requested via env"
    (fun _ctx ->
      with_tempdir "tusk_fix_api"
        (fun tmpdir ->
          with_cwd tmpdir
            (fun () ->
              Fs.write
                {|
[workspace]
members = []

[tusk.fix.provider]
rules = ["std:no-stdlib"]
|}
                Path.(tmpdir / Path.v "tusk.toml") |> Result.expect ~msg:"failed to write workspace tusk.toml";
              with_env_var ~name:"TUSK_FIX_DISABLE_GENERATED_RUNNER" ~value:"1"
                (fun () ->
                  match parse_matches [ "--check"; "sample.ml" ] with
                  | Error _ as err -> err
                  | Ok matches -> (
                      match Tusk_fix.fix_request_of_matches matches with
                      | Error err -> Error (Exception.to_string err)
                      | Ok request -> (
                          match request.action with
                          | Tusk_fix.Run { use_generated_runner; _ } ->
                              Test.assert_equal ~expected:false ~actual:use_generated_runner;
                              Ok ()
                          | _ -> Error "expected run request"
                        )
                    ))))); ]

let () =
  Miniriot.run
    ~main:(fun ~args:_ -> Test.Cli.main ~name:"tusk-fix:api" ~tests ~args:Env.args)
    ~args:Env.args
    ()
