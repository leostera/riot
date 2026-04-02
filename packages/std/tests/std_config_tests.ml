open Std

(* Test 1: Spec DSL construction *)

let test_spec_dsl = fun _ctx ->
  let spec = Config.Spec.for_app
    ~app:"testapp"
    [
      Config.Spec.key
        "server"
        (Config.Spec.map
          [ Config.Spec.string "host" ~default:"localhost"; Config.Spec.int "port" ~default:4_000; ]);
    ] in
  if Config.Spec.app_name spec = "testapp" then
    Ok ()
  else
    Error "Spec app name mismatch"

(* Test 2: Loader - environment detection *)

let test_env_detection = fun _ctx ->
  let env = Config.Loader.detect_env () in
  (* Just check that it returns something valid *)
  match env with
  | Config.Loader.Dev
  | Config.Loader.Test
  | Config.Loader.Prod -> Ok ()

(* Test 3: Loader - file loading *)

let test_file_loading = fun _ctx ->
  match Config.Loader.load_file "./packages/std/tests/fixtures/config/dev.toml" with
  | Error err -> Error ("Failed to load file: " ^ err)
  | Ok _toml -> Ok ()

(* Test 4: Loader - extract app section *)

let test_extract_app_section = fun _ctx ->
  match Config.Loader.load_file "./packages/std/tests/fixtures/config/dev.toml" with
  | Error err -> Error ("Failed to load file: " ^ err)
  | Ok toml -> (
      match Config.Loader.extract_app_section "testapp" toml with
      | Error err -> Error ("Failed to extract app section: " ^ err)
      | Ok section -> (
          match section with
          | Data.Toml.Table _ -> Ok ()
          | _ -> Error "Expected table section"
        )
    )

(* Test 5: Validator - simple validation *)

let test_validation = fun _ctx ->
  let spec = Config.Spec.for_app ~app:"testapp" [ Config.Spec.string "log_level" ~default:"info"; ] in
  let toml = Data.Toml.Table [ ("log_level", Data.Toml.String "debug"); ] in
  match Config.Validator.validate spec toml with
  | Error err -> Error ("Validation failed: " ^ err)
  | Ok _validated -> Ok ()

(* Test 6: Validator - default values *)

let test_defaults = fun _ctx ->
  let spec = Config.Spec.for_app
    ~app:"testapp"
    [ Config.Spec.string "log_level" ~default:"info"; Config.Spec.int "port" ~default:8_080; ] in
  let toml = Data.Toml.Table [] in
  match Config.Validator.validate spec toml with
  | Error err ->
      Error ("Validation failed: " ^ err)
  | Ok (Config.Spec.Map validated) -> (
      match Collections.List.assoc_opt "log_level" validated with
      | Some (Config.Spec.String "info") -> Ok ()
      | _ -> Error "Default value not applied"
    )
  | Ok _ ->
      Error "Unexpected validation result"

(* Test 7: Validator - nested maps *)

let test_nested_validation = fun _ctx ->
  let spec = Config.Spec.for_app
    ~app:"testapp"
    [
      Config.Spec.key
        "server"
        (Config.Spec.map
          [ Config.Spec.string "host" ~default:"localhost"; Config.Spec.int "port" ~default:4_000; ]);
    ] in
  let toml = Data.Toml.Table [
    (
      "server",
      Data.Toml.Table [ ("host", Data.Toml.String "0.0.0.0"); ("port", Data.Toml.String "8080"); ]
    );
  ] in
  match Config.Validator.validate spec toml with
  | Error err -> Error ("Nested validation failed: " ^ err)
  | Ok _validated -> Ok ()

(* Test 8: Error handling - missing required field *)

let test_required_field = fun _ctx ->
  let spec = Config.Spec.for_app ~app:"testapp" [ Config.Spec.string "api_key" ~required:true; ] in
  let toml = Data.Toml.Table [] in
  match Config.Validator.validate spec toml with
  | Error _err -> Ok ()
  | Ok _ -> Error "Should have failed on missing required field"

(* Test 9: Enum string - valid value *)

let test_enum_string_valid = fun _ctx ->
  let spec = Config.Spec.for_app
    ~app:"testapp"
    [
      Config.Spec.enum
        (Config.Spec.string "log_level" ~default:"info")
        [
          Config.Spec.String "debug";
          Config.Spec.String "info";
          Config.Spec.String "warn";
          Config.Spec.String "error"
        ];
    ] in
  let toml = Data.Toml.Table [ ("log_level", Data.Toml.String "debug"); ] in
  match Config.Validator.validate spec toml with
  | Error err ->
      Error ("Validation failed: " ^ err)
  | Ok (Config.Spec.Map validated) -> (
      match Collections.List.assoc_opt "log_level" validated with
      | Some (Config.Spec.String "debug") -> Ok ()
      | _ -> Error "Enum string value not validated correctly"
    )
  | Ok _ ->
      Error "Unexpected validation result"

(* Test 10: Enum string - invalid value *)

let test_enum_string_invalid = fun _ctx ->
  let spec = Config.Spec.for_app
    ~app:"testapp"
    [
      Config.Spec.enum
        (Config.Spec.string "log_level" ~default:"info")
        [
          Config.Spec.String "debug";
          Config.Spec.String "info";
          Config.Spec.String "warn";
          Config.Spec.String "error"
        ];
    ] in
  let toml = Data.Toml.Table [ ("log_level", Data.Toml.String "invalid"); ] in
  match Config.Validator.validate spec toml with
  | Error _err -> Ok ()
  | Ok _ -> Error "Should have failed on invalid enum value"

(* Test 11: Enum string - default value *)

let test_enum_string_default = fun _ctx ->
  let spec = Config.Spec.for_app
    ~app:"testapp"
    [
      Config.Spec.enum
        (Config.Spec.string "log_level" ~default:"info")
        [
          Config.Spec.String "debug";
          Config.Spec.String "info";
          Config.Spec.String "warn";
          Config.Spec.String "error"
        ];
    ] in
  let toml = Data.Toml.Table [] in
  match Config.Validator.validate spec toml with
  | Error err ->
      Error ("Validation failed: " ^ err)
  | Ok (Config.Spec.Map validated) -> (
      match Collections.List.assoc_opt "log_level" validated with
      | Some (Config.Spec.String "info") -> Ok ()
      | _ -> Error "Enum string default not applied"
    )
  | Ok _ ->
      Error "Unexpected validation result"

(* Test 12: Enum int - valid value *)

let test_enum_int_valid = fun _ctx ->
  let spec = Config.Spec.for_app
    ~app:"testapp"
    [
      Config.Spec.enum
        (Config.Spec.int "status_code" ~default:200)
        [
          Config.Spec.Int 200;
          Config.Spec.Int 201;
          Config.Spec.Int 400;
          Config.Spec.Int 404;
          Config.Spec.Int 500
        ];
    ] in
  let toml = Data.Toml.Table [ ("status_code", Data.Toml.String "404"); ] in
  match Config.Validator.validate spec toml with
  | Error err ->
      Error ("Validation failed: " ^ err)
  | Ok (Config.Spec.Map validated) -> (
      match Collections.List.assoc_opt "status_code" validated with
      | Some (Config.Spec.Int 404) -> Ok ()
      | _ -> Error "Enum int value not validated correctly"
    )
  | Ok _ ->
      Error "Unexpected validation result"

(* Test 13: Enum int - invalid value *)

let test_enum_int_invalid = fun _ctx ->
  let spec = Config.Spec.for_app
    ~app:"testapp"
    [
      Config.Spec.enum
        (Config.Spec.int "status_code" ~default:200)
        [
          Config.Spec.Int 200;
          Config.Spec.Int 201;
          Config.Spec.Int 400;
          Config.Spec.Int 404;
          Config.Spec.Int 500
        ];
    ] in
  let toml = Data.Toml.Table [ ("status_code", Data.Toml.String "999"); ] in
  match Config.Validator.validate spec toml with
  | Error _err -> Ok ()
  | Ok _ -> Error "Should have failed on invalid enum value"

(* Test 14: List of strings *)

let test_list_of_strings = fun _ctx ->
  let spec = Config.Spec.for_app
    ~app:"testapp"
    [ Config.Spec.list (Config.Spec.string "" ~default:"") "tags" ~default:[]; ] in
  let toml = Data.Toml.Table [
    (
      "tags",
      Data.Toml.Array [ Data.Toml.String "tag1"; Data.Toml.String "tag2"; Data.Toml.String "tag3"; ]
    );
  ] in
  match Config.Validator.validate spec toml with
  | Error err ->
      Error ("Validation failed: " ^ err)
  | Ok (Config.Spec.Map validated) -> (
      match Collections.List.assoc_opt "tags" validated with
      | Some (Config.Spec.List items) ->
          let strings = Collections.List.map Config.as_string items in
          if strings = [ "tag1"; "tag2"; "tag3" ] then
            Ok ()
          else
            Error "List items not correct"
      | _ -> Error "List not validated correctly"
    )
  | Ok _ ->
      Error "Unexpected validation result"

(* Test 15: List with default *)

let test_list_default = fun _ctx ->
  let spec = Config.Spec.for_app
    ~app:"testapp"
    [ Config.Spec.list (Config.Spec.int "" ~default:0) "ports" ~default:[]; ] in
  let toml = Data.Toml.Table [] in
  match Config.Validator.validate spec toml with
  | Error err ->
      Error ("Validation failed: " ^ err)
  | Ok (Config.Spec.Map validated) -> (
      match Collections.List.assoc_opt "ports" validated with
      | Some (Config.Spec.List []) -> Ok ()
      | _ -> Error "Default empty list not applied"
    )
  | Ok _ ->
      Error "Unexpected validation result"

(* Test 16: List of maps *)

let test_list_of_maps = fun _ctx ->
  let spec = Config.Spec.for_app
    ~app:"testapp"
    [
      Config.Spec.list
        (Config.Spec.map
          [ Config.Spec.string "name" ~required:true; Config.Spec.int "age" ~default:0; ])
        "users"
        ~default:[];
    ] in
  let toml = Data.Toml.Table [
    (
      "users",
      Data.Toml.Array [
        Data.Toml.Table [ ("name", Data.Toml.String "Alice"); ("age", Data.Toml.String "30"); ];
        Data.Toml.Table [ ("name", Data.Toml.String "Bob"); ("age", Data.Toml.String "25"); ];
      ]
    );
  ] in
  match Config.Validator.validate spec toml with
  | Error err ->
      Error ("Validation failed: " ^ err)
  | Ok (Config.Spec.Map validated) -> (
      match Collections.List.assoc_opt "users" validated with
      | Some (Config.Spec.List items) ->
          if Collections.List.length items = 2 then
            Ok ()
          else
            Error ("Expected 2 users, got " ^ string_of_int (Collections.List.length items))
      | _ -> Error "List of maps not validated correctly"
    )
  | Ok _ ->
      Error "Unexpected validation result"

(* Test 17: Discriminated union - console variant *)

let test_discriminated_union_console = fun _ctx ->
  let spec = Config.Spec.for_app
    ~app:"testapp"
    [
      Config.Spec.key
        "handler"
        (Config.Spec.discriminated_union
          ~discriminant:"type"
          ~cases:[
            (
              "console",
              [
                Config.Spec.string "id" ~required:true;
                Config.Spec.string "format" ~default:"full";
              ]
            );
            (
              "file",
              [ Config.Spec.string "id" ~required:true; Config.Spec.string "path" ~required:true; ]
            );
          ]);
    ] in
  let toml = Data.Toml.Table [
    (
      "handler",
      Data.Toml.Table [
        ("type", Data.Toml.String "console");
        ("id", Data.Toml.String "main");
        ("format", Data.Toml.String "json");
      ]
    );
  ] in
  match Config.Validator.validate spec toml with
  | Error err ->
      Error ("Validation failed: " ^ err)
  | Ok validated ->
      let (disc, variant, fields) = Config.get_discriminated_union validated "handler" in
      if disc = "type" && variant = "console" then
        match Collections.List.assoc_opt "id" fields with
        | Some (Config.Spec.String "main") -> Ok ()
        | _ -> Error "ID not found or incorrect"
      else
        Error ("Wrong variant: expected console, got " ^ variant)

(* Test 18: Discriminated union - unknown variant *)

let test_discriminated_union_unknown = fun _ctx ->
  let spec = Config.Spec.for_app
    ~app:"testapp"
    [
      Config.Spec.key
        "handler"
        (Config.Spec.discriminated_union
          ~discriminant:"type"
          ~cases:[
            ("console", [ Config.Spec.string "id" ~required:true ]);
            ("file", [ Config.Spec.string "id" ~required:true ]);
          ]);
    ] in
  let toml = Data.Toml.Table [
    (
      "handler",
      Data.Toml.Table [ ("type", Data.Toml.String "syslog"); ("id", Data.Toml.String "remote"); ]
    );
  ] in
  match Config.Validator.validate spec toml with
  | Error _err -> Ok ()
  | Ok _ -> Error "Should have failed on unknown variant"

(* Test 19: List of discriminated unions *)

let test_list_of_discriminated_unions = fun _ctx ->
  let spec = Config.Spec.for_app
    ~app:"testapp"
    [
      Config.Spec.list
        (Config.Spec.discriminated_union
          ~discriminant:"type"
          ~cases:[
            (
              "console",
              [
                Config.Spec.string "id" ~required:true;
                Config.Spec.list (Config.Spec.string "" ~default:"info") "levels" ~default:[];
              ]
            );
            (
              "file",
              [ Config.Spec.string "id" ~required:true; Config.Spec.string "path" ~required:true; ]
            );
          ])
        "handlers"
        ~default:[];
    ] in
  let toml = Data.Toml.Table [
    (
      "handlers",
      Data.Toml.Array [
        Data.Toml.Table [
          ("type", Data.Toml.String "console");
          ("id", Data.Toml.String "main");
          ("levels", Data.Toml.Array [ Data.Toml.String "info"; Data.Toml.String "debug" ]);
        ];
        Data.Toml.Table [
          ("type", Data.Toml.String "file");
          ("id", Data.Toml.String "errors");
          ("path", Data.Toml.String "error.log");
        ];
      ]
    );
  ] in
  match Config.Validator.validate spec toml with
  | Error err ->
      Error ("Validation failed: " ^ err)
  | Ok (Config.Spec.Map validated) -> (
      match Collections.List.assoc_opt "handlers" validated with
      | Some (Config.Spec.List items) ->
          if Collections.List.length items = 2 then
            (
              match Collections.List.nth_opt items 0 with
              | Some handler ->
                  let (_, variant, _) = Config.as_discriminated_union handler in
                  if variant = "console" then
                    Ok ()
                  else
                    Error ("Expected console, got " ^ variant)
              | None -> Error "First handler not found"
            )
          else
            Error ("Expected 2 handlers, got " ^ string_of_int (Collections.List.length items))
      | _ -> Error "Handlers list not validated correctly"
    )
  | Ok _ ->
      Error "Unexpected validation result"

(* Test 20: Dotted path - two levels *)

let test_dotted_path_two_levels = fun _ctx ->
  match Config.Loader.load_file "./packages/std/tests/fixtures/config/dotted.toml" with
  | Error err -> Error ("Failed to load file: " ^ err)
  | Ok toml -> (
      match Config.Loader.extract_app_section "log.handler" toml with
      | Error err -> Error ("Failed to extract log.handler: " ^ err)
      | Ok section -> (
          match section with
          | Data.Toml.Table _ -> Ok ()
          | _ -> Error "Expected table section"
        )
    )

(* Test 21: Dotted path - three levels *)

let test_dotted_path_three_levels = fun _ctx ->
  match Config.Loader.load_file "./packages/std/tests/fixtures/config/dotted.toml" with
  | Error err -> Error ("Failed to load file: " ^ err)
  | Ok toml -> (
      match Config.Loader.extract_app_section "log.handler.stdout" toml with
      | Error err -> Error ("Failed to extract log.handler.stdout: " ^ err)
      | Ok section -> (
          match section with
          | Data.Toml.Table fields -> (
              match Collections.List.assoc_opt "format" fields with
              | Some (Data.Toml.String "full") -> Ok ()
              | _ -> Error "Expected format = full"
            )
          | _ -> Error "Expected table section"
        )
    )

(* Test 22: Dotted path - missing section *)

let test_dotted_path_missing = fun _ctx ->
  match Config.Loader.load_file "./packages/std/tests/fixtures/config/dotted.toml" with
  | Error err -> Error ("Failed to load file: " ^ err)
  | Ok toml -> (
      match Config.Loader.extract_app_section "log.handler.missing" toml with
      | Error _ -> Ok ()
      | Ok _ -> Error "Should have failed on missing section"
    )

(* Test 23: Integer values - both TOML Int and String forms *)

let test_int_values_both_forms = fun _ctx ->
  let spec = Config.Spec.for_app
    ~app:"testapp"
    [ Config.Spec.int "port" ~default:8_080; Config.Spec.int "timeout" ~default:30; ] in
  (* Test with Data.Toml.Int (native TOML integers) *)
  let toml_native = Data.Toml.Table [ ("port", Data.Toml.Int 2_112); ("timeout", Data.Toml.Int 60); ] in
  match Config.Validator.validate spec toml_native with
  | Error err ->
      Error ("Native int validation failed: " ^ err)
  | Ok (Config.Spec.Map validated) -> (
      match Collections.List.assoc_opt "port" validated with
      | Some (Config.Spec.Int 2_112) -> (
          (* Test with Data.Toml.String (backward compatibility) *)
          let toml_string = Data.Toml.Table [
            ("port", Data.Toml.String "3000");
            ("timeout", Data.Toml.String "45");
          ] in
          match Config.Validator.validate spec toml_string with
          | Error err ->
              Error ("String int validation failed: " ^ err)
          | Ok (Config.Spec.Map validated2) -> (
              match Collections.List.assoc_opt "port" validated2 with
              | Some (Config.Spec.Int 3_000) -> Ok ()
              | _ -> Error "String form port not parsed correctly"
            )
          | Ok _ ->
              Error "Unexpected validation result for string form"
        )
      | _ -> Error "Native form port not parsed correctly"
    )
  | Ok _ ->
      Error "Unexpected validation result for native form"

let tests =
  Test.[
    case "spec DSL construction" test_spec_dsl;
    case "environment detection" test_env_detection;
    case "file loading" test_file_loading;
    case "extract app section" test_extract_app_section;
    case "simple validation" test_validation;
    case "default values" test_defaults;
    case "nested validation" test_nested_validation;
    case "required field validation" test_required_field;
    case "enum string valid" test_enum_string_valid;
    case "enum string invalid" test_enum_string_invalid;
    case "enum string default" test_enum_string_default;
    case "enum int valid" test_enum_int_valid;
    case "enum int invalid" test_enum_int_invalid;
    case "list of strings" test_list_of_strings;
    case "list with default" test_list_default;
    case "list of maps" test_list_of_maps;
    case "discriminated union console" test_discriminated_union_console;
    case "discriminated union unknown" test_discriminated_union_unknown;
    case "list of discriminated unions" test_list_of_discriminated_unions;
    case "dotted path two levels" test_dotted_path_two_levels;
    case "dotted path three levels" test_dotted_path_three_levels;
    case "dotted path missing section" test_dotted_path_missing;
    case "int values both forms" test_int_values_both_forms;
  ]

let () =
  Miniriot.run ~main:(fun ~args -> Test.Cli.main ~name:"config" ~tests ~args) ~args:Env.args ()
