open Std
open Typ

let label_to_json = function
  | BodyArena.Positional -> Data.Json.String "positional"
  | BodyArena.Labeled label -> Data.Json.String ("labeled:" ^ label)
  | BodyArena.Optional label -> Data.Json.String ("optional:" ^ label)

let pattern_name = fun arena pattern_id ->
  match BodyArena.find_pattern arena pattern_id with
  | Some { desc=BodyArena.PVar name; _ } -> name
  | Some _ -> "<non-var>"
  | None -> "<missing>"

let export_to_json = fun (name, scheme) ->
  Data.Json.Object [
    ("name", Data.Json.String name);
    ("scheme", Data.Json.String (TypePrinter.scheme_to_string scheme));
  ]

let actual_lowering_json = fun (report: Check_result.t) ->
  let body_arena = report.body_arena |> Option.expect ~msg:"expected lowered body arena in lowering test" in
  let choose_binding = BodyArena.bindings body_arena
  |> List.find_opt (fun (binding: BodyArena.binding) -> binding.name = Some "choose")
  |> Option.expect ~msg:"expected choose binding in lowering test" in
  let (parameter_spine, body_tag, case_count) =
    match BodyArena.find_expr body_arena choose_binding.value_id with
    | Some { desc=BodyArena.EFun (parameters, body_id); _ } ->
        let parameter_spine = parameters
        |> List.map
          (fun (parameter: BodyArena.function_parameter) ->
            Data.Json.Object [
              ("label", label_to_json parameter.label);
              ("pattern", Data.Json.String (pattern_name body_arena parameter.pattern_id));
            ]) in
        let (body_tag, case_count) =
          match BodyArena.find_expr body_arena body_id with
          | Some { desc=BodyArena.EMatch (_, cases); _ } -> ("match", List.length cases)
          | Some { desc=BodyArena.ETry (_, cases); _ } -> ("try", List.length cases)
          | Some _ -> ("other", 0)
          | None -> ("missing", 0)
        in
        (parameter_spine, body_tag, case_count)
    | _ -> ([], "missing", 0)
  in
  Data.Json.Object [
    ("body_tag", Data.Json.String body_tag);
    ("case_count", Data.Json.Int case_count);
    ("exports", Data.Json.Array (List.map export_to_json report.exports));
    ("parameter_spine", Data.Json.Array parameter_spine);
  ]

let expected_lowering_json = Data.Json.Object [
  ("body_tag", Data.Json.String "match");
  ("case_count", Data.Json.Int 2);
  (
    "exports",
    Data.Json.Array [
      Data.Json.Object [
        ("name", Data.Json.String "choose");
        ("scheme", Data.Json.String "int -> ~delta:int -> bool -> int");
      ];
      Data.Json.Object [ ("name", Data.Json.String "picked"); ("scheme", Data.Json.String "int"); ];
    ]
  );
  (
    "parameter_spine",
    Data.Json.Array [
      Data.Json.Object [
        ("label", Data.Json.String "positional");
        ("pattern", Data.Json.String "base");
      ];
      Data.Json.Object [
        ("label", Data.Json.String "labeled:delta");
        ("pattern", Data.Json.String "delta");
      ];
      Data.Json.Object [
        ("label", Data.Json.String "positional");
        ("pattern", Data.Json.String "$function_arg0");
      ];
    ]
  );
]

let test_fun_cases_preserve_preceding_parameters = fun ctx ->
  let source = String.concat
    "\n"
    [
      "let choose = fun base ~delta ->";
      "  function";
      "  | true -> base + delta";
      "  | false -> base";
      "";
      "let picked = choose 1 ~delta:2 true";
      "";
    ] in
  let report = Check.check_source ~filename:(Path.v "packages/typ/tests/fun_cases_with_params.ml") source in
  Test.Snapshot.assert_inline_json ~ctx ~actual:(actual_lowering_json report) ~expected:expected_lowering_json

let () =
  Actors.run
    ~main:(fun ~args ->
      let tests = [
        Test.case "fun cases preserve preceding parameters during lowering" test_fun_cases_preserve_preceding_parameters;
      ] in
      Test.Cli.main ~name:"typ:lowering" ~tests ~args)
    ~args:Env.args
    ()
