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

let type_item_summary_to_json = function
  | ItemTree.Type type_item ->
      Data.Json.Object [
        ("tag", Data.Json.String "type");
        ("type_name", Data.Json.String type_item.declaration.type_name);
        ("param_count", Data.Json.Int (List.length type_item.declaration.param_ids));
        ("constructor_count", Data.Json.Int (List.length type_item.declaration.constructors));
        ("label_count", Data.Json.Int (List.length type_item.declaration.labels));
      ]
  | ItemTree.Unsupported unsupported_item ->
      Data.Json.Object [
        ("tag", Data.Json.String "unsupported");
        ("summary", Data.Json.String unsupported_item.summary);
      ]
  | ItemTree.Value _ ->
      Data.Json.Object [ ("tag", Data.Json.String "value") ]
  | ItemTree.Exception _ ->
      Data.Json.Object [ ("tag", Data.Json.String "exception") ]
  | ItemTree.Open _ ->
      Data.Json.Object [ ("tag", Data.Json.String "open") ]
  | ItemTree.Include _ ->
      Data.Json.Object [ ("tag", Data.Json.String "include") ]
  | ItemTree.ModuleAlias _ ->
      Data.Json.Object [ ("tag", Data.Json.String "module_alias") ]

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

let actual_type_item_lowering_json = fun (report: Check_result.t) ->
  let item_tree = report.item_tree |> Option.expect ~msg:"expected lowered item tree in lowering test" in
  Data.Json.Object [
    (
      "items",
      Data.Json.Array (ItemTree.items item_tree |> List.map type_item_summary_to_json)
    );
    (
      "lowering_diagnostics",
      Data.Json.Array (List.map Diagnostic.to_json report.lowering_diagnostics)
    );
  ]

let expected_abstract_type_lowering_json = Data.Json.Object [
  (
    "items",
    Data.Json.Array [
      Data.Json.Object [
        ("tag", Data.Json.String "type");
        ("type_name", Data.Json.String "t");
        ("param_count", Data.Json.Int 0);
        ("constructor_count", Data.Json.Int 0);
        ("label_count", Data.Json.Int 0);
      ];
      Data.Json.Object [
        ("tag", Data.Json.String "type");
        ("type_name", Data.Json.String "pair");
        ("param_count", Data.Json.Int 2);
        ("constructor_count", Data.Json.Int 0);
        ("label_count", Data.Json.Int 0);
      ];
      Data.Json.Object [ ("tag", Data.Json.String "value") ];
    ]
  );
  ("lowering_diagnostics", Data.Json.Array []);
]

let expected_type_alias_recovery_json = Data.Json.Object [
  (
    "items",
    Data.Json.Array [
      Data.Json.Object [
        ("tag", Data.Json.String "unsupported");
        ("summary", Data.Json.String "TYPE_DECL");
      ];
      Data.Json.Object [ ("tag", Data.Json.String "value") ];
    ]
  );
  (
    "lowering_diagnostics",
    Data.Json.Array [
      Data.Json.Object [
        ("id", Data.Json.String "TYP1001");
        ("name", Data.Json.String "unsupported-syntax");
        ("severity", Data.Json.String "error");
        (
          "message",
          Data.Json.String "unsupported structure item lowered using placeholder item: TYPE_DECL"
        );
        (
          "syntax_span",
          Data.Json.Object [
            ("start", Data.Json.Int 0);
            ("end", Data.Json.Int 18);
          ]
        );
        ("syntax_kind", Data.Json.String "TYPE_DECL");
        ("context", Data.Json.String "structure_item");
        ("recovery", Data.Json.String "placeholder_item");
        ("reason", Data.Json.Null);
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

let test_abstract_type_declarations_lower_to_type_items = fun ctx ->
  let source = String.concat "\n" [ "type t"; "type ('a, 'b) pair"; "let value = ()"; "" ] in
  let report = Check.check_source ~filename:(Path.v "packages/typ/tests/abstract_types.ml") source in
  Test.Snapshot.assert_inline_json
    ~ctx
    ~actual:(actual_type_item_lowering_json report)
    ~expected:expected_abstract_type_lowering_json

let test_unsupported_type_aliases_lower_to_placeholder_items = fun ctx ->
  let source = String.concat "\n" [ "type name = string"; "let value = \"riot\""; "" ] in
  let report = Check.check_source ~filename:(Path.v "packages/typ/tests/type_alias_recovery.ml") source in
  Test.Snapshot.assert_inline_json
    ~ctx
    ~actual:(actual_type_item_lowering_json report)
    ~expected:expected_type_alias_recovery_json

let () =
  Actors.run
    ~main:(fun ~args ->
      let tests = [
        Test.case "fun cases preserve preceding parameters during lowering" test_fun_cases_preserve_preceding_parameters;
        Test.case
          "abstract type declarations lower to type items"
          test_abstract_type_declarations_lower_to_type_items;
        Test.case
          "unsupported type aliases lower to placeholder items"
          test_unsupported_type_aliases_lower_to_placeholder_items;
      ] in
      Test.Cli.main ~name:"typ:lowering" ~tests ~args)
    ~args:Env.args
    ()
