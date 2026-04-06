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

let manifest_to_json = function
  | TypeDecl.Alias manifest_type -> Data.Json.Object [
    ("tag", Data.Json.String "alias");
    ("type", Data.Json.String (TypePrinter.type_to_string manifest_type));
  ]
  | TypeDecl.PolyVariant { bound; tags; inherited } ->
      let bound =
        match bound with
        | TypeDecl.Exact -> "exact"
        | TypeDecl.UpperBound -> "upper"
        | TypeDecl.LowerBound -> "lower"
      in
      let tag_to_json (tag: TypeDecl.poly_variant_tag) =
        let fields = [ ("name", Data.Json.String tag.name) ] in
        let fields =
          match tag.payload_type with
          | Some payload_type -> fields
          @ [ ("payload_type", Data.Json.String (TypePrinter.type_to_string payload_type)) ]
          | None -> fields
        in
        Data.Json.Object fields
      in
      Data.Json.Object [
        ("tag", Data.Json.String "poly_variant");
        ("bound", Data.Json.String bound);
        ("tags", Data.Json.Array (List.map tag_to_json tags));
        (
          "inherited",
          Data.Json.Array (List.map
            (fun inherited -> Data.Json.String (TypePrinter.type_to_string inherited))
            inherited)
        );
      ]

let type_item_summary_to_json = function
  | ItemTree.Type type_item ->
      let fields = [
        ("tag", Data.Json.String "type");
        ("type_name", Data.Json.String type_item.declaration.type_name);
        ("param_count", Data.Json.Int (List.length type_item.declaration.param_ids));
        ("constructor_count", Data.Json.Int (List.length type_item.declaration.constructors));
        ("label_count", Data.Json.Int (List.length type_item.declaration.labels));
      ] in
      let fields =
        match type_item.declaration.manifest with
        | Some manifest -> fields @ [ ("manifest", manifest_to_json manifest) ]
        | None -> fields
      in
      Data.Json.Object fields
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
    ("items", Data.Json.Array (ItemTree.items item_tree |> List.map type_item_summary_to_json));
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

let expected_manifest_alias_lowering_json = Data.Json.Object [
  (
    "items",
    Data.Json.Array [
      Data.Json.Object [
        ("tag", Data.Json.String "type");
        ("type_name", Data.Json.String "name");
        ("param_count", Data.Json.Int 0);
        ("constructor_count", Data.Json.Int 0);
        ("label_count", Data.Json.Int 0);
        (
          "manifest",
          Data.Json.Object [
            ("tag", Data.Json.String "alias");
            ("type", Data.Json.String "string");
          ]
        );
      ];
      Data.Json.Object [ ("tag", Data.Json.String "value") ];
    ]
  );
  ("lowering_diagnostics", Data.Json.Array []);
]

let expected_arrow_manifest_alias_lowering_json = Data.Json.Object [
  (
    "items",
    Data.Json.Array [
      Data.Json.Object [
        ("tag", Data.Json.String "type");
        ("type_name", Data.Json.String "transform");
        ("param_count", Data.Json.Int 2);
        ("constructor_count", Data.Json.Int 0);
        ("label_count", Data.Json.Int 0);
        (
          "manifest",
          Data.Json.Object [
            ("tag", Data.Json.String "alias");
            ("type", Data.Json.String "'b -> ~step:('b -> 'a) -> ?fallback:'a -> 'a");
          ]
        );
      ];
    ]
  );
  ("lowering_diagnostics", Data.Json.Array []);
]

let expected_poly_variant_type_lowering_json = Data.Json.Object [
  (
    "items",
    Data.Json.Array [
      Data.Json.Object [
        ("tag", Data.Json.String "type");
        ("type_name", Data.Json.String "ansi");
        ("param_count", Data.Json.Int 0);
        ("constructor_count", Data.Json.Int 0);
        ("label_count", Data.Json.Int 0);
        (
          "manifest",
          Data.Json.Object [
            ("tag", Data.Json.String "poly_variant");
            ("bound", Data.Json.String "exact");
            (
              "tags",
              Data.Json.Array [
                Data.Json.Object [
                  ("name", Data.Json.String "ansi");
                  ("payload_type", Data.Json.String "int");
                ];
              ]
            );
            ("inherited", Data.Json.Array []);
          ]
        );
      ];
      Data.Json.Object [
        ("tag", Data.Json.String "type");
        ("type_name", Data.Json.String "rgb");
        ("param_count", Data.Json.Int 0);
        ("constructor_count", Data.Json.Int 0);
        ("label_count", Data.Json.Int 0);
        (
          "manifest",
          Data.Json.Object [
            ("tag", Data.Json.String "poly_variant");
            ("bound", Data.Json.String "exact");
            (
              "tags",
              Data.Json.Array [
                Data.Json.Object [
                  ("name", Data.Json.String "rgb");
                  ("payload_type", Data.Json.String "int * int * int");
                ];
              ]
            );
            ("inherited", Data.Json.Array []);
          ]
        );
      ];
      Data.Json.Object [
        ("tag", Data.Json.String "type");
        ("type_name", Data.Json.String "color");
        ("param_count", Data.Json.Int 0);
        ("constructor_count", Data.Json.Int 0);
        ("label_count", Data.Json.Int 0);
        (
          "manifest",
          Data.Json.Object [
            ("tag", Data.Json.String "poly_variant");
            ("bound", Data.Json.String "exact");
            ("tags", Data.Json.Array [ Data.Json.Object [ ("name", Data.Json.String "hex") ]; ]);
            ("inherited", Data.Json.Array [ Data.Json.String "ansi"; Data.Json.String "rgb"; ]);
          ]
        );
      ];
    ]
  );
  ("lowering_diagnostics", Data.Json.Array []);
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
  Test.Snapshot.assert_inline_json ~ctx ~actual:(actual_type_item_lowering_json report) ~expected:expected_abstract_type_lowering_json

let test_manifest_type_aliases_lower_to_type_items = fun ctx ->
  let source = String.concat "\n" [ "type name = string"; "let value = \"riot\""; "" ] in
  let report = Check.check_source ~filename:(Path.v "packages/typ/tests/type_alias_recovery.ml") source in
  Test.Snapshot.assert_inline_json ~ctx ~actual:(actual_type_item_lowering_json report) ~expected:expected_manifest_alias_lowering_json

let test_arrow_type_aliases_preserve_labels_during_lowering = fun ctx ->
  let source = String.concat
    "\n"
    [
      "type ('input, 'output) transform =";
      "  'input -> step:('input -> 'output) -> ?fallback:'output -> 'output";
      "";
    ] in
  let report = Check.check_source ~filename:(Path.v "packages/typ/tests/arrow_type_alias.ml") source in
  Test.Snapshot.assert_inline_json
    ~ctx
    ~actual:(actual_type_item_lowering_json report)
    ~expected:expected_arrow_manifest_alias_lowering_json

let test_poly_variant_type_declarations_lower_to_type_items = fun ctx ->
  let source = String.concat
    "\n"
    [
      "type ansi = [ `ansi of int ]";
      "type rgb = [ `rgb of int * int * int ]";
      "type color = [ ansi | rgb | `hex ]";
      "";
    ] in
  let report = Check.check_source ~filename:(Path.v "packages/typ/tests/poly_variant_types.ml") source in
  Test.Snapshot.assert_inline_json ~ctx ~actual:(actual_type_item_lowering_json report) ~expected:expected_poly_variant_type_lowering_json

let () =
  Actors.run
    ~main:(fun ~args ->
      let tests = [
        Test.case "fun cases preserve preceding parameters during lowering" test_fun_cases_preserve_preceding_parameters;
        Test.case "abstract type declarations lower to type items" test_abstract_type_declarations_lower_to_type_items;
        Test.case "manifest type aliases lower to type items" test_manifest_type_aliases_lower_to_type_items;
        Test.case
          "arrow type aliases preserve labels during lowering"
          test_arrow_type_aliases_preserve_labels_during_lowering;
        Test.case "polymorphic-variant type declarations lower to type items" test_poly_variant_type_declarations_lower_to_type_items;
      ] in
      Test.Cli.main ~name:"typ:lowering" ~tests ~args)
    ~args:Env.args
    ()
