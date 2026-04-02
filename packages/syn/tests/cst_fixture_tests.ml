open Std
open Std.Data
open Syn

let approved_fixtures = [
  "0001_basic.ml";
  "0002_let_with_string.ml";
  "0003_let_with_bool.ml";
  "0005_addition.ml";
  "0010_int_octal.ml";
  "0011_int_binary.ml";
  "0012_float_simple.ml";
  "0016_string_empty.ml";
  "0017_string_simple.ml";
  "0018_char_simple.ml";
  "0019_bool_false.ml";
  "0020_unit.ml";
  "0021_ident_simple.ml";
  "0022_ident_underscore.ml";
  "0023_ident_prime.ml";
  "0024_ident_caps.ml";
  "0025_ident_digits.ml";
  "0029_paren_string.ml";
  "0030_paren_bool.ml";
  "0031_add_two_ints.ml";
  "0032_sub_two_ints.ml";
  "0033_mul_two_ints.ml";
  "0034_div_two_ints.ml";
  "0035_mod_two_ints.ml";
  "0036_add_chain.ml";
  "0037_mul_precedence.ml";
  "0038_paren_precedence.ml";
  "0039_mixed_ops.ml";
  "0040_float_add.ml";
  "0047_string_eq.ml";
  "0048_bool_eq.ml";
  "0049_comparison_chain.ml";
  "0050_nested_comparison.ml";
  "0053_and_chain.ml";
  "0054_or_chain.ml";
  "0055_mixed_logic.ml";
  "0058_not.ml";
  "0059_neg_expr.ml";
  "0060_not_comparison.ml";
  "0061_app_one_arg.ml";
  "0062_app_two_args.ml";
  "0063_app_three_args.ml";
  "0065_app_nested.ml";
  "0066_app_chain.ml";
  "0068_app_module_path.ml";
  "0069_app_constructor.ml";
  "0070_app_unit.ml";
  "0081_let_in_simple.ml";
  "0082_let_in_two_bindings.ml";
  "0083_let_in_use_outer.ml";
  "0084_let_in_nested.ml";
  "0087_let_in_complex_expr.ml";
  "0101_fun_one_param.ml";
  "0102_fun_two_params.ml";
  "0103_fun_three_params.ml";
  "0104_fun_pattern.ml";
  "0105_fun_tuple_param.ml";
  "0106_fun_nested.ml";
]

let fixture_root = Path.v "packages/syn/tests/fixtures"

let has_approved_fixture = fun path ->
  let basename = Path.basename path in
  List.exists (String.equal basename) approved_fixtures

let cst_snapshot_path = fun path ->
  Path.join (Path.dirname path) (Path.v (Path.basename path ^ ".expected_cst.json"))

let has_cst_snapshot = fun path ->
  match Path.extension path with
  | Some ".ml"
  | Some ".mli" ->
      if has_approved_fixture path then
        let snapshot_path = cst_snapshot_path path in
        let exists = Fs.exists snapshot_path |> Result.unwrap_or ~default:false in
        if exists then
          `keep
        else
          `skip
      else
        `skip
  | _ -> `skip

let format_parse_diagnostics = fun diagnostics ->
  diagnostics
  |> List.map Diagnostic.to_string
  |> String.concat "\n"

let test_cst_fixture = fun ~(ctx:Test.FixtureRunner.ctx) ->
  let source = Fs.read ctx.fixture_path |> Result.expect ~msg:"Failed to read CST fixture" in
  let parse_result = Syn.parse ~filename:ctx.fixture_path source in
  if parse_result.Parser.diagnostics != [] then
    Error ("unexpected parse diagnostics:\n" ^ format_parse_diagnostics parse_result.Parser.diagnostics)
  else
    match Syn.build_cst parse_result with
    | Ok cst ->
        Test.Snapshot.assert_with
          ~ctx:ctx.test
          ~render:(fun json -> Json.to_string_pretty json ^ "\n")
          ~actual:(Syn.CstJson.of_result (Ok cst))
    | Error (Syn.Parse_diagnostics diagnostics) ->
        Error ("unexpected build_cst parse diagnostics:\n" ^ format_parse_diagnostics diagnostics)
    | Error (Syn.Cst_builder_error error) ->
        Error ("unexpected CST builder error: "
        ^ error.Syn.CstBuilder.message
        ^ " @ "
        ^ Syn.SyntaxKind.to_string error.Syn.CstBuilder.syntax_kind
        ^ " in "
        ^ String.concat " > " error.Syn.CstBuilder.context)

let () =
  Actors.run
    ~main:(fun ~args ->
      let tests =
        Test.FixtureRunner.cases
          ()
          ~dir:fixture_root
          ~filter:has_cst_snapshot
          ~snapshot_path:(fun path -> Some (cst_snapshot_path path))
          ~run:(fun ctx -> test_cst_fixture ~ctx)
      in
      Test.Cli.main ~name:"syn-cst-fixtures" ~tests ~args)
    ~args:Env.args
    ()
