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
  "0121_tuple_two.ml";
  "0122_tuple_three.ml";
  "0123_tuple_mixed_types.ml";
  "0124_tuple_nested.ml";
  "0125_tuple_with_expr.ml";
  "0126_tuple_with_ident.ml";
  "0127_tuple_with_call.ml";
  "0128_tuple_unit.ml";
  "0129_tuple_four.ml";
  "0130_tuple_large.ml";
  "0131_list_empty.ml";
  "0132_list_one.ml";
  "0133_list_three.ml";
  "0134_list_strings.ml";
  "0135_list_nested.ml";
  "0136_list_with_expr.ml";
  "0138_list_cons_mixed.ml";
  "0139_list_with_ident.ml";
  "0140_list_with_call.ml";
  "0141_seq_two.ml";
  "0142_seq_three.ml";
  "0143_seq_with_let.ml";
  "0145_seq_with_match.ml";
  "0146_seq_nested.ml";
  "0149_seq_long.ml";
  "0150_seq_unit.ml";
  "0156_nested_app_in_app.ml";
  "0157_nested_tuple_list.ml";
  "0158_nested_list_tuple.ml";
  "0159_complex_arithmetic.ml";
  "0160_complex_logic.ml";
  "0168_curry.ml";
  "0170_compose.ml";
  "0174_fun_tuple_params.ml";
  "0177_let_chain.ml";
  "0180_list_append.ml";
  "0181_cons_chain.ml";
  "0183_tuple_five.ml";
  "0184_fun_unit_param.ml";
  "0185_app_unit.ml";
  "0188_tuple_ten.ml";
  "0189_fun_five_params.ml";
  "0193_list_options.ml";
  "0194_list_tuples.ml";
  "0195_fun_make_tuple.ml";
  "0200_match_option_pair.ml";
  "0202_record_one_field.ml";
  "0203_record_two_fields.ml";
  "0204_record_three_fields.ml";
  "0205_record_nested.ml";
  "0206_record_with_expr.ml";
  "0207_record_field_access.ml";
  "0208_record_nested_access.ml";
  "0209_record_update.ml";
  "0210_record_update_multi.ml";
  "0212_record_in_tuple.ml";
  "0213_record_in_list.ml";
  "0216_array_empty.ml";
  "0221_array_access.ml";
  "0222_array_nested_access.ml";
  "0227_array_string_index.ml";
  "0229_array_make.ml";
  "0230_array_of_list.ml";
  "0231_string_concat.ml";
  "0232_string_concat_chain.ml";
  "0233_string_concat_expr.ml";
  "0237_list_append_chain.ml";
  "0238_list_append_expr.ml";
  "0239_list_append_empty.ml";
  "0241_ref_create.ml";
  "0244_ref_chain.ml";
  "0245_ref_in_expr.ml";
  "0247_ref_in_tuple.ml";
  "0248_ref_in_list.ml";
  "0249_ref_pattern.ml";
  "0250_ref_mutable.ml";
  "0271_begin_simple.ml";
  "0274_begin_sequence.ml";
  "0275_begin_as_paren.ml";
  "0281_for_simple.ml";
  "0282_for_downto.ml";
  "0283_for_nested.ml";
  "0285_while_simple.ml";
  "0286_while_true.ml";
  "0287_while_nested.ml";
  "0288_while_in_let.ml";
  "0292_lazy_force.ml";
  "0299_new_object.ml";
  "0300_downto_keyword.ml";
  "0301_bitwise_land.ml";
  "0302_bitwise_lor.ml";
  "0304_bitwise_lnot.ml";
  "0305_shift_lsl.ml";
  "0308_float_power.ml";
  "0309_float_ops_chain.ml";
  "0310_int_ops_chain.ml";
  "0311_comparison_ops.ml";
  "0312_physical_eq.ml";
  "0314_ampersand_single.ml";
  "0316_compose_ops.ml";
  "0317_reverse_app.ml";
  "0318_compose_right.ml";
  "0319_compose_left.ml";
  "0320_dollar_app.ml";
  "0321_module_ident.ml";
  "0322_module_nested.ml";
  "0323_module_three_level.ml";
  "0324_module_constructor.ml";
  "0325_module_type_path.ml";
  "0328_module_record_field.ml";
  "0329_module_functor_app.ml";
  "0331_module_let_open.ml";
  "0332_module_chained_access.ml";
  "0334_module_unpack.ml";
  "0335_module_pack_expr.ml";
  "0341_label_simple.ml";
  "0342_label_shorthand.ml";
  "0343_label_multiple.ml";
  "0344_label_with_unlabeled.ml";
  "0345_label_in_fun.ml";
  "0346_label_in_fun_multi.ml";
  "0349_optional_none.ml";
  "0350_optional_some.ml";
  "0352_optional_default.ml";
  "0353_optional_multi.ml";
  "0354_label_punning.ml";
  "0355_label_order.ml";
  "0360_label_method_call.ml";
  "0361_poly_var_simple.ml";
  "0362_poly_var_with_arg.ml";
  "0364_poly_var_nested.ml";
  "0366_poly_var_in_list.ml";
  "0367_poly_var_in_tuple.ml";
  "0378_poly_var_int_arg.ml";
  "0379_poly_var_string_arg.ml";
  "0380_poly_var_record_arg.ml";
  "0382_object_method.ml";
  "0383_object_val.ml";
  "0384_object_mutable_val.ml";
  "0385_object_private_method.ml";
  "0386_object_self.ml";
  "0387_object_inherit.ml";
  "0390_method_call_chain.ml";
  "0392_object_clone.ml";
  "0394_object_field_access.ml";
  "0396_extension_expr.ml";
  "0397_extension_percent.ml";
  "0398_attribute_expr.ml";
  "0400_attribute_item.ml";
  "0405_deeply_nested_tuples.ml";
  "0406_nested_function_calls.ml";
  "0408_nested_record_access.ml";
  "0409_complex_list_ops.ml";
  "0410_nested_array_access.ml";
  "0411_complex_arithmetic.ml";
  "0412_nested_bool_logic.ml";
  "0415_complex_record_update.ml";
  "0416_nested_object_calls.ml";
  "0417_complex_module_path.ml";
  "0421_let_destructure_tuple.ml";
  "0422_let_destructure_record.ml";
  "0424_let_constructor_pattern.ml";
  "0425_let_cons_pattern.ml";
  "0426_let_nested_pattern.ml";
  "0427_let_or_pattern.ml";
  "0429_let_lazy_pattern.ml";
  "0430_let_exception_pattern.ml";
  "0431_multiple_let_sequence.ml";
  "0433_let_function_type.ml";
  "0434_let_rec_mutual.ml";
  "0435_let_poly_type.ml";
  "0436_let_module_local.ml";
  "0437_let_open_local.ml";
  "0438_let_constraint.ml";
  "0439_let_first_class_module.ml";
  "0441_fun_multi_params.ml";
  "0442_fun_nested_functions.ml";
  "0443_fun_with_pattern.ml";
  "0444_fun_tuple_destructure.ml";
  "0446_fun_record_pattern.ml";
  "0447_fun_cons_pattern.ml";
  "0449_fun_optional_required.ml";
  "0450_fun_optional_default.ml";
  "0451_fun_labeled_punning.ml";
  "0453_fun_higher_order.ml";
  "0454_fun_returning_fun.ml";
  "0455_fun_returning_tuple.ml";
  "0456_fun_returning_record.ml";
  "0457_fun_with_ref.ml";
  "0458_fun_with_array.ml";
  "0460_fun_partial_application.ml";
  "0481_operator_precedence.ml";
  "0482_float_operator_mix.ml";
  "0483_bitwise_operator_chain.ml";
  "0484_shift_and_bitwise.ml";
  "0485_comparison_chain.ml";
  "0486_application_operators.ml";
  "0487_pipe_operators.ml";
  "0488_compose_operators.ml";
  "0489_list_cons_operators.ml";
  "0490_mixed_infix_prefix.ml";
  "0491_method_call_chain.ml";
  "0492_field_access_chain.ml";
  "0493_array_and_field.ml";
  "0494_complex_assignment.ml";
  "0495_sequence_with_side_effects.ml";
  "0496_nested_begin_end.ml";
  "0497_assert_with_expression.ml";
  "0498_loop_with_ref.ml";
  "0499_while_with_break_flag.ml";
  "0501_type_simple_int.ml";
  "0502_type_tuple.ml";
  "0503_type_function.ml";
  "0507_type_alias.ml";
  "0508_type_arrow_chain.ml";
  "0509_type_tuple_three.ml";
  "0512_complex_infix.ml";
  "0516_nested_records.ml";
  "0517_record_with_list.ml";
  "0521_deeply_nested_let.ml";
  "0526_ref_operations.ml";
  "0527_sequence_three.ml";
  "0528_nested_seq.ml";
  "0529_tuple_large.ml";
  "0531_record_complex.ml";
  "0532_record_update_nested.ml";
  "0533_field_access_chain.ml";
  "0534_mixed_operators.ml";
  "0535_operator_precedence.ml";
  "0536_bool_operators.ml";
  "0539_string_concat.ml";
  "0540_list_append.ml";
  "0541_pattern_tuple_nested.ml";
  "0542_pattern_list_cons.ml";
  "0544_pattern_constructor.ml";
  "0545_pattern_poly_variant.ml";
  "0548_fun_tuple_param.ml";
  "0551_nested_fun.ml";
  "0555_optional_args.ml";
  "0556_mixed_args.ml";
  "0557_label_punning.ml";
  "0560_labeled_arg_pipeline_tail.ml";
  "0561_type_list.ml";
  "0562_type_option.ml";
  "0570_type_poly_two.ml";
  "0571_type_arrow_multi.ml";
  "0573_type_nested_tuple.ml";
  "0574_type_list_tuple.ml";
  "0575_type_option_list.ml";
  "0576_type_function_tuple.ml";
  "0581_labeled_poly_variant_then_positional.ml";
  "0582_prec_mul_div.ml";
  "0583_labeled_string_then_or_chain.ml";
  "0584_prec_comp_bool.ml";
  "0585_prec_cons_append.ml";
  "0587_prec_app_infix.ml";
  "0592_prec_fun_app.ml";
  "0593_prec_ref_deref.ml";
  "0594_prec_field_app.ml";
  "0595_prec_index_app.ml";
  "0597_poly_variant_local_open_pattern_payload.ml";
  "0598_prec_bool_short.ml";
  "0599_prec_comp_chain.ml";
  "0602_begin_seq.ml";
  "0606_while_loop.ml";
  "0612_fun_labeled.ml";
  "0613_fun_optional.ml";
  "0616_constructor_multi.ml";
  "0618_poly_var_arg.ml";
  "0619_record_single.ml";
  "0620_record_multiple.ml";
  "0621_record_update_simple.ml";
  "0622_record_update_multi.ml";
  "0625_string_get.ml";
  "0661_type_bool.ml";
  "0662_type_string.ml";
  "0663_type_float.ml";
  "0664_type_unit.ml";
  "0665_type_char.ml";
  "0666_type_array.ml";
  "0667_type_ref.ml";
  "0668_type_option_int.ml";
  "0669_type_list_int.ml";
  "0670_type_tuple_pair.ml";
  "0671_type_arrow_simple.ml";
  "0676_type_poly_single.ml";
  "0677_type_poly_pair.ml";
  "0678_type_poly_triple.ml";
  "0681_type_nested_list.ml";
  "0682_type_nested_option.ml";
  "0684_type_tuple_arrow.ml";
  "0685_type_list_arrow.ml";
  "0686_type_option_arrow.ml";
  "0692_let_string.ml";
  "0693_let_bool.ml";
  "0694_let_unit.ml";
  "0700_let_in_simple.ml";
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
