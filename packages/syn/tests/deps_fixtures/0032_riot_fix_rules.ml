let name = "std"

let rules = fun () ->
  [
    No_stdlib.rule ();
    Prefer_bang_equal_inequality.rule ();
    No_double_list_rev.rule ();
    Prefer_iter_over_ignored_map.rule ();
    Prefer_list_is_empty.rule ();
    Prefer_option_map_over_manual_match.rule ();
    Prefer_result_map_over_manual_match.rule ();
    Upgrade_test_ctx_callbacks.rule ();
  ]

let explanations = fun () ->
  No_stdlib.explanations ()
  @ Prefer_bang_equal_inequality.explanations ()
  @ No_double_list_rev.explanations ()
  @ Prefer_iter_over_ignored_map.explanations ()
  @ Prefer_list_is_empty.explanations ()
  @ Prefer_option_map_over_manual_match.explanations ()
  @ Prefer_result_map_over_manual_match.explanations ()
  @ Upgrade_test_ctx_callbacks.explanations ()
