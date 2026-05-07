open Std
open Std.Result.Syntax

let time_profile_xml =
  {|<?xml version="1.0"?>
<trace-query-result>
  <node xpath='//trace-toc[1]/run[1]/data[1]/table[12]'>
    <row>
      <weight id="w1" fmt="1.00 ms">1000000</weight>
      <tagged-backtrace id="bt1">
        <backtrace id="stack1">
          <frame id="leaf_a" name="leaf_a"/>
          <frame id="parent" name="parent"/>
          <frame id="root" name="root"/>
          <frame id="bogus_parent" name="0x4" addr="0x5"/>
          <frame id="bogus_root" name="0xffffff23eb3063fe" addr="0xffffff23eb3063ff"/>
        </backtrace>
      </tagged-backtrace>
    </row>
    <row>
      <weight ref="w1"/>
      <tagged-backtrace id="bt2">
        <backtrace id="stack2">
          <frame id="leaf_b" name="leaf_b"/>
          <frame ref="parent"/>
          <frame ref="root"/>
          <frame ref="bogus_parent"/>
          <frame ref="bogus_root"/>
        </backtrace>
      </tagged-backtrace>
    </row>
    <row>
      <weight ref="w1"/>
      <sentinel/>
    </row>
  </node>
</trace-query-result>|}

let assert_int = fun ~label ~expected ~actual ->
  if Int.equal expected actual then
    Ok ()
  else
    Error
      (label
      ^ ": expected "
      ^ Int.to_string expected
      ^ ", got "
      ^ Int.to_string actual)

let find_cost = fun name costs ->
  List.find costs ~fn:(fun (cost: Riot_trace.call_cost) -> String.equal cost.name name)

let require_cost = fun name costs ->
  match find_cost name costs with
  | Some cost -> Ok cost
  | None -> Error ("missing cost for " ^ name)

let test_summarizes_xctrace_rows_with_refs = fun _ctx ->
  let profile = Riot_trace.Internal.Xctrace.summarize_time_profile_xml time_profile_xml in
  let* () = assert_int ~label:"sample count" ~expected:2 ~actual:profile.sample_count in
  let* () =
    assert_int ~label:"total weight" ~expected:2_000_000 ~actual:profile.total_weight_ns
  in
  let* leaf_a = require_cost "leaf_a" profile.top_self in
  let* () =
    assert_int ~label:"leaf_a self weight" ~expected:1_000_000 ~actual:leaf_a.self_weight_ns
  in
  let* root = require_cost "root" profile.top_total in
  let* () =
    assert_int ~label:"root total weight" ~expected:2_000_000 ~actual:root.total_weight_ns
  in
  let* () =
    match find_cost "0xffffff23eb3063fe" profile.top_total with
    | None -> Ok ()
    | Some _ -> Error "raw no-binary frame should be skipped"
  in
  match profile.call_tree with
  | [
      Riot_trace.{
        name = "root";
        total_weight_ns = 2_000_000;
        children = [
          {
            name = "parent";
            total_weight_ns = 2_000_000;
            children = [ { name = "leaf_a"; _ }; { name = "leaf_b"; _ } ];
            _;
          };
        ];
        _;
      };
    ] ->
      Ok ()
  | _ -> Error "unexpected call tree shape"

let tests =
  Test.[
    case "summarizes xctrace rows with id refs" test_summarizes_xctrace_rows_with_refs;
  ]

let main ~args = Test.Cli.main ~name:"xctrace" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
