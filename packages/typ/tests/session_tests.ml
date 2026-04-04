open Std
open Typ

let export_names = function
  | Some (FileSummary.TrustedExport { exports })
  | Some (FileSummary.ErroredExport { exports }) ->
      List.map fst exports
  | Some FileSummary.NoExport
  | None ->
      []

let inferred_type_at = fun snapshot source_id offset ->
  Query.type_at snapshot source_id (Position.make ~offset)
  |> function
  | Some ty -> Some (TypePrinter.type_to_string ty)
  | None -> None

let test_source_id_stays_stable_across_updates = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, source_id) =
    Session.create_source
      session
      ~kind:Source.File
      ~origin:(Source.Label "stable.ml")
      ~text:"let x = 1"
  in
  let snapshot_before = Session.snapshot session in
  let session = Session.update_source_text session source_id ~text:"let y = 2" in
  let snapshot_after = Session.snapshot session in
  let before_names = export_names (Query.export_of snapshot_before source_id) in
  let after_names = export_names (Query.export_of snapshot_after source_id) in
  let () = Test.assert_equal ~expected:[ "x" ] ~actual:before_names in
  let () = Test.assert_equal ~expected:[ "y" ] ~actual:after_names in
  Ok ()

let test_snapshots_remain_immutable_after_updates = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, source_id) =
    Session.create_source
      session
      ~kind:Source.File
      ~origin:(Source.Label "immutable.ml")
      ~text:"let x = 1"
  in
  let snapshot_before = Session.snapshot session in
  let session = Session.update_source_text session source_id ~text:"let x = true" in
  let snapshot_after = Session.snapshot session in
  let before_type = inferred_type_at snapshot_before source_id 8 in
  let after_type = inferred_type_at snapshot_after source_id 8 in
  let () = Test.assert_equal ~expected:(Some "int") ~actual:before_type in
  let () = Test.assert_equal ~expected:(Some "bool") ~actual:after_type in
  Ok ()

let test_type_at_uses_smallest_indexed_expression = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let source = "let id x = x\nlet answer = id 42\n" in
  let (session, source_id) =
    Session.create_source
      session
      ~kind:Source.File
      ~origin:(Source.Label "type_at.ml")
      ~text:source
  in
  let snapshot = Session.snapshot session in
  let callee_type = inferred_type_at snapshot source_id 26 in
  let argument_type = inferred_type_at snapshot source_id 29 in
  let () = Test.assert_equal ~expected:(Some "int -> int") ~actual:callee_type in
  let () = Test.assert_equal ~expected:(Some "int") ~actual:argument_type in
  Ok ()

let () =
  Actors.run
    ~main:(fun ~args ->
      let tests = [
        Test.case "source id stays stable across updates" test_source_id_stays_stable_across_updates;
        Test.case "snapshots remain immutable after updates" test_snapshots_remain_immutable_after_updates;
        Test.case "type_at uses smallest indexed expression" test_type_at_uses_smallest_indexed_expression;
      ] in
      Test.Cli.main ~name:"typ:session" ~tests ~args)
    ~args:Env.args
    ()
