open Std

let sample_ml = Path.v "sample.ml"

let workspace_files = [
  Path.v "packages/syn/src/token_cursor.mli";
  Path.v "packages/std/src/int.ml";
  Path.v "packages/std/src/bool.ml";
  Path.v "packages/std/src/option.ml";
  Path.v "packages/std/src/result.ml";
]

let parse_ml = fun source -> Syn.parse ~filename:sample_ml source

let parse_mli = fun source -> Syn.parse ~filename:(Path.v "sample.mli") source

let parse_file = fun path ->
  let source = Fs.read path |> Result.expect ~msg:"fixture file should exist" in
  Syn.parse ~filename:path source

let with_tempdir = fun prefix fn ->
  match Fs.with_tempdir ~prefix fn with
  | Ok result -> result
  | Error err -> Error (IO.error_message err)

let capture_json_event = fun ~root event ->
  let buffer = IO.Buffer.create 128 in
  let writer =
    let module Write = struct
      type t = IO.Buffer.t

      type err = unit

      let write = fun buffer ~buf ->
        IO.Buffer.add_string buffer buf;
        Ok (String.length buf)

      let write_owned_vectored = fun _buffer ~bufs:_ -> unimplemented ()

      let flush = fun _buffer -> Ok ()
    end in
    IO.Writer.of_write_src (module Write) buffer
  in
  Krasny.Report.write_json_event ~writer ~root event |> Result.expect ~msg:"failed to serialize json event";
  IO.Buffer.contents buffer |> String.trim

let assert_json_timestamp_field = fun json ->
  match Data.Json.get_field "timestamp" json with
  | Some (Data.Json.String timestamp) ->
      Test.assert_true (String.contains timestamp "T");
      Test.assert_true (String.ends_with ~suffix:"Z" timestamp)
  | Some _ ->
      panic "timestamp field should be a JSON string"
  | None ->
      panic "timestamp field missing"

let assert_json_duration_ms_field = fun json ->
  match Data.Json.get_field "duration_ms" json with
  | Some (Data.Json.Int duration_ms) -> Test.assert_true (duration_ms >= 0)
  | Some _ -> panic "duration_ms field should be a JSON int"
  | None -> panic "duration_ms field missing"

let assert_idempotent = fun ~source ~msg ->
  let first = parse_ml source |> Krasny.format |> Result.expect ~msg in
  let second = parse_ml first |> Krasny.format |> Result.expect ~msg:"formatted output should reformat" in
  Test.assert_equal ~expected:first ~actual:second

let assert_roundtrip_hash = fun path ->
  let parsed = parse_file path in
  let original_hash = Krasny.syntax_hash parsed in
  let formatted = Krasny.format parsed |> Result.expect ~msg:"selected repo files should format" in
  let reparsed = Syn.parse ~filename:path formatted in
  let reparsed_hash = Krasny.syntax_hash reparsed in
  Test.assert_equal ~expected:original_hash ~actual:reparsed_hash

let tests = [
  Test.case "format returns the original source for a simple implementation"
    (fun _ctx ->
      let source = "let x = 1 + 2\n" in
      let actual = parse_ml source |> Krasny.format |> Result.expect ~msg:"simple implementations should format" in
      Test.assert_equal ~expected:source ~actual;
      Ok ());
  Test.case "format adds a final newline to non-empty output"
    (fun _ctx ->
      let source = "let x = 1 + 2" in
      let actual = parse_ml source |> Krasny.format |> Result.expect ~msg:"formatted output should end with a final newline" in
      Test.assert_equal ~expected:"let x = 1 + 2\n" ~actual;
      Ok ());
  Test.case "format keeps empty files empty"
    (fun _ctx ->
      let actual = parse_ml "" |> Krasny.format |> Result.expect ~msg:"empty files should still format" in
      Test.assert_equal ~expected:"" ~actual;
      Ok ());
  Test.case "format keeps explicit fun rhs bindings explicit"
    (fun _ctx ->
      let source = "let id = fun x -> x\n" in
      let actual = parse_ml source |> Krasny.format |> Result.expect ~msg:"explicit fun rhs bindings should format" in
      Test.assert_equal ~expected:source ~actual;
      Ok ());
  Test.case "format renders fun body trivia from token-leading trivia"
    (fun _ctx ->
      let source = {|let with_comment = fun x -> (* keep *) x
let with_doc = fun x -> (** keep *) x
|}
      in
      let actual = parse_ml source |> Krasny.format |> Result.expect ~msg:"fun-body comment and docstring trivia should not need source reparsing" in
      Test.assert_equal
        ~expected:{|let with_comment = fun x ->
  (* keep *)
  x

let with_doc = fun x ->
  (** keep *)
  x
|}
        ~actual;
      Ok ());
  Test.case "format renders if-branch trivia from token-leading trivia"
    (fun _ctx ->
      let source = {|let classify = fun flag -> if flag then value (* keep before else *) else other
let nested = fun flag other -> if flag then value else (* keep before branch *) if other then (* nested *) next else last
|}
      in
      let actual = parse_ml source |> Krasny.format |> Result.expect ~msg:"if/else comment trivia should not need source reparsing" in
      Test.assert_equal
        ~expected:{|let classify = fun flag ->
  if flag then
    value
    (* keep before else *)
  else
    other

let nested = fun flag other ->
  if flag then
    value
  else
    (* keep before branch *)
    if other then
      (* nested *)
      next
    else
      last
|}
        ~actual;
      Ok ());
  Test.case "format renders let rhs and body trivia from token-leading trivia"
    (fun _ctx ->
      let source = {|let run =
  let value = (* keep before rhs *) compute in
  (* keep before body *)
  use value
|}
      in
      let actual = parse_ml source |> Krasny.format |> Result.expect ~msg:"let rhs/body trivia should not need source reparsing" in
      Test.assert_equal
        ~expected:{|let run =
  let value =
    (* keep before rhs *)
    compute
  in
  (* keep before body *)
  use value
|}
        ~actual;
      Ok ());
  Test.case "format renders sequence and let-operator trivia from tokens"
    (fun _ctx ->
      let source = {|let run = fun () -> first (* keep after first *); (* keep before second *) second; (** keep before third *) third
let bind =
  let* value = (* keep before bound value *) compute in
  (* keep before body *)
  finish value
|}
      in
      let actual = parse_ml source |> Krasny.format |> Result.expect ~msg:"sequence and binding-operator trivia should not need source reparsing" in
      Test.assert_equal
        ~expected:{|let run = fun () ->
  first;
  (* keep after first *)
  (* keep before second *)
  second;
  (** keep before third *)
  third

let bind =
  let* value =
    (* keep before bound value *)
    compute
  in
  (* keep before body *)
  finish value
|}
        ~actual;
      Ok ());
  Test.case "format match cases from structure, not arrow source newlines"
    (fun _ctx ->
      let source = {|let render = function
  | A ->
      value
|}
      in
      let actual = parse_ml source |> Krasny.format |> Result.expect ~msg:"match case layout should not preserve source newlines after arrows" in
      Test.assert_equal
        ~expected:{|let render =
  function
  | A -> value
|}
        ~actual;
      Ok ());
  Test.case "format polymorphic variant heads from explicit tag tokens"
    (fun _ctx ->
      let source = {|let classify = function
  | `Ok value -> value
  | `Error -> fallback

let value = `Ok 1
|}
      in
      let actual = parse_ml source |> Krasny.format |> Result.expect ~msg:"polymorphic variant heads should format from tag tokens" in
      Test.assert_equal ~expected:source ~actual;
      Ok ());
  Test.case "format quoted core type variables from explicit sigil tokens"
    (fun _ctx ->
      let source = {|type 'a t = 'a list

val id : 'a -> 'a
|}
      in
      let actual = parse_mli source |> Krasny.format |> Result.expect ~msg:"quoted core type variables should format from sigil and name tokens" in
      Test.assert_equal ~expected:source ~actual;
      Ok ());
  Test.case "format core type alias binders from explicit sigil tokens"
    (fun _ctx ->
      let source = {|val cast : ('a list as 'whole) -> 'whole
|}
      in
      let actual = parse_mli source |> Krasny.format |> Result.expect ~msg:"core type alias binders should format from explicit sigil tokens" in
      Test.assert_equal ~expected:source ~actual;
      Ok ());
  Test.case "format record fields without name-length multiline forcing"
    (fun _ctx ->
      let source = {|type t = {
  this_is_a_pretty_long_record_field_name : int list;
}

type u = {
  mutable this_is_a_pretty_long_record_field_name : int list;
}
|}
      in
      let actual = parse_ml source |> Krasny.format |> Result.expect ~msg:"record fields should not break after ':' just because the field name is long" in
      Test.assert_equal ~expected:source ~actual;
      Ok ());
  Test.case "desugar typed named parameters without duplicating inner annotations"
    (fun _ctx ->
      let source = {|type 'a t = 'a list

let map (type a b) (iter : a t) ~(fn : a -> b) : b t = failwith "todo"
|}
      in
      let expected = {|type 'a t = 'a list

let map : type a b. a t -> fn:(a -> b) -> b t = fun iter ~fn -> failwith "todo"
|}
      in
      let actual = parse_ml source |> Krasny.format |> Result.expect ~msg:"typed named parameters should move to the synthesized outer annotation" in
      Test.assert_equal ~expected ~actual;
      Ok ());
  Test.case "keep typed parameters in the binding header when annotation synthesis declines"
    (fun _ctx ->
      let source = {|let pick x : int = x
|}
      in
      let actual = parse_ml source |> Krasny.format |> Result.expect ~msg:"typed parameters should stay in the binding header when outer annotation synthesis does not apply" in
      Test.assert_equal ~expected:source ~actual;
      Ok ());
  Test.case "format index expressions from explicit delimiter tokens"
    (fun _ctx ->
      let source = {|let x = s.[0]
let y = a.(0)
let z = x.%(0)
|}
      in
      let actual = parse_ml source |> Krasny.format |> Result.expect ~msg:"index expressions should format from CST-carried delimiters, not token replay" in
      Test.assert_equal ~expected:source ~actual;
      Ok ());
  Test.case "format signed literal patterns from structural sign tokens"
    (fun _ctx ->
      let source = {|let classify = function | -1 -> `Neg | +2 -> `Pos | _ -> `Other
|}
      in
      let actual = parse_ml source |> Krasny.format |> Result.expect ~msg:"signed literal patterns should format from CST-carried sign tokens" in
      Test.assert_equal
        ~expected:{|let classify =
  function
  | -1 -> `Neg
  | +2 -> `Pos
  | _ -> `Other
|}
        ~actual;
      Ok ());
  Test.case "format leaves a blank line before docstring-led top-level items"
    (fun _ctx ->
      let source = {|let first = 1
(** doc for second *)
let second = 2
|}
      in
      let actual = parse_ml source |> Krasny.format |> Result.expect ~msg:"top-level docstring-led items should stay visually separated" in
      Test.assert_equal
        ~expected:{|let first = 1

(** doc for second *)
let second = 2
|}
        ~actual;
      Ok ());
  Test.case "format leaves a blank line before docstring-led signature items"
    (fun _ctx ->
      let source = {|val first : int
(** doc for second *)
val second : int
|}
      in
      let actual = parse_mli source |> Krasny.format |> Result.expect ~msg:"signature docstring-led items should stay visually separated" in
      Test.assert_equal
        ~expected:{|val first: int

(** doc for second *)
val second: int
|}
        ~actual;
      Ok ());
  Test.case "format operator expressions and patterns from explicit operator tokens"
    (fun _ctx ->
      let source = {|let op = ( + )
let is_plus = function | ( + ) -> true | _ -> false
|}
      in
      let actual = parse_ml source |> Krasny.format |> Result.expect ~msg:"operator expressions and patterns should format from CST-carried operator tokens" in
      Test.assert_equal
        ~expected:{|let op = ( + )

let is_plus =
  function
  | ( + ) -> true
  | _ -> false
|}
        ~actual;
      Ok ());
  Test.case "format infix and prefix expression operators from explicit operator tokens"
    (fun _ctx ->
      let source = {|let negate value = ~-value
let ready = flag01 && flag02 && flag03 && flag04 && flag05 && flag06 && flag07 && flag08 && flag09
|}
      in
      let actual = parse_ml source |> Krasny.format |> Result.expect ~msg:"infix and prefix expressions should format from CST-carried operator tokens" in
      Test.assert_equal
        ~expected:{|let negate value = ~-value

let ready =
  flag01
  && flag02
  && flag03
  && flag04
  && flag05
  && flag06
  && flag07
  && flag08
  && flag09
|}
        ~actual;
      Ok ());
  Test.case "format singleton list patterns with explicit formatter spacing"
    (fun _ctx ->
      let compact_source = {|let classify = function
  | [value] -> hit
|}
      in
      let spaced_source = {|let classify = function
  | [ value ] -> hit
|}
      in
      let expected = {|let classify =
  function
  | [ value ] -> hit
|}
      in
      let actual_compact = parse_ml compact_source |> Krasny.format |> Result.expect ~msg:"singleton list patterns should not preserve compact source spacing" in
      let actual_spaced = parse_ml spaced_source |> Krasny.format |> Result.expect ~msg:"singleton list patterns should keep the explicit formatter style" in
      Test.assert_equal ~expected ~actual:actual_compact;
      Test.assert_equal ~expected ~actual:actual_spaced;
      Ok ());
  Test.case "format if conditions from infix structure, not token scans"
    (fun _ctx ->
      let source = {|let decide =
  if a&&b
     || c
  then hit else miss
|}
      in
      let actual = parse_ml source |> Krasny.format |> Result.expect ~msg:"if conditions should format from infix expression structure" in
      Test.assert_equal
        ~expected:{|let decide =
  if a && b || c then
    hit
  else
    miss
|}
        ~actual;
      Ok ());
  Test.case "format binding values from structure, not source newlines"
    (fun _ctx ->
      let source = {|let wrapped =
  (
    value
  )
|}
      in
      let actual = parse_ml source |> Krasny.format |> Result.expect ~msg:"binding layout should not preserve multiline source for a simple wrapped value" in
      Test.assert_equal
        ~expected:{|let wrapped = (value)
|}
        ~actual;
      Ok ());
  Test.case "format simple string bindings inline from ordinary simplicity checks"
    (fun _ctx ->
      let source = {|let message =
  (
    "ok"
  )
let bind =
  let* value = "ok" in
  finish value
|}
      in
      let actual = parse_ml source |> Krasny.format |> Result.expect ~msg:"simple string bindings should stay inline without a separate override" in
      Test.assert_equal
        ~expected:{|let message = ("ok")

let bind =
  let* value = "ok" in
  finish value
|}
        ~actual;
      Ok ());
  Test.case "format keeps simple applies inline even when identifiers contain keywords"
    (fun _ctx ->
      let source = "let handler = use function_handler\n" in
      let actual = parse_ml source |> Krasny.format |> Result.expect ~msg:"simple applies should not sniff keyword substrings" in
      Test.assert_equal ~expected:source ~actual;
      Ok ());
  Test.case "format normalizes simple applies from structure, not source newlines"
    (fun _ctx ->
      let source = {|let call =
  run
    first
    second
|}
      in
      let actual = parse_ml source |> Krasny.format |> Result.expect ~msg:"simple applies should not preserve source newlines" in
      Test.assert_equal
        ~expected:{|let call = run first second
|}
        ~actual;
      Ok ());
  Test.case "format rewrites parameterized let bindings between formatted lets"
    (fun _ctx ->
      let source = "(* intro *)\nlet x = 1 + 2\nlet f x = x + 1\nlet y = 3 + 4\n" in
      let actual = parse_ml source |> Krasny.format |> Result.expect ~msg:"parameterized let bindings should lower through explicit fun syntax" in
      Test.assert_equal
        ~expected:"(* intro *)\nlet x = 1 + 2\n\nlet f x = x + 1\n\nlet y = 3 + 4\n"
        ~actual;
      Ok ());
  Test.case "format keeps mixed trivia and unsupported items parseable"
    (fun _ctx ->
      let source = {|open Std
type t =
  | A
  | B
(* keep with x *)
let x = 1 + 2
let y = 3 + 4
|}
      in
      assert_idempotent ~source ~msg:"mixed implementation files should format";
      Ok ());
  Test.case "format keeps tuple/list/array docs idempotent"
    (fun _ctx ->
      let source = {|let tuple_value = (left_side_identifier, right_side_identifier, final_identifier)
let list_value = [first_item_identifier; second_item_identifier; third_item_identifier]
let array_value = [|first_item_identifier; second_item_identifier; third_item_identifier|]
|}
      in
      assert_idempotent ~source ~msg:"collection expressions should stay stable";
      Ok ());
  Test.case "format canonicalizes multiline list apply arguments"
    (fun _ctx ->
      let source = {|let cmd =
  f [
    first_item;
    second_item;
  ]
|}
      in
      let formatted = parse_ml source |> Krasny.format |> Result.expect ~msg:"list arguments should format" in
      Test.assert_equal
        ~expected:{|let cmd = f [ first_item; second_item ]
|}
        ~actual:formatted;
      Ok ());
  Test.case "format normalizes let-open bodies from structure, not source newlines"
    (fun _ctx ->
      let source = {|let answer =
  let open Option in
  value
|}
      in
      let formatted = parse_ml source |> Krasny.format |> Result.expect ~msg:"let-open expressions should format structurally" in
      Test.assert_equal
        ~expected:{|let answer =
  let open Option in value
|}
        ~actual:formatted;
      Ok ());
  Test.case "format open bang from explicit bang tokens in ml and mli"
    (fun _ctx ->
      let source = "open! Inline\n" in
      let actual_ml = parse_ml source |> Krasny.format |> Result.expect ~msg:"implementation open! should render from bang_token" in
      let actual_mli = parse_mli source |> Krasny.format |> Result.expect ~msg:"signature open! should render from bang_token" in
      Test.assert_equal ~expected:source ~actual:actual_ml;
      Test.assert_equal ~expected:source ~actual:actual_mli;
      Ok ());
  Test.case "format local binding equals policy for boolean chains and pipelines"
    (fun _ctx ->
      let source = {|let run flag01 flag02 flag03 flag04 flag05 flag06 flag07 flag08 flag09 value =
  let ready = flag01 && flag02 && flag03 && flag04 && flag05 && flag06 && flag07 && flag08 && flag09 in
  let staged = value |> stage01 |> stage02 |> stage03 |> stage04 |> stage05 |> stage06 in
  ready, staged
|}
      in
      let actual = parse_ml source |> Krasny.format |> Result.expect ~msg:"local binding equals policy should stay stable while heuristics are isolated" in
      Test.assert_equal
        ~expected:{|let run flag01 flag02 flag03 flag04 flag05 flag06 flag07 flag08 flag09 value =
  let ready =
    flag01
    && flag02
    && flag03
    && flag04
    && flag05
    && flag06
    && flag07
    && flag08
    && flag09
  in
  let staged =
    value
    |> stage01
    |> stage02
    |> stage03
    |> stage04
    |> stage05
    |> stage06
  in
  ready, staged
|}
        ~actual;
      Ok ());
  Test.case "format local binding infix threshold around inline-after-equals cutoff"
    (fun _ctx ->
      let source = {|let totals a b c d e f g h i =
  let total8 = a + b + c + d + e + f + g + h in
  let total9 = a + b + c + d + e + f + g + h + i in
  total8, total9
|}
      in
      let actual = parse_ml source |> Krasny.format |> Result.expect ~msg:"local binding infix threshold should stay explicit and stable" in
      Test.assert_equal
        ~expected:{|let totals a b c d e f g h i =
  let total8 = a + b + c + d + e + f + g + h in
  let total9 =
    a
    + b
    + c
    + d
    + e
    + f
    + g
    + h
    + i
  in
  total8, total9
|}
        ~actual;
      Ok ());
  Test.case "format simple apply rhs by shape, not comment scans"
    (fun _ctx ->
      let source = {|let run x =
  let value = f (* keep *) x in
  value
|}
      in
      let actual = parse_ml source |> Krasny.format |> Result.expect ~msg:"simple apply rhs layout should not depend on scanning raw token trivia" in
      Test.assert_equal ~expected:source ~actual;
      assert_idempotent ~source ~msg:"comment-bearing simple apply rhs should stay stable";
      Ok ());
  Test.case "format binding-operator equals policy with explicit fun and multiline values"
    (fun _ctx ->
      let source = {|let bind flag01 flag02 flag03 flag04 flag05 flag06 flag07 flag08 flag09 value =
  let* callback = fun x -> x in
  let* ready = flag01 && flag02 && flag03 && flag04 && flag05 && flag06 && flag07 && flag08 && flag09 in
  let+ staged = value |> stage01 |> stage02 |> stage03 |> stage04 |> stage05 |> stage06 in
  callback staged, ready
|}
      in
      let actual = parse_ml source |> Krasny.format |> Result.expect ~msg:"binding-operator equals policy should stay aligned with local bindings" in
      Test.assert_equal
        ~expected:{|let bind flag01 flag02 flag03 flag04 flag05 flag06 flag07 flag08 flag09 value =
  let* callback = fun x -> x in
  let* ready =
    flag01
    && flag02
    && flag03
    && flag04
    && flag05
    && flag06
    && flag07
    && flag08
    && flag09
  in
  let+ staged =
    value
    |> stage01
    |> stage02
    |> stage03
    |> stage04
    |> stage05
    |> stage06
  in
  callback staged, ready
|}
        ~actual;
      Ok ());
  Test.case "format recursive local bindings with multiline bodies"
    (fun _ctx ->
      let source = {|let outer value =
  let rec loop n = if n = 0 then value else loop (n - 1) in
  loop 3
|}
      in
      let actual = parse_ml source |> Krasny.format |> Result.expect ~msg:"recursive local bindings should keep multiline bodies explicit" in
      Test.assert_equal
        ~expected:{|let outer value =
  let rec loop n =
    if n = 0 then
      value
    else
      loop (n - 1)
  in
  loop 3
|}
        ~actual;
      Ok ());
  Test.case "format breaks long tuples without source-length sniffing"
    (fun _ctx ->
      let source = {|let tuple_value = (left_side_identifier, right_side_identifier, final_identifier, fourth_identifier)
|}
      in
      let formatted = parse_ml source |> Krasny.format |> Result.expect ~msg:"long tuples should still break from doc layout" in
      Test.assert_equal
        ~expected:{|let tuple_value =
  ( left_side_identifier,
    right_side_identifier,
    final_identifier,
    fourth_identifier
  )
|}
        ~actual:formatted;
      Ok ());
  Test.case "verify treats normalized punctuation and parens as safe"
    (fun _ctx ->
      with_tempdir "krasny_runner_verify_semantic_hash"
        (fun tmpdir ->
          let parens = Path.(tmpdir / Path.v "parens.ml") in
          let listy = Path.(tmpdir / Path.v "listy.ml") in
          let recordy = Path.(tmpdir / Path.v "recordy.ml") in
          let varianty = Path.(tmpdir / Path.v "varianty.ml") in
          Fs.write "let x = configure ~style:(Style.Grow)\n" parens |> Result.expect ~msg:"write parens";
          Fs.write
            {|let cmd =
  f [
    first_item;
    second_item;
  ]
|}
            listy |> Result.expect ~msg:"write listy";
          Fs.write
            {record_fixture|let explanation =
  Api.Explanation.
    {
      rule_id = package_rule_id;
      message = "Use != instead of <> for inequality.";
      body = {|body|};
    }
|record_fixture}
            recordy |> Result.expect ~msg:"write recordy";
          Fs.write
            "type severity = Error | Warning | Info | Hint\n\n\
               type color = [ ansi | rgb | xyz ]\n"
            varianty |> Result.expect ~msg:"write varianty";
          let result = Krasny.Runner.run_verify [ parens; listy; recordy; varianty ] in
          Test.assert_equal ~expected:4 ~actual:result.summary.total_files;
          Test.assert_equal ~expected:4 ~actual:result.summary.would_reformat;
          Test.assert_equal ~expected:0 ~actual:result.summary.unsafe_to_format;
          Ok ()));
  Test.case "format keeps function and match lowering idempotent"
    (fun _ctx ->
      let source = {|let f = function x, y -> x + y
let g = function 0 -> "zero" | _ -> "other"
let h = fun x -> match x with 0 -> "zero" | _ -> "other"
|}
      in
      assert_idempotent ~source ~msg:"function and match forms should stay stable";
      Ok ());
  Test.case "format keeps let/if/sequence layouts idempotent"
    (fun _ctx ->
      let source = {|let x =
  if a then (
    b;
    c)
  else d

let y =
  let rec f n = if n = 0 then 1 else n * f (n - 1) in
  f 5
|}
      in
      assert_idempotent ~source ~msg:"control-flow layouts should stay stable";
      Ok ());
  Test.case "format keeps typed and labeled bindings idempotent"
    (fun _ctx ->
      let source = {|let delimiter_of_keyword : keyword -> delimiter option = function | Begin -> Some BeginEnd | _ -> None
let label_arg = f ~y
let optional_arg = f ?y
let optional_fun = fun ?(y = 0) -> y + 1
|}
      in
      assert_idempotent ~source ~msg:"typed/labeled forms should stay stable";
      Ok ());
  Test.case "format keeps structural named parameters with defaults idempotent"
    (fun _ctx ->
      let source = {|let configure ?(timeout : int = 30) ?retry:retries ~point:{ x; y } ~limit:seconds () =
  (timeout, retries, x, y, seconds)
|}
      in
      assert_idempotent ~source ~msg:"named parameter defaults, renames, and destructuring should format structurally";
      Ok ());
  Test.case "format keeps signature operator values structural"
    (fun _ctx ->
      let source = {|val ( = ) : 'a -> 'a -> bool
val (mod) : int -> int -> int
val ( := ) : 'a ref -> 'a -> unit
|}
      in
      let formatted = parse_mli source |> Krasny.format |> Result.expect ~msg:"operator value declarations should format structurally" in
      Test.assert_equal
        ~expected:{|val ( = ) : 'a -> 'a -> bool

val ( mod ) : int -> int -> int

val ( := ) : 'a ref -> 'a -> unit
|}
        ~actual:formatted;
      Ok ());
  Test.case "format keeps alias patterns idempotent"
    (fun _ctx ->
      let source = {|open Std

let request = fun (Conn conn as c) () -> ()
|}
      in
      assert_idempotent ~source ~msg:"alias patterns should stay stable";
      Ok ());
  Test.case "format keeps constructor parameter patterns idempotent"
    (fun _ctx ->
      let source = {|open Std

let request = fun (Conn conn) () -> ()
|}
      in
      assert_idempotent ~source ~msg:"constructor parameter patterns should not gain extra parentheses";
      Ok ());
  Test.case "format keeps typed first-class module patterns idempotent"
    (fun _ctx ->
      let source = {|let run_comparison index (module R : Reporter.Intf.Intf) comp = (index, comp)
|}
      in
      assert_idempotent ~source ~msg:"typed first-class module patterns should lower structurally";
      Ok ());
  Test.case "format keeps first-class module expressions idempotent"
    (fun _ctx ->
      let source = {|open Std

module Protocol = struct
  module Http1 = struct end
end

let packed = (module Protocol.Http1)
|}
      in
      assert_idempotent ~source ~msg:"first-class module expressions should stay stable";
      Ok ());
  Test.case "format keeps class declaration items idempotent"
    (fun _ctx ->
      let source = {|class ['a] service : object
  val mutable state : int
  method private run : int
end =
  object (self)
    val mutable state = 0
    method private run = self#state
    (** keep body doc *)
    [%%foo]
  end
|}
      in
      assert_idempotent ~source ~msg:"class declaration items should lower structurally";
      Ok ());
  Test.case "format keeps class type declaration items idempotent"
    (fun _ctx ->
      let source = {|class type ['a] service = object
  inherit base
  val mutable state : int
  method private run : 'a
  (** keep body doc *)
  [%%foo]
end

class worker : int -> service
|}
      in
      let first = parse_mli source |> Krasny.format |> Result.expect ~msg:"class type declaration items should lower structurally" in
      let second = parse_mli first |> Krasny.format |> Result.expect ~msg:"formatted class type declarations should reformat" in
      Test.assert_equal ~expected:first ~actual:second;
      Ok ());
  Test.case "format keeps shortcut class declaration modifiers idempotent"
    (fun _ctx ->
      let source = {|class%foo [@foo] x = x
class type%foo [@foo] y = y
|}
      in
      assert_idempotent ~source ~msg:"class declaration shell modifiers should stay structural";
      Ok ());
  Test.case "format keeps structural signature items idempotent"
    (fun _ctx ->
      let source = {|[@@@warning "-32"]

type t +=
  | Added of int

exception Parse_error of string
exception Nested = Std.Result.Error
|}
      in
      let formatted = parse_mli source |> Krasny.format |> Result.expect ~msg:"signature attributes, type extensions, and exceptions should format structurally" in
      let reparsed = parse_mli formatted |> Krasny.format |> Result.expect ~msg:"formatted signature items should reformat" in
      Test.assert_equal ~expected:formatted ~actual:reparsed;
      Ok ());
  Test.case "format floating attributes from structural payload items"
    (fun _ctx ->
      let source = "[@@@warning    \"-32\"]\n" in
      let actual = parse_ml source |> Krasny.format |> Result.expect ~msg:"floating attributes should render from structural payload items" in
      Test.assert_equal ~expected:"[@@@warning \"-32\"]\n" ~actual;
      Ok ());
  Test.case "format floating extension items structurally"
    (fun _ctx ->
      let structure_source = {|[%%foo]
[%%bar let x = 1]
|}
      in
      let signature_source = {|[%%foo]
[%%bar val x : int]
|}
      in
      let actual_structure = parse_ml structure_source |> Krasny.format |> Result.expect ~msg:"floating structure extensions should render structurally from the extension shell and payload" in
      let actual_signature = parse_mli signature_source |> Krasny.format |> Result.expect ~msg:"floating signature extensions should render structurally from the extension shell and payload" in
      Test.assert_equal ~expected:structure_source ~actual:actual_structure;
      Test.assert_equal ~expected:signature_source ~actual:actual_signature;
      assert_idempotent ~source:structure_source ~msg:"floating structure extensions should stay stable across repeated formatting";
      let reformatted_signature = parse_mli actual_signature |> Krasny.format |> Result.expect ~msg:"floating signature extensions should stay stable across repeated formatting" in
      Test.assert_equal ~expected:actual_signature ~actual:reformatted_signature;
      Ok ());
  Test.case "format module-expression and module-type extensions structurally"
    (fun _ctx ->
      let source = {|module type S = [%foo]
module M = [%foo]
|}
      in
      let actual = parse_ml source |> Krasny.format |> Result.expect ~msg:"module-expression and module-type extensions should render from the structural extension shell" in
      Test.assert_equal ~expected:source ~actual;
      assert_idempotent ~source ~msg:"module-expression and module-type extensions should stay stable";
      Ok ());
  Test.case "format keeps structural core types idempotent"
    (fun _ctx ->
      let source = {|val use : #service -> M.(t list) -> < close : unit -> unit; next : int >
|}
      in
      let formatted = parse_mli source |> Krasny.format |> Result.expect ~msg:"class, local-open, and object core types should format structurally" in
      let reparsed = parse_mli formatted |> Krasny.format |> Result.expect ~msg:"formatted core types should reformat" in
      Test.assert_equal ~expected:formatted ~actual:reparsed;
      Ok ());
  Test.case "format first-class module types from structural module-type docs"
    (fun _ctx ->
      let source = {|type packed = (module   Transport   with   type t = int)
type extended = (module [%foo])
type payload = (module [%foo: S])
|}
      in
      let actual = parse_ml source |> Krasny.format |> Result.expect ~msg:"first-class module types should format from structural module-type rendering" in
      Test.assert_equal
        ~expected:{|type packed = (module Transport with type t = int)
type extended = (module [%foo])
type payload = (module [%foo: S])
|}
        ~actual;
      Ok ());
  Test.case "format shared core-type attributes keeps opaque payload tokens"
    (fun _ctx ->
      let source = "type t = int [@deprecated   \"use other\"]\n" in
      let actual = parse_ml source |> Krasny.format |> Result.expect ~msg:"shared core-type attributes should render from opaque payload tokens" in
      Test.assert_equal ~expected:source ~actual;
      Ok ());
  Test.case "format shared attribute payload infix expressions opaquely"
    (fun _ctx ->
      let source = "type t = int [@foo 1 + 2]\n" in
      let actual = parse_ml source |> Krasny.format |> Result.expect ~msg:"shared attribute payload infix expressions should render opaquely" in
      Test.assert_equal ~expected:source ~actual;
      Ok ());
  Test.case "format expression attributes keeps opaque payload tokens"
    (fun _ctx ->
      let source = "let _ = value [@foo   1  +  2]\n" in
      let actual = parse_ml source |> Krasny.format |> Result.expect ~msg:"expression attributes should render from opaque payload tokens" in
      Test.assert_equal ~expected:source ~actual;
      Ok ());
  Test.case "format ordinary pattern-payload attributes structurally"
    (fun _ctx ->
      let source = {|let simple = 1 [@foo? Some y]
let guarded = 1 [@foo? Some y when y > 0]
|}
      in
      let actual = parse_ml source |> Krasny.format |> Result.expect ~msg:"ordinary pattern-payload attributes should render structurally" in
      Test.assert_equal ~expected:source ~actual;
      assert_idempotent ~source ~msg:"ordinary pattern-payload attributes should stay stable";
      Ok ());
  Test.case "format parenthesizes attributed non-atomic expressions"
    (fun _ctx ->
      let source = {|let constructor = Some 0 [@inline always]
let apply = I64.logor b (I64.shift_left b 32) [@inline always]
let infix = mask land (mask - 1) [@inline always]
|}
      in
      let expected = {|let constructor = (Some 0) [@inline always]

let apply = (I64.logor b (I64.shift_left b 32)) [@inline always]

let infix = (mask land (mask - 1)) [@inline always]
|}
      in
      let actual = parse_ml source |> Krasny.format |> Result.expect ~msg:"postfix expression attributes should preserve attributed apply and infix payloads" in
      Test.assert_equal ~expected ~actual;
      assert_idempotent ~source ~msg:"postfix attributed apply and infix expressions should stay stable";
      Ok ());
  Test.case "format plain object expressions structurally"
    (fun _ctx ->
      let source = {|let empty = object end
let methods =
  object
    method m = 1
    method! private x = 3 [@@foo]
  end
let fields =
  object
    val mutable y = 1
    val virtual z : t [@@foo]
  end
let self_ =
  object (self)
    method m = self#n
  end
let inherited =
  object
    inherit c [@@foo]
    initializer setup [@@foo]
  end
let typed =
  (object
     method m = 1
   end
    : < m : int >)
|}
      in
      let expected = {|let empty =
  object end

let methods =
  object
    method m = 1
    method! private x = 3 [@@foo]
  end

let fields =
  object
    val mutable y = 1
    val virtual z : t [@@foo]
  end

let self_ =
  object (self)
    method m = self#n
  end

let inherited =
  object
    inherit c [@@foo]
    initializer setup [@@foo]
  end

let typed =
  (object
     method m = 1
   end: <
     m : int
   >)
|}
      in
      let actual = parse_ml source |> Krasny.format |> Result.expect ~msg:"plain object expressions should render structurally from the CST" in
      Test.assert_equal ~expected ~actual;
      assert_idempotent ~source ~msg:"plain object expressions should stay stable across repeated formatting";
      Ok ());
  Test.case "format object bodies preserve terminal trivia"
    (fun _ctx ->
      let source = {|let empty = object
  (* trailing comment *)
  (** trailing docstring *)
  method run = 1
  (* trailing comment *)
  (** trailing docstring *)
  end
|}
      in
      let expected = {|let empty =
  object
    (* trailing comment *)
    (** trailing docstring *)
    method run = 1
    (* trailing comment *)
    (** trailing docstring *)
  end
|}
      in
      let actual = parse_ml source |> Krasny.format |> Result.expect ~msg:"object bodies should preserve trailing in-body comments and docstrings" in
      Test.assert_equal ~expected ~actual;
      assert_idempotent ~source ~msg:"object-body terminal trivia should stay stable across repeated formatting";
      Ok ());
  Test.case "format object extension members structurally"
    (fun _ctx ->
      let source = {|let extended =
  object
    [%%foo]
    [%%bar let x = 1]
  end
|}
      in
      let expected = {|let extended =
  object
    [%%foo]
    [%%bar let x = 1]
  end
|}
      in
      let actual = parse_ml source |> Krasny.format |> Result.expect ~msg:"object extension members should render structurally from the CST" in
      Test.assert_equal ~expected ~actual;
      assert_idempotent ~source ~msg:"object extension members should stay stable across repeated formatting";
      Ok ());
  Test.case "format trailing variant comments with explicit separator policy"
    (fun _ctx ->
      let source = "type t =\n  | A (* comment *)\n" in
      let actual = parse_ml source |> Krasny.format |> Result.expect ~msg:"trailing variant comments should format from explicit trivia separators" in
      Test.assert_equal ~expected:"type t =\n  | A (* comment *)\n" ~actual;
      Ok ());
  Test.case "format trailing variant docstrings with explicit separator policy"
    (fun _ctx ->
      let source = "type t =\n  | A (** doc *)\n" in
      let actual = parse_ml source |> Krasny.format |> Result.expect ~msg:"trailing variant docstrings should format from explicit trivia separators" in
      Test.assert_equal ~expected:"type t =\n  | A\n  (** doc *)\n" ~actual;
      Ok ());
  Test.case "format fails for signature-bodied first-class module types"
    (fun _ctx ->
      let source = {|type packed = (module sig
  type t
end)
|}
      in
      match parse_ml source |> Krasny.format with
      | Ok _ -> panic "signature-bodied first-class module types should fail until they have a structural formatter"
      | Error _ -> Ok ());
  Test.case "format core-type extensions structurally"
    (fun _ctx ->
      let source = "val use : [%foo: int]\n" in
      let actual = parse_mli source |> Krasny.format |> Result.expect ~msg:"core-type extensions should render structurally from the extension shell and payload" in
      Test.assert_equal ~expected:source ~actual;
      let reparsed = parse_mli actual |> Krasny.format |> Result.expect ~msg:"core-type extensions should stay stable across repeated formatting" in
      Test.assert_equal ~expected:actual ~actual:reparsed;
      Ok ());
  Test.case "format keeps structural patterns idempotent"
    (fun _ctx ->
      let source = {|let unpack = function
  | (module M) -> ()
  | (M.(Some x) as whole) -> whole
  | (lazy y : t) -> y
|}
      in
      assert_idempotent ~source ~msg:"first-class module, local-open, alias, and typed patterns should format structurally";
      Ok ());
  Test.case "format keeps polymorphic-variant inherit patterns idempotent"
    (fun _ctx ->
      let source = "let x = match y with #color -> 1\n" in
      let actual = parse_ml source |> Krasny.format |> Result.expect ~msg:"polymorphic-variant inherit patterns should render from the structural path" in
      Test.assert_equal
        ~expected:{|let x =
  match y with
  | #color -> 1
|}
        ~actual;
      assert_idempotent ~source ~msg:"polymorphic-variant inherit patterns should stay stable";
      Ok ());
  Test.case "format fails for typed first-class-module patterns"
    (fun _ctx ->
      let source = {|let unpack = function
  | (module M : S) -> ()
|}
      in
      match parse_ml source |> Krasny.format with
      | Ok _ -> panic "typed first-class-module patterns should fail formatting instead of preserving source"
      | Error _ -> Ok ());
  Test.case "format pattern extensions structurally"
    (fun _ctx ->
      let source = {|let unpack = function
  | [%foo? Some x] -> x
  | [%foo? Some y when y > 0] -> y
|}
      in
      let actual = parse_ml source |> Krasny.format |> Result.expect ~msg:"pattern extensions should render structurally from the extension shell and payload" in
      Test.assert_equal
        ~expected:{|let unpack =
  function
  | [%foo? Some x] -> x
  | [%foo? Some y when y > 0] -> y
|}
        ~actual;
      assert_idempotent ~source ~msg:"pattern extensions should stay stable across repeated formatting";
      Ok ());
  Test.case "format keeps structural imperative and module expressions idempotent"
    (fun _ctx ->
      let source = {|let packed = (module M : S)
let guarded ready = assert ready
let delayed compute = lazy (compute ())
let loop cond body = while cond () do body () done
let count () = for i = 10 downto 0 do print_int i done
let call obj = obj#run
let make () = new queue
let cast value = (value : source :> target)
let widen value = (value :> target)
let update next count = {< current = next; count >}
|}
      in
      assert_idempotent ~source ~msg:"module-pack, imperative, coercion, and object-override expressions should format structurally";
      Ok ());
  Test.case "format expression extensions structurally"
    (fun _ctx ->
      let source = {|let generated = [%foo]
let computed = [%test 42]
let typed = [%foo: int]
let nested = [%foo let x = 1]
|}
      in
      let actual = parse_ml source |> Krasny.format |> Result.expect ~msg:"expression extensions should render structurally from the extension shell and payload" in
      Test.assert_equal ~expected:source ~actual;
      assert_idempotent ~source ~msg:"expression extensions should stay stable across repeated formatting";
      Ok ());
  Test.case "format unreachable expressions structurally"
    (fun _ctx ->
      let source = {|let absurd maybe =
  match maybe with
  | Some value -> value
  | None -> .
|}
      in
      let actual = parse_ml source |> Krasny.format |> Result.expect ~msg:"unreachable expressions should render structurally from the CST token" in
      Test.assert_equal ~expected:source ~actual;
      assert_idempotent ~source ~msg:"unreachable expressions should stay stable across repeated formatting";
      Ok ());
  Test.case "format keeps typed and polymorphic expressions structural"
    (fun _ctx ->
      let source = {|let typed value = (value : source)
let shaped handler = (handler : < run : int >)
let poly = ((fun x -> x) : 'a. 'a -> 'a)
|}
      in
      assert_idempotent ~source ~msg:"typed and polymorphic expressions should lower through structural core-type rendering";
      Ok ());
  Test.case "format keeps nested module bodies structural"
    (fun _ctx ->
      let source = {|module type S = sig
  (** x *)
  val x : int
end

module M = struct
  let x = 1
end
|}
      in
      assert_idempotent ~source ~msg:"nested signature and structure bodies should lower from structural item streams";
      Ok ());
  Test.case "format keeps grouped GADT type declarations structural"
    (fun _ctx ->
      let source = {|type _ expr =
  | Int : int expr
and packed =
  | Packed : int expr -> packed
|}
      in
      assert_idempotent ~source ~msg:"grouped GADT type declarations should lower structurally instead of preserving source";
      Ok ());
  Test.case "format inline record constructors from structure, not source newlines"
    (fun _ctx ->
      let source = {|type t =
  | A of {
      x : int;
      y : int;
    }
  | B
|}
      in
      let actual = parse_ml source |> Krasny.format |> Result.expect ~msg:"inline record constructors should format structurally" in
      Test.assert_equal
        ~expected:{|type t =
  | A of { x : int; y : int }
  | B
|}
        ~actual;
      Ok ());
  Test.case "format keeps boolean if conditions with matches idempotent"
    (fun _ctx ->
      let source = {|open Std

let status_char mode summary =
  if
    match mode with
    | Runner.Check -> summary.needs_formatting = 0 && summary.failed_files = 0
    | Runner.Verify -> summary.unsafe_to_format = 0 && summary.failed_files = 0
    | Runner.Format -> summary.failed_files = 0
  then
    "."
  else
    "!"
|}
      in
      assert_idempotent ~source ~msg:"boolean match conditions should stay stable";
      Ok ());
  Test.case "format keeps top-level lowered fun phrases separated"
    (fun _ctx ->
      let source = {|open Std

let ( .??[] ) () () = ();;

(()).??[(();
         ())]
;;
|}
      in
      assert_idempotent ~source ~msg:"top-level expression phrases should stay outside lowered fun bindings";
      Ok ());
  Test.case "format keeps top-level phrase separators structural"
    (fun _ctx ->
      let source = {|let project x = x
;;
1
;;
module M = struct end
|}
      in
      let actual = parse_ml source |> Krasny.format |> Result.expect ~msg:"top-level phrase separators should come from source-file tokens, not source gaps" in
      Test.assert_equal
        ~expected:{|let project x = x;;

1;;

module M = struct

end
|}
        ~actual;
      Ok ());
  Test.case "format preserves syntax hash for selected codebase files"
    (fun _ctx ->
      List.iter assert_roundtrip_hash workspace_files;
      Ok ());
  Test.case "runner skips hidden and build directories"
    (fun _ctx ->
      with_tempdir "krasny_runner_scan"
        (fun tmpdir ->
          let visible_ml = Path.(tmpdir / Path.v "visible.ml") in
          let nested_dir = Path.(tmpdir / Path.v "nested") in
          let nested_mli = Path.(nested_dir / Path.v "visible.mli") in
          let hidden_dir = Path.(tmpdir / Path.v ".hidden") in
          let build_dir = Path.(tmpdir / Path.v "_build") in
          Fs.create_dir_all nested_dir |> Result.expect ~msg:"create nested";
          Fs.create_dir_all hidden_dir |> Result.expect ~msg:"create hidden";
          Fs.create_dir_all build_dir |> Result.expect ~msg:"create build";
          Fs.write "let x = 1\n" visible_ml |> Result.expect ~msg:"write visible";
          Fs.write "val x : int\n" nested_mli |> Result.expect ~msg:"write nested";
          Fs.write "let hidden = 1\n" Path.(hidden_dir / Path.v "hidden.ml") |> Result.expect ~msg:"write hidden";
          Fs.write "let built = 1\n" Path.(build_dir / Path.v "built.ml") |> Result.expect ~msg:"write build";
          let files = Krasny.Runner.collect_ocaml_files ~roots:[ tmpdir ] () |> List.map Path.to_string in
          let expected = [ Path.to_string visible_ml; Path.to_string nested_mli ]
          |> List.sort String.compare in
          let actual = List.sort String.compare files in
          Test.assert_equal ~expected ~actual;
          Ok ()));
  Test.case "runner skips ignored subtrees during collection"
    (fun _ctx ->
      with_tempdir "krasny_runner_ignore_tree"
        (fun tmpdir ->
          let keep = Path.(tmpdir / Path.v "keep.ml") in
          let fixtures_dir = Path.(tmpdir / Path.v "tests" / Path.v "fixtures") in
          let ignored = Path.(fixtures_dir / Path.v "fixture.ml") in
          Fs.create_dir_all fixtures_dir |> Result.expect ~msg:"create fixtures dir";
          Fs.write "let kept = 1\n" keep |> Result.expect ~msg:"write keep";
          Fs.write "let ignored = 1\n" ignored |> Result.expect ~msg:"write ignored";
          let files =
            Krasny.Runner.collect_ocaml_files ~roots:[ tmpdir ]
              ~should_ignore:(fun path ->
                String.contains (Path.to_string path) "fixtures")
              ()
            |> List.map Path.to_string
          in
          Test.assert_equal ~expected:[ Path.to_string keep ] ~actual:files;
          Ok ()));
  Test.case "runner reports formatting status and emits json events"
    (fun _ctx ->
      with_tempdir "krasny_runner_check"
        (fun tmpdir ->
          let formatted = Path.(tmpdir / Path.v "formatted.ml") in
          let needs = Path.(tmpdir / Path.v "needs.ml") in
          Fs.write "let x = 1 + 2\n" formatted |> Result.expect ~msg:"write formatted";
          Fs.write "let x = 1 + 2\nlet f x = x + 1\n" needs |> Result.expect ~msg:"write needs";
          let result = Krasny.Runner.run_checks [ formatted; needs ] in
          Test.assert_equal ~expected:2 ~actual:result.summary.total_files;
          Test.assert_equal ~expected:1 ~actual:result.summary.already_formatted;
          Test.assert_equal ~expected:1 ~actual:result.summary.needs_formatting;
          Test.assert_equal ~expected:0 ~actual:result.summary.would_reformat;
          Test.assert_equal ~expected:0 ~actual:result.summary.unsafe_to_format;
          Test.assert_equal ~expected:0 ~actual:result.summary.failed_files;
          let needs_result =
            result.files
            |> List.find_opt
              (fun file_result ->
                String.equal (Path.to_string file_result.Krasny.Runner.file) (Path.to_string needs))
            |> Option.expect ~msg:"needs result missing"
          in
          let json = capture_json_event ~root:tmpdir (Krasny.Report.File needs_result)
          |> Data.Json.of_string
          |> Result.expect ~msg:"parse event json" in
          let open Data.Json in
            Test.assert_equal ~expected:(Some (String "file")) ~actual:(get_field "type" json);
            assert_json_timestamp_field json;
            assert_json_duration_ms_field json;
            Test.assert_equal ~expected:(Some (String "needs.ml")) ~actual:(get_field "file" json);
            Test.assert_equal
              ~expected:(Some (String "needs_formatting"))
              ~actual:(get_field "status" json);
            Ok ()));
  Test.case "verify reports files that would reformat safely"
    (fun _ctx ->
      with_tempdir "krasny_runner_verify"
        (fun tmpdir ->
          let formatted = Path.(tmpdir / Path.v "formatted.ml") in
          let needs = Path.(tmpdir / Path.v "needs.ml") in
          Fs.write "let x = 1 + 2\n" formatted |> Result.expect ~msg:"write formatted";
          Fs.write "let x = 1 + 2\nlet f x = x + 1\n" needs |> Result.expect ~msg:"write needs";
          let result = Krasny.Runner.run_verify [ formatted; needs ] in
          Test.assert_equal ~expected:2 ~actual:result.summary.total_files;
          Test.assert_equal ~expected:1 ~actual:result.summary.already_formatted;
          Test.assert_equal ~expected:0 ~actual:result.summary.needs_formatting;
          Test.assert_equal ~expected:1 ~actual:result.summary.would_reformat;
          Test.assert_equal ~expected:0 ~actual:result.summary.unsafe_to_format;
          Test.assert_equal ~expected:0 ~actual:result.summary.failed_files;
          let needs_result =
            result.files
            |> List.find_opt
              (fun file_result ->
                String.equal (Path.to_string file_result.Krasny.Runner.file) (Path.to_string needs))
            |> Option.expect ~msg:"verify result missing"
          in
          let json = capture_json_event ~root:tmpdir (Krasny.Report.File needs_result)
          |> Data.Json.of_string
          |> Result.expect ~msg:"parse event json" in
          let open Data.Json in
            Test.assert_equal
              ~expected:(Some (String "would_reformat"))
              ~actual:(get_field "status" json);
            Ok ()));
  Test.case "format rewrites files in place and reports formatted status"
    (fun _ctx ->
      with_tempdir "krasny_runner_format"
        (fun tmpdir ->
          let formatted = Path.(tmpdir / Path.v "formatted.ml") in
          let needs = Path.(tmpdir / Path.v "needs.ml") in
          Fs.write "let x = 1 + 2\n" formatted |> Result.expect ~msg:"write formatted";
          Fs.write "let x = 1 + 2\nlet f x = x + 1\n" needs |> Result.expect ~msg:"write needs";
          let result = Krasny.Runner.run_format [ formatted; needs ] in
          Test.assert_equal ~expected:2 ~actual:result.summary.total_files;
          Test.assert_equal ~expected:1 ~actual:result.summary.already_formatted;
          Test.assert_equal ~expected:1 ~actual:result.summary.formatted_files;
          Test.assert_equal ~expected:0 ~actual:result.summary.failed_files;
          let formatted_source = Fs.read needs |> Result.expect ~msg:"read formatted output" in
          Test.assert_equal ~expected:"let x = 1 + 2\n\nlet f x = x + 1\n" ~actual:formatted_source;
          let file_result =
            result.files
            |> List.find_opt
              (fun file_result ->
                String.equal (Path.to_string file_result.Krasny.Runner.file) (Path.to_string needs))
            |> Option.expect ~msg:"format result missing"
          in
          let json = capture_json_event ~root:tmpdir (Krasny.Report.File file_result)
          |> Data.Json.of_string
          |> Result.expect ~msg:"parse event json" in
          let open Data.Json in
            Test.assert_equal ~expected:(Some (String "formatted")) ~actual:(get_field "status" json);
            Ok ()));
  Test.case "streaming runner skips ignored files"
    (fun _ctx ->
      with_tempdir "krasny_runner_ignore"
        (fun tmpdir ->
          let keep = Path.(tmpdir / Path.v "keep.ml") in
          let fixtures_dir = Path.(tmpdir / Path.v "tests" / Path.v "fixtures") in
          let ignored = Path.(fixtures_dir / Path.v "fixture.ml") in
          Fs.create_dir_all fixtures_dir |> Result.expect ~msg:"create fixtures dir";
          Fs.write "let kept = 1\n" keep |> Result.expect ~msg:"write keep";
          Fs.write "let ignored = 1\n" ignored |> Result.expect ~msg:"write ignored";
          let seen = cell [] in
          let result =
            Krasny.Runner.run_checks_streaming ~concurrency:1 ~roots:[ tmpdir ]
              ~should_ignore:(fun path ->
                String.contains (Path.to_string path) "fixtures")
              ~on_result:(fun file_result -> seen := Path.to_string file_result.file :: !seen)
              ()
          in
          Test.assert_equal ~expected:[ Path.to_string keep ] ~actual:(List.rev !seen);
          Test.assert_equal ~expected:1 ~actual:result.summary.total_files;
          Ok ()));
  Test.case "streaming runner scans roots and streams file results"
    (fun _ctx ->
      with_tempdir "krasny_runner_stream"
        (fun tmpdir ->
          let formatted = Path.(tmpdir / Path.v "formatted.ml") in
          let nested_dir = Path.(tmpdir / Path.v "nested") in
          let needs = Path.(nested_dir / Path.v "needs.mli") in
          Fs.create_dir_all nested_dir |> Result.expect ~msg:"create nested";
          Fs.write "let x = 1 + 2\n" formatted |> Result.expect ~msg:"write formatted";
          Fs.write "val x : int\n" needs |> Result.expect ~msg:"write needs";
          let seen = cell [] in
          let result =
            Krasny.Runner.run_checks_streaming
              ~concurrency:1
              ~roots:[ tmpdir ]
              ~on_result:(fun file_result -> seen := Path.to_string file_result.file :: !seen)
              ()
          in
          let actual = List.sort String.compare !seen in
          let expected = [ Path.to_string formatted; Path.to_string needs ] |> List.sort String.compare in
          Test.assert_equal ~expected ~actual;
          Test.assert_equal ~expected:2 ~actual:result.summary.total_files;
          Test.assert_equal ~expected:2 ~actual:result.summary.already_formatted;
          Test.assert_equal ~expected:0 ~actual:result.summary.needs_formatting;
          Test.assert_equal ~expected:0 ~actual:result.summary.would_reformat;
          Test.assert_equal ~expected:0 ~actual:result.summary.unsafe_to_format;
          Test.assert_equal ~expected:0 ~actual:result.summary.failed_files;
          let start_json = capture_json_event
            ~root:tmpdir
            (Krasny.Report.Start { mode = Krasny.Runner.Check; concurrency = 3 })
          |> Data.Json.of_string
          |> Result.expect ~msg:"parse start json" in
          let open Data.Json in
            Test.assert_equal ~expected:(Some (String "start")) ~actual:(get_field "type" start_json);
            assert_json_timestamp_field start_json;
            Test.assert_equal ~expected:(Some (Int 3)) ~actual:(get_field "concurrency" start_json);
            Test.assert_equal ~expected:(Some (String "check")) ~actual:(get_field "mode" start_json);
            Test.assert_equal ~expected:None ~actual:(get_field "total_files" start_json);
            Ok ()));
]

let () =
  Miniriot.run
    ~main:(fun ~args:_ -> Test.Cli.main ~name:"krasny:format" ~tests ~args:Env.args)
    ~args:Env.args
    ()
