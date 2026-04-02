open Std

(* === TYPES === *)

type property_result =
  | Success
  | Failure of { counter_example: string; shrink_steps: int }
  | Error of { exception_: exn; backtrace: string }
  | Assumption_violated

type config = {
  test_count: int;
  max_shrink_steps: int;
  seed: int option;
  verbose: bool;
}

let default_config = { test_count = 100; max_shrink_steps = 1_000; seed = None; verbose = false }

(* Internal property representation *)

type 'value property_internal = {
  name: string;
  arbitrary: 'value Arbitrary.t;
  predicate: 'value -> bool;
}

type test_property =
  Prop: 'value property_internal -> test_property

(* === EXCEPTIONS FOR CONTROL FLOW === *)

exception Assumption_failed

exception Property_failed of string

(* === ASSUMPTIONS === *)

let implies = fun precondition conclusion ->
  if not precondition then
    raise Assumption_failed;
  conclusion

let assume = fun cond ->
  if not cond then
    raise Assumption_failed

let assume_fail = fun () -> raise Assumption_failed

(* === FAILURE REPORTING === *)

let fail = fun msg -> raise (Property_failed msg)

(* === RUNNING PROPERTIES === *)

let get_name = fun (Prop prop) -> prop.name

(* Shrink a counter-example to find minimal failing value *)

let shrink_counter_example = fun arb value predicate max_steps ->
  match arb.Arbitrary.shrink with
  | None -> (value, 0)
  | Some shrinker ->
      let rec shrink_loop current steps_taken =
        if steps_taken >= max_steps then
          (current, steps_taken)
        else
          let candidates = Shrinker.shrink shrinker current in
          let rec try_candidates = function
            | [] -> (current, steps_taken)
            | candidate :: rest ->
                (* Test if this smaller value also fails *)
                let fails =
                  try
                    let result = predicate candidate in
                    not result
                  with
                  | Assumption_failed -> false
                  | Property_failed _ -> true
                  | _ -> true
                in
                if fails then
                  shrink_loop candidate (steps_taken + 1)
                else
                  (* This candidate passes, try next one *)
                  try_candidates rest
          in
          try_candidates candidates
      in
      shrink_loop value 0

let check = fun ?(config = default_config) (Prop prop) ->
  let rnd =
    match config.seed with
    | Some seed -> Random.State.make [|seed|]
    | None -> Random.State.make_self_init ()
  in
  let arbitrary = prop.arbitrary in
  let predicate = prop.predicate in
  (* Track assumption failures *)
  let assumptions_failed = ref 0 in
  let tests_run = ref 0 in
  let rec test_loop n =
    if n >= config.test_count then
      Success
    else if !assumptions_failed > config.test_count * 10 then
      Assumption_violated
    else
      (* Generate a test value *)
      let value = Generator.generate rnd arbitrary.gen in
      (* Test the property *)
      let test_result =
        try
          let result = predicate value in
          if result then
            Some `Passed
          else
            Some (`Failed value)
        with
        | Assumption_failed ->
            assumptions_failed := !assumptions_failed + 1;
            None
        | Property_failed msg ->
            Some (`Failed_with_msg (value, msg))
        | exn ->
            let backtrace = Exception.get_backtrace () in
            Some (`Exception (value, exn, backtrace))
      in
      match test_result with
      | None ->
          (* Assumption failed, try next test *)
          test_loop n
      | Some `Passed ->
          tests_run := !tests_run + 1;
          if config.verbose then
            println
              ("Test " ^ Int.to_string (!tests_run) ^ "/" ^ Int.to_string config.test_count ^ " passed");
          test_loop (n + 1)
      | Some (`Failed value) ->
          (* Property failed! Shrink to find minimal counter-example *)
          let (minimal_value, shrink_steps) = shrink_counter_example
            arbitrary
            value
            predicate
            config.max_shrink_steps in
          let counter_example =
            match arbitrary.print with
            | Some printer -> printer minimal_value
            | None -> "<no printer available>"
          in
          Failure { counter_example; shrink_steps }
      | Some (`Failed_with_msg (value, msg)) ->
          let (minimal_value, shrink_steps) = shrink_counter_example
            arbitrary
            value
            predicate
            config.max_shrink_steps in
          let value_str =
            match arbitrary.print with
            | Some printer -> printer minimal_value
            | None -> "<no printer available>"
          in
          let counter_example = value_str ^ "\nMessage: " ^ msg in
          Failure { counter_example; shrink_steps }
      | Some (`Exception (value, exn, backtrace)) ->
          let (minimal_value, shrink_steps) =
            shrink_counter_example arbitrary value
              (fun v ->
                try
                  let _ = predicate v in
                  true
                with
                | _ -> false)
              config.max_shrink_steps
          in
          let counter_example_str =
            match arbitrary.print with
            | Some printer -> printer minimal_value
            | None -> "<no printer available>"
          in
          Error { exception_ = exn; backtrace = counter_example_str ^ "\nBacktrace:\n" ^ backtrace }
  in
  test_loop 0

(* === CREATING PROPERTIES === *)

let for_all = fun arbitrary predicate -> Prop { name = "unnamed property"; arbitrary; predicate }

let get_test_count_from_env = fun () ->
  Env.var Int ~name:"PROPANE_TESTS" |> Option.unwrap_or ~default:default_config.test_count

let get_seed_from_env = fun () -> Env.var Int ~name:"PROPANE_SEED"

let property = fun name arbitrary predicate ->
  let test_count = get_test_count_from_env () in
  let seed = get_seed_from_env () in
  let config = { default_config with test_count; seed } in
  let prop = Prop { name; arbitrary; predicate } in
  Test.property name ~examples:test_count
    (fun _ctx ->
      let result = check ~config prop in
      match result with
      | Success ->
          Ok ()
      | Failure { counter_example; shrink_steps } ->
          let msg = String.concat
            "\n"
            [
              "Property failed";
              "Counter-example (after " ^ Int.to_string shrink_steps ^ " shrink steps):";
              counter_example;
            ] in
          Error msg
      | Error { exception_; backtrace } ->
          let msg = String.concat
            "\n"
            [ "Exception raised:"; Exception.to_string exception_; backtrace; ] in
          Error msg
      | Assumption_violated ->
          Error "Too many test cases violated assumptions (>10x test count)")
