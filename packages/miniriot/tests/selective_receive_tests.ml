open Miniriot
open Miniriot.Exception
module Result = Std.Result
module Test = Std.Test

type Message.t +=
  A
  | B
  | C

let sender = fun parent ->
  send parent A;
  send parent B;
  send parent C;
  Result.Ok ()

let expect_message = fun ~name ~expected actual ->
  if expected actual then
    Result.Ok ()
  else
    Result.Error (Kernel.String.concat "" [ "unexpected message for "; name ])

let test_selective_receive_preserves_skipped_messages = fun () ->
  let parent = self () in
  let _ =
    spawn (fun () -> sender parent)
  in
  let selected =
    receive
      ~selector:(
        function
        | B -> `select B
        | _ -> `skip
      )
      ~timeout:2.0
      ()
  in
  match
    expect_message ~name:"selected"
      ~expected:(
        function
        | B -> true
        | _ -> false
      )
      selected
  with
  | Result.Error _ as err -> err
  | Result.Ok () ->
      let first = receive_any ~timeout:2.0 () in
      (
        match
          expect_message ~name:"first"
            ~expected:(
              function
              | A -> true
              | _ -> false
            )
            first
        with
        | Result.Error _ as err -> err
        | Result.Ok () ->
            let second = receive_any ~timeout:2.0 () in
            expect_message ~name:"second"
              ~expected:(
                function
                | C -> true
                | _ -> false
              )
              second
      )

let test_case = fun _ctx ->
  try test_selective_receive_preserves_skipped_messages () with
  | Receive_timeout -> Result.Error "timed out waiting for expected message"
  | exn -> Result.Error (Kernel.Exception.to_string exn)

let () =
  let tests = [ Test.case "selective receive drains saved messages first" test_case ] in
  let normalize_args = function
    | [] -> [ "selective_receive_tests"; "run-tests" ]
    | [ exe ] -> [ exe; "run-tests" ]
    | args -> args
  in
  let main ~args =
    match Test.Cli.main ~name:"selective_receive_tests" ~tests ~args:(normalize_args args) with
    | Result.Ok () -> Result.Ok ()
    | Result.Error msg -> Result.Error msg
  in
  Miniriot.run ~main ~args:Std.Env.args ~config:(Miniriot.Config.make ~scheduler_count:4 ()) ()
