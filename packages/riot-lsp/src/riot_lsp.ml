open Std

module Framing = Framing
module Session = Session

let ( let* ) = Result.and_then

let write_outbound = fun output messages ->
  List.fold_left
    (fun acc json ->
      let* () = acc in
      Framing.write output (Std.Data.Json.to_string json) |> Result.map_error (fun message -> Failure message))
    (Ok ()) messages

let rec loop = fun input -> fun output -> fun state ->
  let* payload_opt = Framing.read input |> Result.map_error (fun message -> Failure message) in
  match payload_opt with
  | None -> Ok ()
  | Some payload ->
      let outcome = Session.handle_payload state payload in
      let* () = write_outbound output outcome.outbound in
      match outcome.exit_code with
      | Some 0 -> Ok ()
      | Some code -> Error (Failure ("riot-lsp exited with status " ^ Int.to_string code))
      | None -> loop input output outcome.state

let run = fun () ->
  let input = Fs.File.from_fd IO.stdin in
  let output = Fs.File.from_fd IO.stdout in
  loop input output Session.empty
