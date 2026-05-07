#use blink;;

open Std
open Std.Collections
open Std.Result.Syntax
open Std.Net


let send_request url =
  let* conn = Blink.connect url in
  let req = Http.Request.create Get url in
  let* res = Blink.request conn req () in
  let* (resp, _body) = Blink.await conn in
  let status = Http.Response.status resp |> Http.Status.to_string in
  println status;
  Ok ()

let main ~args =
  let url = List.head args  |> Option.unwrap |> Uri.from_string |> Result.unwrap in
  send_request url |> Result.unwrap;
  Ok ()
