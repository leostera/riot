open Kernel

module Exception = struct
  exception Receive_timeout

  exception Syscall_timeout
end

let yield = fun () -> Effect.perform Proc_effect.Yield

type 'msg selector =
  Message.t -> [
    `select of 'msg
    | `skip
  ]

let receive_any = fun ?timeout () ->
  let timeout =
    match timeout with
    | None -> `infinity
    | Some after -> `after after
  in
  Effect.perform (Proc_effect.Receive { selector = (fun msg -> `select msg); timeout })

let receive = fun ~selector ?timeout () ->
  let timeout =
    match timeout with
    | None -> `infinity
    | Some after -> `after after
  in
  Effect.perform (Proc_effect.Receive { selector; timeout })

let exit = fun () -> Ok ()

let syscall = fun ?timeout ~name ~interest ~source cb ->
  let timeout =
    match timeout with
    | None -> `infinity
    | Some after -> `after after
  in
  Effect.perform (Proc_effect.Syscall { name; interest; source; timeout });
  cb ()
