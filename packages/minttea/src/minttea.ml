(* Don't open Std here to avoid conflict with Std.Command *)

type Std.Message.t +=
  | ProgramDone

module App = App
module Command = Command
module Component = Component
module Config = Config
module Element = Gooey.Element
module Event = Event
module Program = Program
module Style = Gooey.Style

let config = Config.make

let app = App.make

let run = fun ?(config = config ()) initial_model app ->
  let open Std in Program.run ~app ~config ~initial_model
  |> Result.map_err (fun reason -> Failure reason)

let start = fun ?(config = config ()) (app: 'model App.t) initial_model ->
  let main ~args:_ =
    let open Std in
      let main_pid = self () in
      let _prog_pid =
        spawn
          (fun () ->
            run ~config initial_model app |> Std.Result.expect ~msg:"Program finished";
            Log.trace "program finished";
            send main_pid ProgramDone;
            Ok ())
      in
      (* Wait for the program to finish *)
      let rec wait () =
        match receive_any () with
        | ProgramDone ->
            Log.trace "main finished";
            Ok ()
        | _ -> wait ()
      in
      wait ()
  in
  let _ = Std.Runtime.run ~main ~args:[] () in
  ()
