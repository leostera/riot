open Std

type request = Context.request
type event = Riot_build.Event.t

let request_of_workspace = Context.of_workspace

let detached_request = Context.detached

let run_string = Runner.run_string

let run_file = Runner.run_file

module Session = struct
  include Runner.Session
end
