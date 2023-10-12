module PidSet = Hashtbl.Make (struct
  type t = Process.t

  let hash (t : t) = Pid.hash t.pid
  let equal (a : t) (b : t) = Pid.equal a.pid b.pid
end)

type t = { mutable _set : unit PidSet.t }

let create () = { _set = PidSet.create 1024 }
let remove t proc = PidSet.remove t._set proc
let contains t proc = PidSet.mem t._set proc
let size t = PidSet.length t._set
let is_empty t = size t = 0

(* NOTE(leostera): `PidSet.add` actually keeps duplicates! we want to use `replace` to drop the old one *)
let add t proc = PidSet.replace t._set proc ()

let flush t =
  let pids = PidSet.to_seq_keys t._set |> List.of_seq in
  PidSet.clear t._set;
  pids
