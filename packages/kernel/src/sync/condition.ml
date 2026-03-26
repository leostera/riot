type t = Stdlib.Condition.t

let create = Stdlib.Condition.create
let wait = Stdlib.Condition.wait
let signal = Stdlib.Condition.signal
let broadcast = Stdlib.Condition.broadcast
