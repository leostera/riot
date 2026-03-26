type t = Stdlib.Mutex.t

let create = Stdlib.Mutex.create
let lock = Stdlib.Mutex.lock
let unlock = Stdlib.Mutex.unlock
let try_lock = Stdlib.Mutex.try_lock
