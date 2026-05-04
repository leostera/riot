open Std
open Types

val run: request -> (result, Error.t) Result.t

val replay:
  target:target ->
  input_path:Path.t ->
  timeout_ms:int ->
  (replay_result, Error.t) Result.t

val minimize_corpus: minimize_request -> (minimize_result, Error.t) Result.t

val run_many: ?concurrency:int -> request list -> many_result
