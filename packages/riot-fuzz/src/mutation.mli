open Std

val choose_corpus_input: Random.Rng.t -> string list -> (string, Error.t) Result.t

val mutate:
  Random.Rng.t ->
  max_len:int ->
  corpus:string list ->
  dictionary:string list ->
  splicing:bool ->
  string ->
  (string, Error.t) Result.t
