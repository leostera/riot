open Std

type result = {
  status: Afl.status;
  stdout: string;
  stderr: string;
}

val run: target:Types.target -> input_path:Path.t -> timeout_ms:int -> (result, Error.t) Result.t
