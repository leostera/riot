module Io_vector = struct
type t = {
  iov_base : Bigstringaf.t;
  iov_len : int;
}
let of_bigstring iov_base = { iov_base; iov_len = Bigstringaf.length iov_base }
end

type iovec = Io_vector.t

external _unsafe_readv : Unix.file_descr -> iovec array  -> int -> int = "riot_unix_readv"

let readv fd (bigstring: Bigstringaf.t) = 
  let iovecs = [| Io_vector.of_bigstring bigstring |] in
  _unsafe_readv fd iovecs 1

external _unsafe_writev : Unix.file_descr -> iovec array -> int -> int = "riot_unix_writev"

let writev fd bigstring =
  let iovecs = [| Io_vector.of_bigstring bigstring |] in
  _unsafe_writev fd iovecs 1
