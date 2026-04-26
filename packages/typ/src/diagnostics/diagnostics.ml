open Std
open Std.Collections
module Diagnostic = Diagnostic

type t = {
  items: Diagnostic.t vec;
}

let create () = { items = Vector.create () }
