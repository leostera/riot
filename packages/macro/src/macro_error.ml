open Std

type t = {
  message: string;
  span: Syn.Ceibo.Span.t option;
}

let make = fun ?span message -> { message; span }

let message = fun error -> error.message
