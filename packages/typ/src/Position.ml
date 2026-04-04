open Std

type t = {
  offset: int;
}

let make = fun ~offset -> { offset }

let is_within_span = fun position (span: Syn.Ceibo.Span.t) ->
  position.offset >= span.start && position.offset <= span.end_
