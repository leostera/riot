open Std

type error = {
  message: string;
  span: Syn.Ceibo.Span.t option;
}

type expansion = {
  source: string;
  changed: bool;
}

val error_message: error -> string

val expand_source: filename: Path.t -> string -> (expansion, error) result
