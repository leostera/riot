open Std

type token_kind =
  | Line_text
  | Newline
  | EOF

type t = {
  kind: token_kind;
  span: Ceibo.Span.t;
  text: string;
}

let show_kind = function
  | Line_text -> "line_text"
  | Newline -> "newline"
  | EOF -> "eof"
