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

val show_kind: token_kind -> string
