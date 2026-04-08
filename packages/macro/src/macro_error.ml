open Std

type t = {
  message: string;
  span: Syn.Ceibo.Span.t option;
}

let make = fun ?span message -> { message; span }

let of_parse_diagnostic = fun diagnostic ->
  make
    ~span:diagnostic.Syn.Diagnostic.span
    ("macro expansion produced invalid OCaml: " ^ Syn.Diagnostic.main_message diagnostic)

let message = fun error -> error.message
