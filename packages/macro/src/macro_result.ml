open Std

type t = {
  output: Macro_token_stream.t;
  diagnostics: Macro_error.t list;
}

let ok = fun output -> { output; diagnostics = [] }

let ok_source = fun ?span source ->
  ok (Macro_token_stream.make ?span source)

let error = fun diagnostic ->
  {
    output = Macro_token_stream.make "";
    diagnostics = [ diagnostic ];
  }
