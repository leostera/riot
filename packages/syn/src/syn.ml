open Std
module Ceibo = Ceibo
module Error = Error
module Token = Token
module Keyword = Keyword
module Cursor = Cursor
module Lexer = Lexer
module SyntaxKind = Syntax_kind
module Diagnostic = Diagnostic
module Parser = Parser
module DiagnosticReporter = Diagnostic_reporter
module Cst = Cst
module Visit = Visit
module CstBuilder = Cst_builder
module CstJson = Cst_json
module Deps = Deps

type build_cst_error =
  | Parse_diagnostics of Diagnostic.t list
  | Cst_builder_error of CstBuilder.error

let tokenize = fun source -> Lexer.tokenize source

let build_cst = fun (result: Parser.parse_result) ->
  if List.length result.Parser.diagnostics > 0 then
    Error (Parse_diagnostics result.Parser.diagnostics)
  else
    match CstBuilder.create_from_ceibo
      ~kind:result.Parser.kind
      ~source:result.Parser.source
      ~tokens:result.Parser.tokens
      result.tree with
    | Ok cst -> Ok cst
    | Error err -> Error (Cst_builder_error err)

let parse_interface = fun source ->
  let tokens = Lexer.tokenize source in
  Parser.parse_interface ~source tokens

let parse_implementation = fun source ->
  let tokens = Lexer.tokenize source in
  Parser.parse_implementation ~source tokens

let parse = fun ~filename source ->
  match Path.extension filename with
  | Some ".mli" -> parse_interface source
  | _ -> parse_implementation source
