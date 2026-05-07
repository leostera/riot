open Std

module Span = Span
module Error = Error
module Token = Token
module Keyword = Keyword
module Cursor = Cursor
module Lexer = Lexer
module SyntaxKind = Syntax_kind
module RawToken = Raw_token
module Event = Event
module SyntaxTree = Syntax_tree
module Ast = Ast
module Visitor = Visitor
module Diagnostic = Diagnostic
module Parser = Parser
module DiagnosticReporter = Diagnostic_reporter
module Deps = Deps

let tokenize = fun source -> Lexer.tokenize source

let parse_interface = Parser.parse_interface

let parse_implementation = Parser.parse_implementation

let phrase_complete = Parser.phrase_complete

let parse_structure_phrase = Parser.parse_structure_phrase

let parse_signature_phrase = Parser.parse_signature_phrase

let parse = Parser.parse

let source_slice = fun source ->
  IO.IoVec.IoSlice.from_string source
  |> Result.expect ~msg:"failed to create source slice"

let parse_ident = fun source ->
  let result = Parser.parse_implementation (source_slice source) in
  if not (Std.Collections.Vector.is_empty result.Parser.diagnostics) then
    None
  else
    let source_file = Ast.SourceFile.make result.Parser.tree in
    match Ast.SourceFile.view source_file with
    | Ast.SourceFile.Interface _ -> None
    | Ast.SourceFile.Implementation implementation ->
        if not (Int.equal (Ast.Implementation.item_count implementation) 1) then
          None
        else
          Ast.Implementation.fold_item
            implementation
            ~init:None
            ~fn:(fun item _ ->
              match Ast.StructureItem.view item with
              | Ast.StructureItem.Expr expr_item -> (
                  match Ast.ExprItem.expr expr_item with
                  | None -> Ast.Return None
                  | Some expr -> (
                      match Ast.Expr.view expr with
                      | Ast.Expr.Ident { ident } -> Ast.Return (Some ident)
                      | Ast.Expr.Constructor { constructor; payload = None } ->
                          Ast.Return (Some constructor)
                      | _ -> Ast.Return None
                    )
                )
              | _ -> Ast.Return None)
