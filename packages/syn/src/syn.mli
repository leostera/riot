open Std

(** Syn - OCaml Lexer and Parser

    Syn provides lexical analysis and parsing for OCaml source code, producing
    lossless Ceibo syntax trees that preserve all source information including
    whitespace and comments.

    # Architecture

    Syn is built in layers:

    - **Cursor**: Low-level character stream abstraction
    - **Lexer**: Tokenizes source into a flat token stream
    - **Parser**: Builds Ceibo green trees from tokens
    - **Syntax Trees**: Ceibo red-green trees with full fidelity

    # Key Properties

    - **Lossless**: Every byte of input is preserved in the tree
    - **Error Recovery**: Parser never fails, creates ERROR nodes for problems
    - **Structured Diagnostics**: Machine-readable error information
    - **Red-Green Trees**: Efficient immutable trees with structural sharing

    # Example Usage

    ## Lexing Only

    ```ocaml 
    let source = "let x = 42" in let tokens = Syn.tokenize source in
    List.iter 
      (fun tok -> Printf.printf "%s\n" (Token.show_kind tok.kind) )
      tokens 
    ```

    ## Full Parsing

    ```ocaml 
    let source = "let x = 1 + 2" in let result = Syn.parse source in

    (* Check for errors *) 
    if result.diagnostics != [] then 
      List.iter
        (fun diag -> print ("Error: " ^ (Diagnostic.to_string diag) ^ "\n") )
        result.diagnostics;

    (* Work with the tree *) 
    let root = Ceibo.Red.new_root result.tree in 
    (* ... traverse tree ... *)
    ```

    ## Working with Parse Errors

    The parser never fails - it produces a tree even from malformed code:

    ```ocaml 
    let source = "let x = " in 
    (* incomplete *) 
    let result = Syn.parse source in

    (* result.tree is a valid tree with ERROR/MISSING nodes *) 
    (* result.diagnostics contains structured error info *)

    match result.diagnostics with 
    | [] -> Printf.printf "No errors\n" 
    | errs -> 
        List.iter 
          (fun err -> 
            Printf.printf "%s at %s\n" 
              (Diagnostic.to_string err)
              (Ceibo.Span.to_string err.span)) 
          errs

    ```
*)
(** # Module Exports *)

(** Red-green syntax tree library. See `Ceibo` module documentation. *)
module Ceibo = Ceibo

(** Token types and utilities. *)
module Error: module type of Error

(** OCaml keyword definitions. *)
module Token: module type of Token

(** Low-level character stream cursor. *)
module Keyword: module type of Keyword

(** Lexical analyzer (tokenizer). *)
module Cursor: module type of Cursor

(** Syntax node kind enumeration for OCaml. *)
module Lexer: module type of Lexer

(** Structured parse error types. *)
module SyntaxKind: module type of Syntax_kind

module Diagnostic: module type of Diagnostic

(** Typed concrete syntax tree layered on top of the lossless Ceibo tree.

    `Cst` is only produced for parse results without diagnostics. *)
module DiagnosticReporter: module type of Diagnostic_reporter

(** Defaultable visitor-style traversal over the typed CST. *)
module Cst: module type of Cst

(** Faithful Ceibo-to-CST lift with a result-based API. *)
module Visit: module type of Visit

(** JSON serialization helpers for the typed CST and lift errors. *)
module CstBuilder: module type of Cst_builder

(** Syntactic module dependency extraction. *)
module Deps: module type of Deps

(** OCaml parser that produces Ceibo trees. *)
module CstJson: module type of Cst_json

(** Why a typed CST could not be constructed from a parse result. *)
module Parser: module type of Parser

type build_cst_error =
  | Parse_diagnostics of Diagnostic.t list
  | Cst_builder_error of CstBuilder.error
(** # High-Level API *)
(** `tokenize source` lexes source code into a flat list of real tokens.

    Leading whitespace/comments/docstrings are preserved on each token's
    `leading_trivia`, and trailing file trivia is preserved on `EOF`.

    Example: ```ocaml let tokens = Syn.tokenize "let x = 42" in (* tokens =
    [Keyword Let; Ident "x"; Eq; Literal (Int 42); EOF] with trivia attached
    to the later tokens *) ``` *)
val tokenize: string -> Token.t list

(** `parse_interface source` parses .mli source code into a Ceibo green tree
    with diagnostics. *)
val parse_interface: string -> Parser.parse_result

(** `parse_implementation source` parses .ml source code into a Ceibo green tree
    with diagnostics. *)
val parse_implementation: string -> Parser.parse_result

(** `parse ~filename source` parses source code into a Ceibo green tree with
    diagnostics.

    Automatically chooses parse_interface or parse_implementation based on
    filename extension.

    This function **never fails**. Even with syntax errors, it returns a tree
    structure with ERROR/MISSING nodes and a list of diagnostics.

    Returns:
    - `tokens`: Original lexer tokens with token-attached trivia preserved
    - `tree`: A Ceibo green tree (lossless, immutable)
    - `diagnostics`: List of parse errors found

    Example: ```ocaml let result = Syn.parse "let x = 1 + 2" in

    (* Use the green tree directly *) Printf.printf "Tree width: %d\n"
    result.tree.width;

    (* Or create a red view for positioned traversal *) let root =
    Ceibo.Red.new_root result.tree in let span = Ceibo.Red.SyntaxNode.span root
    in Printf.printf "Covers: %s\n" (Ceibo.Span.to_string span) ``` *)
val parse: filename:Std.Path.t -> string -> Parser.parse_result

(** `build_cst result` lifts a diagnostics-free Ceibo parse result into the
    typed CST.

    Parsing and CST construction are separate steps so callers that only need
    the lossless Ceibo tree do not pay the typed lift by default. The lift uses
    `result.tokens` as the file-level trivia source of truth instead of
    re-lexing `result.source`.

    Returns:
    - `Ok cst` when parsing produced no diagnostics and the faithful lift
      succeeded
    - `Error (Parse_diagnostics diags)` when parse recovery was needed
    - `Error (Cst_builder_error err)` when the current CST lift does not cover
      the parsed syntax
*)
val build_cst: Parser.parse_result -> (Cst.source_file, build_cst_error) result
