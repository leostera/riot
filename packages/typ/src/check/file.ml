open Std
open Std.Collections

type t = {
  ast: Ast.t;
  diagnostics: Diagnostics.Diagnostic.t list;
  type_declarations: Ast.type_declaration list;
  bindings: Typing_context.value_binding list;
  typing_context: Typing_context.t;
}

let empty = fun ~ast ~typing_context ->
  {
    ast;
    diagnostics = [];
    type_declarations = [];
    bindings = [];
    typing_context;
  }

let is_ok = fun file -> List.is_empty file.diagnostics

let serializer = Serde.Ser.record
  (Serde.Ser.fields
    [
      Serde.Ser.field "ast" Ast.serializer (fun (file: t) -> file.ast);
      Serde.Ser.field
        "diagnostics"
        (Serde.Ser.contramap Array.from_list (Serde.Ser.array Diagnostics.Diagnostic.serializer))
        (fun (file: t) -> file.diagnostics);
      Serde.Ser.field
        "type_declarations"
        (Serde.Ser.contramap Array.from_list (Serde.Ser.array Serde.Ser.string))
        (fun (file: t) ->
          List.map file.type_declarations ~fn:(fun declaration -> declaration.Ast.name));
      Serde.Ser.field
        "bindings"
        (Serde.Ser.contramap
          Array.from_list
          (Serde.Ser.array Typing_context.value_binding_serializer))
        (fun (file: t) -> file.bindings);
      Serde.Ser.field
        "typing_context"
        Typing_context.serializer
        (fun (file: t) -> file.typing_context);
    ])
