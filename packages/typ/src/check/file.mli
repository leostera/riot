type t = {
  ast: Ast.t;
  diagnostics: Diagnostics.Diagnostic.t list;
  type_declarations: Ast.type_declaration list;
  bindings: Typing_context.value_binding list;
  typing_context: Typing_context.t;
}
val empty: ast:Ast.t -> typing_context:Typing_context.t -> t

val is_ok: t -> bool

val serializer: t Serde.Ser.t
