val check: Ast.expr -> Diagnostics.Diagnostic.t list

val check_parameter: Ast.parameter -> Diagnostics.Diagnostic.t list

val check_let_binding: Ast.let_binding -> Diagnostics.Diagnostic.t list
