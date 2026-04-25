val unsupported_syntax: Ast.origin -> string -> Diagnostics.Diagnostic.t

val unsupported_type: Ast.origin -> string -> Diagnostics.Diagnostic.t

val check_core_type: Ast.core_type -> Diagnostics.Diagnostic.t list

val check_type_definition: Ast.origin -> Diagnostics.Diagnostic.t list

val check_type_declaration: Ast.origin -> Diagnostics.Diagnostic.t list

val check_value_declaration: Ast.value_declaration -> Diagnostics.Diagnostic.t list

val check_external_declaration: Ast.external_declaration -> Diagnostics.Diagnostic.t list

val check_exception_rhs: Ast.origin -> Diagnostics.Diagnostic.t list
