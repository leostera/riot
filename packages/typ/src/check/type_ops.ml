let unsupported_syntax = Core.unsupported_syntax

let unsupported_type = Core.unsupported_type

let check_core_type = Core.check_core_type

let check_type_definition = fun (_origin: Ast.origin) -> []

let check_type_declaration = fun (_origin: Ast.origin) -> []

let check_value_declaration = fun (_declaration: Ast.value_declaration) -> []

let check_external_declaration = fun (_declaration: Ast.external_declaration) -> []

let check_exception_rhs = fun (_origin: Ast.origin) -> []
