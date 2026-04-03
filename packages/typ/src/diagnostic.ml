open Std

type severity =
  | Error
  | Warning

type supported_literal =
  | IntLiteral
  | BoolLiteral
  | StringLiteral
  | UnitLiteral

type unsupported_reason =
  | LiteralOutsideSupportedSubset of {
      supported_literals: supported_literal list;
    }

type unsupported_context =
  | StructureItem
  | Pattern
  | Expression

type unsupported_recovery =
  | PlaceholderItem
  | RecoveryPattern
  | HoleExpression

type mismatch =
  | ExpectedActual of {
      expected: string;
      actual: string;
    }
  | TupleArityMismatch of {
      left: string;
      right: string;
      left_arity: int;
      right_arity: int;
    }
  | OccursCheckFailed of {
      variable_id: int;
      in_type: string;
    }

type t =
  | CstBuilderError of {
      builder_error: Syn.CstBuilder.error;
    }
  | UnsupportedSyntax of {
      syntax_span: Syn.Ceibo.Span.t;
      syntax_kind: Syn.SyntaxKind.t;
      context: unsupported_context;
      recovery: unsupported_recovery;
      reason: unsupported_reason option;
    }
  | IgnoredPatternTypeConstraint of {
      constraint_span: Syn.Ceibo.Span.t;
    }
  | ParameterLoweredAsPositional of {
      parameter_span: Syn.Ceibo.Span.t;
    }
  | IgnoredMatchGuard of {
      guard_span: Syn.Ceibo.Span.t;
    }
  | UnsupportedApplicationArgumentLabels of {
      application_span: Syn.Ceibo.Span.t;
    }
  | IgnoredTypeAscription of {
      ascription_span: Syn.Ceibo.Span.t;
    }
  | IgnoredPolymorphicAnnotation of {
      annotation_span: Syn.Ceibo.Span.t;
    }
  | UnsupportedInterfaceFile of {
      interface_span: Syn.Ceibo.Span.t;
    }
  | UnboundName of {
      reference_span: Syn.Ceibo.Span.t;
      name: string;
    }
  | TypeMismatch of {
      mismatch_span: Syn.Ceibo.Span.t;
      mismatch: mismatch;
    }
  | UnsupportedSemanticExpression of {
      expression_span: Syn.Ceibo.Span.t;
      summary: string;
    }
  | RecursiveGroupRequiresSimpleVariableBinders of {
      binding_span: Syn.Ceibo.Span.t;
    }

let code = function
  | CstBuilderError _ -> "TYP1011"
  | UnsupportedSyntax _ -> "TYP1001"
  | IgnoredPatternTypeConstraint _ -> "TYP1004"
  | ParameterLoweredAsPositional _ -> "TYP1005"
  | IgnoredMatchGuard _ -> "TYP1006"
  | UnsupportedApplicationArgumentLabels _ -> "TYP1007"
  | IgnoredTypeAscription _ -> "TYP1008"
  | IgnoredPolymorphicAnnotation _ -> "TYP1009"
  | UnsupportedInterfaceFile _ -> "TYP1010"
  | UnboundName _ -> "TYP2001"
  | TypeMismatch _ -> "TYP2002"
  | UnsupportedSemanticExpression _ -> "TYP2010"
  | RecursiveGroupRequiresSimpleVariableBinders _ -> "TYP2004"

let name = function
  | CstBuilderError _ -> "cst-builder-error"
  | UnsupportedSyntax _ -> "unsupported-syntax"
  | IgnoredPatternTypeConstraint _ -> "ignored-pattern-type-constraint"
  | ParameterLoweredAsPositional _ -> "parameter-lowered-as-positional"
  | IgnoredMatchGuard _ -> "ignored-match-guard"
  | UnsupportedApplicationArgumentLabels _ -> "unsupported-application-argument-labels"
  | IgnoredTypeAscription _ -> "ignored-type-ascription"
  | IgnoredPolymorphicAnnotation _ -> "ignored-polymorphic-annotation"
  | UnsupportedInterfaceFile _ -> "unsupported-interface-file"
  | UnboundName _ -> "unbound-name"
  | TypeMismatch _ -> "type-mismatch"
  | UnsupportedSemanticExpression _ -> "unsupported-semantic-expression"
  | RecursiveGroupRequiresSimpleVariableBinders _ -> "recursive-group-requires-simple-variable-binders"

let severity = function
  | CstBuilderError _
  | UnsupportedSyntax _
  | IgnoredPatternTypeConstraint _
  | ParameterLoweredAsPositional _
  | IgnoredMatchGuard _
  | UnsupportedApplicationArgumentLabels _
  | IgnoredTypeAscription _
  | IgnoredPolymorphicAnnotation _
  | UnsupportedInterfaceFile _
  | UnboundName _
  | TypeMismatch _
  | UnsupportedSemanticExpression _
  | RecursiveGroupRequiresSimpleVariableBinders _ -> Error

let severity_to_string = function
  | Error -> "error"
  | Warning -> "warning"

let primary_span = function
  | CstBuilderError { builder_error } -> builder_error.span
  | UnsupportedSyntax { syntax_span; _ } -> syntax_span
  | IgnoredPatternTypeConstraint { constraint_span } -> constraint_span
  | ParameterLoweredAsPositional { parameter_span } -> parameter_span
  | IgnoredMatchGuard { guard_span } -> guard_span
  | UnsupportedApplicationArgumentLabels { application_span } -> application_span
  | IgnoredTypeAscription { ascription_span } -> ascription_span
  | IgnoredPolymorphicAnnotation { annotation_span } -> annotation_span
  | UnsupportedInterfaceFile { interface_span } -> interface_span
  | UnboundName { reference_span; _ } -> reference_span
  | TypeMismatch { mismatch_span; _ } -> mismatch_span
  | UnsupportedSemanticExpression { expression_span; _ } -> expression_span
  | RecursiveGroupRequiresSimpleVariableBinders { binding_span } -> binding_span

let supported_literal_to_string = function
  | IntLiteral -> "int"
  | BoolLiteral -> "bool"
  | StringLiteral -> "string"
  | UnitLiteral -> "unit"

let unsupported_context_tag = function
  | StructureItem -> "structure_item"
  | Pattern -> "pattern"
  | Expression -> "expression"

let unsupported_context_to_string = function
  | StructureItem -> "structure item"
  | Pattern -> "pattern"
  | Expression -> "expression"

let unsupported_recovery_tag = function
  | PlaceholderItem -> "placeholder_item"
  | RecoveryPattern -> "recovery_pattern"
  | HoleExpression -> "hole_expression"

let unsupported_recovery_to_string = function
  | PlaceholderItem -> "placeholder item"
  | RecoveryPattern -> "recovery pattern"
  | HoleExpression -> "type hole"

let unsupported_reason_to_string = function
  | LiteralOutsideSupportedSubset { supported_literals } ->
      "literal kind is outside the currently supported subset (supported: "
      ^ String.concat ", " (List.map supported_literal_to_string supported_literals)
      ^ ")"

let message = function
  | CstBuilderError { builder_error } ->
      "Syn.build_cst failed before lowering: " ^ builder_error.message
  | UnsupportedSyntax { syntax_kind; context; recovery; reason = None; _ } ->
      "unsupported "
      ^ unsupported_context_to_string context
      ^ " lowered using "
      ^ unsupported_recovery_to_string recovery
      ^ ": "
      ^ Syn.SyntaxKind.to_string syntax_kind
  | UnsupportedSyntax { syntax_kind; context; recovery; reason = Some reason; _ } ->
      "unsupported "
      ^ unsupported_context_to_string context
      ^ " lowered using "
      ^ unsupported_recovery_to_string recovery
      ^ ": "
      ^ Syn.SyntaxKind.to_string syntax_kind
      ^ " ("
      ^ unsupported_reason_to_string reason
      ^ ")"
  | IgnoredPatternTypeConstraint _ ->
      "type-constrained pattern lowered without its annotation"
  | ParameterLoweredAsPositional _ ->
      "labeled, optional, or locally abstract parameters are currently lowered as ordinary positional binders"
  | IgnoredMatchGuard _ ->
      "match guards are currently ignored during lowering"
  | UnsupportedApplicationArgumentLabels _ ->
      "labeled or optional application arguments are not lowered yet"
  | IgnoredTypeAscription _ ->
      "type ascriptions are currently ignored during lowering"
  | IgnoredPolymorphicAnnotation _ ->
      "explicit polymorphic annotations are currently ignored during lowering"
  | UnsupportedInterfaceFile _ ->
      "interface files are not lowered by the prototype yet"
  | UnboundName { name; _ } ->
      "unbound name: " ^ name
  | TypeMismatch { mismatch = ExpectedActual { expected; actual }; _ } ->
      "type mismatch: expected " ^ expected ^ " but got " ^ actual
  | TypeMismatch { mismatch = TupleArityMismatch { left; right; left_arity; right_arity }; _ } ->
      "type mismatch: tuple arity mismatch ("
      ^ Int.to_string left_arity
      ^ " vs "
      ^ Int.to_string right_arity
      ^ ") between "
      ^ left
      ^ " and "
      ^ right
  | TypeMismatch { mismatch = OccursCheckFailed { variable_id; in_type }; _ } ->
      "type mismatch: occurs check failed for type variable "
      ^ Int.to_string variable_id
      ^ " in "
      ^ in_type
  | UnsupportedSemanticExpression { summary; _ } ->
      "unsupported semantic expression reached inference: " ^ summary
  | RecursiveGroupRequiresSimpleVariableBinders _ ->
      "recursive groups currently require simple variable binders"

let span_to_json = fun (span: Syn.Ceibo.Span.t) ->
  Data.Json.Object [
    ("start", Data.Json.Int span.start);
    ("end", Data.Json.Int span.end_);
  ]

let mismatch_to_json = function
  | ExpectedActual { expected; actual } ->
      Data.Json.Object [
        ("tag", Data.Json.String "expected_actual");
        ("expected", Data.Json.String expected);
        ("actual", Data.Json.String actual);
      ]
  | TupleArityMismatch { left; right; left_arity; right_arity } ->
      Data.Json.Object [
        ("tag", Data.Json.String "tuple_arity_mismatch");
        ("left", Data.Json.String left);
        ("right", Data.Json.String right);
        ("left_arity", Data.Json.Int left_arity);
        ("right_arity", Data.Json.Int right_arity);
      ]
  | OccursCheckFailed { variable_id; in_type } ->
      Data.Json.Object [
        ("tag", Data.Json.String "occurs_check_failed");
        ("variable_id", Data.Json.Int variable_id);
        ("in_type", Data.Json.String in_type);
      ]

let supported_literal_to_json = fun literal ->
  Data.Json.String (supported_literal_to_string literal)

let unsupported_reason_to_json = function
  | LiteralOutsideSupportedSubset { supported_literals } ->
      Data.Json.Object [
        ("tag", Data.Json.String "literal_outside_supported_subset");
        ("supported_literals", Data.Json.Array (List.map supported_literal_to_json supported_literals));
      ]

let cst_builder_error_to_json = fun (error: Syn.CstBuilder.error) ->
  Data.Json.Object [
    ("message", Data.Json.String error.message);
    ("syntax_kind", Data.Json.String (Syn.SyntaxKind.to_string error.syntax_kind));
    ("span", span_to_json error.span);
    ("context", Data.Json.Array (List.map (fun value -> Data.Json.String value) error.context));
  ]

let fields_to_json = function
  | CstBuilderError { builder_error } ->
      [
        ("builder_error", cst_builder_error_to_json builder_error);
      ]
  | UnsupportedSyntax { syntax_span; syntax_kind; context; recovery; reason } ->
      let reason_json =
        match reason with
        | Some reason -> unsupported_reason_to_json reason
        | None -> Data.Json.Null
      in
      [
        ("syntax_span", span_to_json syntax_span);
        ("syntax_kind", Data.Json.String (Syn.SyntaxKind.to_string syntax_kind));
        ("context", Data.Json.String (unsupported_context_tag context));
        ("recovery", Data.Json.String (unsupported_recovery_tag recovery));
        ("reason", reason_json);
      ]
  | IgnoredPatternTypeConstraint { constraint_span } ->
      [
        ("constraint_span", span_to_json constraint_span);
      ]
  | ParameterLoweredAsPositional { parameter_span } ->
      [
        ("parameter_span", span_to_json parameter_span);
      ]
  | IgnoredMatchGuard { guard_span } ->
      [
        ("guard_span", span_to_json guard_span);
      ]
  | UnsupportedApplicationArgumentLabels { application_span } ->
      [
        ("application_span", span_to_json application_span);
      ]
  | IgnoredTypeAscription { ascription_span } ->
      [
        ("ascription_span", span_to_json ascription_span);
      ]
  | IgnoredPolymorphicAnnotation { annotation_span } ->
      [
        ("annotation_span", span_to_json annotation_span);
      ]
  | UnsupportedInterfaceFile { interface_span } ->
      [
        ("interface_span", span_to_json interface_span);
      ]
  | UnboundName { reference_span; name } ->
      [
        ("reference_span", span_to_json reference_span);
        ("name_text", Data.Json.String name);
      ]
  | TypeMismatch { mismatch_span; mismatch } ->
      [
        ("mismatch_span", span_to_json mismatch_span);
        ("mismatch", mismatch_to_json mismatch);
      ]
  | UnsupportedSemanticExpression { expression_span; summary } ->
      [
        ("expression_span", span_to_json expression_span);
        ("summary", Data.Json.String summary);
      ]
  | RecursiveGroupRequiresSimpleVariableBinders { binding_span } ->
      [
        ("binding_span", span_to_json binding_span);
      ]

let to_json = fun diagnostic ->
  Data.Json.Object
    ([
       ("id", Data.Json.String (code diagnostic));
       ("name", Data.Json.String (name diagnostic));
       ("message", Data.Json.String (message diagnostic));
       ("severity", Data.Json.String (severity_to_string (severity diagnostic)));
     ]
     @ fields_to_json diagnostic)

let to_string = fun diagnostic ->
  severity_to_string (severity diagnostic)
  ^ " "
  ^ code diagnostic
  ^ " @ "
  ^ Syn.Ceibo.Span.to_string (primary_span diagnostic)
  ^ ": "
  ^ message diagnostic
