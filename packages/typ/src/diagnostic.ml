open Std

type severity =
  | Error
  | Warning

type supported_literal =
  | IntLiteral
  | FloatLiteral
  | BoolLiteral
  | StringLiteral
  | CharLiteral
  | UnitLiteral

type unsupported_reason =
  | LiteralOutsideSupportedSubset of { supported_literals: supported_literal list }

type unsupported_context =
  | StructureItem
  | SignatureItem
  | Pattern
  | Expression

type unsupported_recovery =
  | PlaceholderItem
  | RecoveryPattern
  | HoleExpression

type mismatch =
  | ExpectedActual of { expected: string; actual: string }
  | TupleArityMismatch of { left: string; right: string; left_arity: int; right_arity: int }
  | OccursCheckFailed of { variable_id: int; in_type: string }

type application_label =
  | PositionalArgument
  | LabeledArgument of string
  | OptionalArgument of string

type record_context =
  | RecordConstruction
  | RecordUpdate
  | RecordPattern
  | RecordFieldAccess

type record_resolution_reason =
  | UnknownRecordLabels of string list
  | AmbiguousRecordLabels of string list
  | MissingRecordFields of string list
  | IncompatibleRecordLabels of string list

type t =
  | CstBuilderError of { builder_error: Syn.CstBuilder.error }
  | UnsupportedSyntax of {
      syntax_span: Syn.Ceibo.Span.t;
      syntax_kind: Syn.SyntaxKind.t;
      context: unsupported_context;
      recovery: unsupported_recovery;
      reason: unsupported_reason option
    }
  | IgnoredPatternTypeConstraint of { constraint_span: Syn.Ceibo.Span.t }
  | ParameterLoweredAsPositional of { parameter_span: Syn.Ceibo.Span.t }
  | ApplicationArgumentLoweredAsPositional of { application_span: Syn.Ceibo.Span.t }
  | IgnoredTypeAscription of { ascription_span: Syn.Ceibo.Span.t }
  | IgnoredPolymorphicAnnotation of { annotation_span: Syn.Ceibo.Span.t }
  | UnsupportedInterfaceFile of { interface_span: Syn.Ceibo.Span.t }
  | UnboundName of { reference_span: Syn.Ceibo.Span.t; name: string }
  | TypeMismatch of { mismatch_span: Syn.Ceibo.Span.t; mismatch: mismatch }
  | ApplicationLabelMismatch of {
      application_span: Syn.Ceibo.Span.t;
      expected_label: application_label;
      actual_labels: application_label list
    }
  | RecordResolutionError of {
      operation_span: Syn.Ceibo.Span.t;
      context: record_context;
      reason: record_resolution_reason
    }
  | OrPatternBindingsMismatch of {
      pattern_span: Syn.Ceibo.Span.t;
      expected_names: string list;
      actual_names: string list
    }
  | UnsupportedSemanticExpression of { expression_span: Syn.Ceibo.Span.t; summary: string }
  | RecursiveGroupRequiresSimpleVariableBinders of { binding_span: Syn.Ceibo.Span.t }

let code = function
  | CstBuilderError _ -> "TYP1011"
  | UnsupportedSyntax _ -> "TYP1001"
  | IgnoredPatternTypeConstraint _ -> "TYP1004"
  | ParameterLoweredAsPositional _ -> "TYP1005"
  | ApplicationArgumentLoweredAsPositional _ -> "TYP1007"
  | IgnoredTypeAscription _ -> "TYP1008"
  | IgnoredPolymorphicAnnotation _ -> "TYP1009"
  | UnsupportedInterfaceFile _ -> "TYP1010"
  | UnboundName _ -> "TYP2001"
  | TypeMismatch _ -> "TYP2002"
  | ApplicationLabelMismatch _ -> "TYP2005"
  | RecordResolutionError _ -> "TYP2006"
  | OrPatternBindingsMismatch _ -> "TYP2007"
  | UnsupportedSemanticExpression _ -> "TYP2010"
  | RecursiveGroupRequiresSimpleVariableBinders _ -> "TYP2004"

let name = function
  | CstBuilderError _ -> "cst-builder-error"
  | UnsupportedSyntax _ -> "unsupported-syntax"
  | IgnoredPatternTypeConstraint _ -> "ignored-pattern-type-constraint"
  | ParameterLoweredAsPositional _ -> "parameter-lowered-as-positional"
  | ApplicationArgumentLoweredAsPositional _ -> "application-argument-lowered-as-positional"
  | IgnoredTypeAscription _ -> "ignored-type-ascription"
  | IgnoredPolymorphicAnnotation _ -> "ignored-polymorphic-annotation"
  | UnsupportedInterfaceFile _ -> "unsupported-interface-file"
  | UnboundName _ -> "unbound-name"
  | TypeMismatch _ -> "type-mismatch"
  | ApplicationLabelMismatch _ -> "application-label-mismatch"
  | RecordResolutionError _ -> "record-resolution-error"
  | OrPatternBindingsMismatch _ -> "or-pattern-bindings-mismatch"
  | UnsupportedSemanticExpression _ -> "unsupported-semantic-expression"
  | RecursiveGroupRequiresSimpleVariableBinders _ -> "recursive-group-requires-simple-variable-binders"

let severity = function
  | CstBuilderError _
  | UnsupportedSyntax _
  | IgnoredPatternTypeConstraint _
  | IgnoredPolymorphicAnnotation _
  | UnboundName _
  | TypeMismatch _
  | ApplicationLabelMismatch _
  | RecordResolutionError _
  | OrPatternBindingsMismatch _
  | UnsupportedSemanticExpression _
  | RecursiveGroupRequiresSimpleVariableBinders _ -> Error
  | ParameterLoweredAsPositional _
  | ApplicationArgumentLoweredAsPositional _
  | IgnoredTypeAscription _
  | UnsupportedInterfaceFile _ -> Warning

let severity_to_string = function
  | Error -> "error"
  | Warning -> "warning"

let primary_span = function
  | CstBuilderError { builder_error } -> builder_error.span
  | UnsupportedSyntax { syntax_span; _ } -> syntax_span
  | IgnoredPatternTypeConstraint { constraint_span } -> constraint_span
  | ParameterLoweredAsPositional { parameter_span } -> parameter_span
  | ApplicationArgumentLoweredAsPositional { application_span } -> application_span
  | IgnoredTypeAscription { ascription_span } -> ascription_span
  | IgnoredPolymorphicAnnotation { annotation_span } -> annotation_span
  | UnsupportedInterfaceFile { interface_span } -> interface_span
  | UnboundName { reference_span; _ } -> reference_span
  | TypeMismatch { mismatch_span; _ } -> mismatch_span
  | ApplicationLabelMismatch { application_span; _ } -> application_span
  | RecordResolutionError { operation_span; _ } -> operation_span
  | OrPatternBindingsMismatch { pattern_span; _ } -> pattern_span
  | UnsupportedSemanticExpression { expression_span; _ } -> expression_span
  | RecursiveGroupRequiresSimpleVariableBinders { binding_span } -> binding_span

let supported_literal_to_string = function
  | IntLiteral -> "int"
  | FloatLiteral -> "float"
  | BoolLiteral -> "bool"
  | StringLiteral -> "string"
  | CharLiteral -> "char"
  | UnitLiteral -> "unit"

let unsupported_context_tag = function
  | StructureItem -> "structure_item"
  | SignatureItem -> "signature_item"
  | Pattern -> "pattern"
  | Expression -> "expression"

let unsupported_context_to_string = function
  | StructureItem -> "structure item"
  | SignatureItem -> "signature item"
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
  | LiteralOutsideSupportedSubset { supported_literals } -> "literal kind is outside the currently supported subset (supported: "
  ^ String.concat ", " (List.map supported_literal_to_string supported_literals)
  ^ ")"

let application_label_to_string = function
  | PositionalArgument -> "positional"
  | LabeledArgument label -> "~" ^ label
  | OptionalArgument label -> "?" ^ label

let record_context_to_string = function
  | RecordConstruction -> "record construction"
  | RecordUpdate -> "record update"
  | RecordPattern -> "record pattern"
  | RecordFieldAccess -> "record field access"

let render_record_labels = fun labels ->
  String.concat ", " labels

let render_binding_names = fun names ->
  match names with
  | [] -> "(none)"
  | _ -> render_record_labels names

let message = function
  | CstBuilderError { builder_error } ->
      "Syn.build_cst failed before lowering: " ^ builder_error.message
  | UnsupportedSyntax {
    syntax_kind;
    context;
    recovery;
    reason=None;
    _
  } ->
      "unsupported "
      ^ unsupported_context_to_string context
      ^ " lowered using "
      ^ unsupported_recovery_to_string recovery
      ^ ": "
      ^ Syn.SyntaxKind.to_string syntax_kind
  | UnsupportedSyntax {
    syntax_kind;
    context;
    recovery;
    reason=Some reason;
    _
  } ->
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
  | ApplicationArgumentLoweredAsPositional _ ->
      "labeled or optional application arguments are currently lowered as ordinary positional arguments"
  | IgnoredTypeAscription _ ->
      "type ascriptions are currently ignored during lowering"
  | IgnoredPolymorphicAnnotation _ ->
      "explicit polymorphic annotations are currently ignored during lowering"
  | UnsupportedInterfaceFile _ ->
      "interface files are not lowered by the prototype yet"
  | UnboundName { name; _ } ->
      "unbound name: " ^ name
  | TypeMismatch { mismatch=ExpectedActual { expected; actual }; _ } ->
      "type mismatch: expected " ^ expected ^ " but got " ^ actual
  | TypeMismatch { mismatch=TupleArityMismatch { left; right; left_arity; right_arity }; _ } ->
      "type mismatch: tuple arity mismatch ("
      ^ Int.to_string left_arity
      ^ " vs "
      ^ Int.to_string right_arity
      ^ ") between "
      ^ left
      ^ " and "
      ^ right
  | TypeMismatch { mismatch=OccursCheckFailed { variable_id; in_type }; _ } ->
      "type mismatch: occurs check failed for type variable "
      ^ Int.to_string variable_id
      ^ " in "
      ^ in_type
  | ApplicationLabelMismatch { expected_label; actual_labels=[]; _ } ->
      "application is missing an argument for " ^ application_label_to_string expected_label
  | ApplicationLabelMismatch { expected_label; actual_labels; _ } ->
      "application label mismatch: expected "
      ^ application_label_to_string expected_label
      ^ " but remaining arguments were "
      ^ String.concat ", " (List.map application_label_to_string actual_labels)
  | RecordResolutionError { context; reason; _ } -> (
      match reason with
      | UnknownRecordLabels labels -> record_context_to_string context
      ^ " uses unknown record labels: "
      ^ render_record_labels labels
      | AmbiguousRecordLabels labels -> record_context_to_string context
      ^ " is ambiguous for record labels: "
      ^ render_record_labels labels
      | MissingRecordFields labels -> record_context_to_string context
      ^ " is missing record fields: "
      ^ render_record_labels labels
      | IncompatibleRecordLabels labels -> record_context_to_string context
      ^ " labels do not belong to a single record type: "
      ^ render_record_labels labels
    )
  | OrPatternBindingsMismatch { expected_names; actual_names; _ } ->
      "or-pattern alternatives must bind the same names (expected: "
      ^ render_binding_names expected_names
      ^ "; actual: "
      ^ render_binding_names actual_names
      ^ ")"
  | UnsupportedSemanticExpression { summary; _ } ->
      "unsupported semantic expression reached inference: " ^ summary
  | RecursiveGroupRequiresSimpleVariableBinders _ ->
      "recursive groups currently require simple function bindings"

let span_to_json = fun (span: Syn.Ceibo.Span.t) ->
  Data.Json.Object [ ("start", Data.Json.Int span.start); ("end", Data.Json.Int span.end_); ]

let mismatch_to_json = function
  | ExpectedActual { expected; actual } -> Data.Json.Object [
    ("tag", Data.Json.String "expected_actual");
    ("expected", Data.Json.String expected);
    ("actual", Data.Json.String actual);
  ]
  | TupleArityMismatch { left; right; left_arity; right_arity } -> Data.Json.Object [
    ("tag", Data.Json.String "tuple_arity_mismatch");
    ("left", Data.Json.String left);
    ("right", Data.Json.String right);
    ("left_arity", Data.Json.Int left_arity);
    ("right_arity", Data.Json.Int right_arity);
  ]
  | OccursCheckFailed { variable_id; in_type } -> Data.Json.Object [
    ("tag", Data.Json.String "occurs_check_failed");
    ("variable_id", Data.Json.Int variable_id);
    ("in_type", Data.Json.String in_type);
  ]

let supported_literal_to_json = fun literal -> Data.Json.String (supported_literal_to_string literal)

let application_label_to_json = function
  | PositionalArgument -> Data.Json.Object [ ("tag", Data.Json.String "positional"); ]
  | LabeledArgument label -> Data.Json.Object [
    ("tag", Data.Json.String "labeled");
    ("label", Data.Json.String label);
  ]
  | OptionalArgument label -> Data.Json.Object [
    ("tag", Data.Json.String "optional");
    ("label", Data.Json.String label);
  ]

let record_context_to_json = function
  | RecordConstruction -> Data.Json.String "construction"
  | RecordUpdate -> Data.Json.String "update"
  | RecordPattern -> Data.Json.String "pattern"
  | RecordFieldAccess -> Data.Json.String "field_access"

let record_resolution_reason_to_json = function
  | UnknownRecordLabels labels -> Data.Json.Object [
    ("tag", Data.Json.String "unknown_labels");
    ("labels", Data.Json.Array (List.map (fun label -> Data.Json.String label) labels));
  ]
  | AmbiguousRecordLabels labels -> Data.Json.Object [
    ("tag", Data.Json.String "ambiguous_labels");
    ("labels", Data.Json.Array (List.map (fun label -> Data.Json.String label) labels));
  ]
  | MissingRecordFields labels -> Data.Json.Object [
    ("tag", Data.Json.String "missing_fields");
    ("labels", Data.Json.Array (List.map (fun label -> Data.Json.String label) labels));
  ]
  | IncompatibleRecordLabels labels -> Data.Json.Object [
    ("tag", Data.Json.String "incompatible_labels");
    ("labels", Data.Json.Array (List.map (fun label -> Data.Json.String label) labels));
  ]

let names_to_json = fun names -> Data.Json.Array (List.map (fun name -> Data.Json.String name) names)

let unsupported_reason_to_json = function
  | LiteralOutsideSupportedSubset { supported_literals } -> Data.Json.Object [
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
      [ ("builder_error", cst_builder_error_to_json builder_error); ]
  | UnsupportedSyntax {
    syntax_span;
    syntax_kind;
    context;
    recovery;
    reason
  } ->
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
      [ ("constraint_span", span_to_json constraint_span); ]
  | ParameterLoweredAsPositional { parameter_span } ->
      [ ("parameter_span", span_to_json parameter_span); ]
  | ApplicationArgumentLoweredAsPositional { application_span } ->
      [ ("application_span", span_to_json application_span); ]
  | IgnoredTypeAscription { ascription_span } ->
      [ ("ascription_span", span_to_json ascription_span); ]
  | IgnoredPolymorphicAnnotation { annotation_span } ->
      [ ("annotation_span", span_to_json annotation_span); ]
  | UnsupportedInterfaceFile { interface_span } ->
      [ ("interface_span", span_to_json interface_span); ]
  | UnboundName { reference_span; name } ->
      [ ("reference_span", span_to_json reference_span); ("name_text", Data.Json.String name); ]
  | TypeMismatch { mismatch_span; mismatch } ->
      [ ("mismatch_span", span_to_json mismatch_span); ("mismatch", mismatch_to_json mismatch); ]
  | ApplicationLabelMismatch { application_span; expected_label; actual_labels } ->
      [
        ("application_span", span_to_json application_span);
        ("expected_label", application_label_to_json expected_label);
        ("actual_labels", Data.Json.Array (List.map application_label_to_json actual_labels));
      ]
  | RecordResolutionError { operation_span; context; reason } ->
      [
        ("operation_span", span_to_json operation_span);
        ("context", record_context_to_json context);
        ("reason", record_resolution_reason_to_json reason);
      ]
  | OrPatternBindingsMismatch { pattern_span; expected_names; actual_names } ->
      [
        ("pattern_span", span_to_json pattern_span);
        ("expected_names", names_to_json expected_names);
        ("actual_names", names_to_json actual_names);
      ]
  | UnsupportedSemanticExpression { expression_span; summary } ->
      [ ("expression_span", span_to_json expression_span); ("summary", Data.Json.String summary); ]
  | RecursiveGroupRequiresSimpleVariableBinders { binding_span } ->
      [ ("binding_span", span_to_json binding_span); ]

let to_json = fun diagnostic ->
  Data.Json.Object ([
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
