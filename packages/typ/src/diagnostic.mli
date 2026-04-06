open Std

(** Structured diagnostics produced by the prototype lowering and inference
    stages.

    Each diagnostic constructor is self-contained: it carries its own primary
    source location plus any structured payload that describes what happened.
    Rendering helpers such as {!message} and {!to_json} are derived views over
    that structured value. *)
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
type signature_mismatch =
  | MissingValue of { name: string }
  | ValueTypeMismatch of { name: string; expected: string; actual: string }
  | MissingTypeDeclaration of { name: string }
  | TypeDeclarationMismatch of { name: string; expected: string; actual: string }
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
  | SignatureInclusionError of {
      mismatch_span: Syn.Ceibo.Span.t;
      counterpart_span: Syn.Ceibo.Span.t option;
      mismatch: signature_mismatch
    }
  | UnsupportedSemanticExpression of { expression_span: Syn.Ceibo.Span.t; summary: string }
  | RecursiveGroupRequiresSimpleVariableBinders of { binding_span: Syn.Ceibo.Span.t }
val code: t -> string

val name: t -> string

val message: t -> string

val severity: t -> severity

(** Severity classification for rendering and command exit behavior. *)
val primary_span: t -> Syn.Ceibo.Span.t

val severity_to_string: severity -> string

val to_string: t -> string

val to_json: t -> Data.Json.t
