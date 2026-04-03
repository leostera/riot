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

val code: t -> string

val name: t -> string

val message: t -> string

val severity: t -> severity

val primary_span: t -> Syn.Ceibo.Span.t

val severity_to_string: severity -> string

val to_string: t -> string

val to_json: t -> Data.Json.t
