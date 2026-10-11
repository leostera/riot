open Std

type id =
  | T0001_UnsupportedSyntax
  | T0002_UnsupportedType
  | T0003_AnnotationMismatch
  | T0004_InfiniteSubstitution
  | T0005_TypeMismatch
  | T0006_UnerasableOptionalArgument
  | T0007_UnresolvedName
  | T0008_TypeConstructorArityMismatch

let id_to_string = function
  | T0001_UnsupportedSyntax -> "T0001"
  | T0002_UnsupportedType -> "T0002"
  | T0003_AnnotationMismatch -> "T0003"
  | T0004_InfiniteSubstitution -> "T0004"
  | T0005_TypeMismatch -> "T0005"
  | T0006_UnerasableOptionalArgument -> "T0006"
  | T0007_UnresolvedName -> "T0007"
  | T0008_TypeConstructorArityMismatch -> "T0008"

let name = function
  | T0001_UnsupportedSyntax -> "unsupported-syntax"
  | T0002_UnsupportedType -> "unsupported-type"
  | T0003_AnnotationMismatch -> "annotation-mismatch"
  | T0004_InfiniteSubstitution -> "infinite-substitution"
  | T0005_TypeMismatch -> "type-mismatch"
  | T0006_UnerasableOptionalArgument -> "unerasable-optional-argument"
  | T0007_UnresolvedName -> "unresolved-name"
  | T0008_TypeConstructorArityMismatch -> "type-constructor-arity-mismatch"

let explain = function
  | T0001_UnsupportedSyntax ->
      "This syntax is parsed by Syn but is not represented by the current Typ.Ast slice yet."
  | T0002_UnsupportedType ->
      "This type syntax is parsed by Syn but is not supported by the current checker slice yet."
  | T0003_AnnotationMismatch ->
      "Change the annotation or update the value so both types agree."
  | T0004_InfiniteSubstitution ->
      "Rewrite the expression so the type variable does not recursively contain itself."
  | T0005_TypeMismatch ->
      "Check the expression at this location and make the expected and actual types agree."
  | T0006_UnerasableOptionalArgument ->
      "Optional arguments need a later positional argument before their default can be erased."
  | T0007_UnresolvedName ->
      "Make the name available in the current value, constructor, field, type, or module namespace."
  | T0008_TypeConstructorArityMismatch ->
      "Apply the type constructor to exactly the number of declared type arguments."
