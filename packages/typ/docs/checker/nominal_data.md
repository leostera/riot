# Typ Nominal Data

This document specifies the next checker slice for `typ`:

- ordinary type declarations
- ordinary variants
- records
- constructor and label environments
- constructor and record use in expressions and patterns

This builds directly on top of [checker.md](./checker.md).

The point here is simple: `checker.md` gives us a tiny HM-style core. This
document adds named data to it without dragging in GADTs, rows, modules, or
other heavier machinery yet.

## 1. Scope

This slice includes:

- abstract type declarations
- ordinary variant declarations
- ordinary record declarations
- constructor expressions
- constructor patterns
- record construction
- record field access
- record update
- record patterns

This slice does not include:

- GADT return annotations
- constructor existentials
- extensible variants
- private types
- private constructors or labels
- polymorphic variants
- row types
- object types
- module types or first-class modules
- representation choices such as float-record or unboxed-variant optimizations

Those are all separate features. They should be specified separately.

## 2. What This Slice Adds

`checker.md` gave us:

- base types
- variables
- functions
- application
- `let`
- a restricted `let rec`

That is enough for a tiny functional language, but not for actual OCaml-style
data.

This slice adds nominal data.

The key word there is nominal.

These declarations do not just expand into structure. They introduce named
types, named constructors, and named labels into the typing environment.

So the checker now needs three related things:

1. declaration elaboration
2. environment formation
3. term and pattern rules for using those declarations

That split matches upstream OCaml too. Declaration typing lives mainly in
`typing/typedecl.ml`, term-level use lives mainly in `typing/typecore.ml`, and
the low-level algebra lives in `typing/ctype.ml`.

## 3. Semantic Objects

For this slice, the declaration layer should elaborate source declarations into
semantic objects with roughly this shape:

```text
type_head ::= TypeHead(name, params, kind)

kind ::= Abstract
       | Variant(constructor_decl list)
       | Record(label_decl list)

constructor_decl ::= ConstructorDecl(
  name,
  owner,
  params,
  payload
)

label_decl ::= LabelDecl(
  name,
  owner,
  params,
  field_type,
  mutable
)
```

where:

- `owner` is the declared nominal type constructor
- `params` are the type parameters bound by the owning declaration
- `payload` is the constructor payload shape

For this slice, constructor payloads are either:

```text
payload ::= Nullary
          | TuplePayload(tau list)
```

This keeps ordinary variants separate from records and from GADT result types.

## 4. Type Language Extension

`checker.md` used a very small type language.

This slice extends it with named type application:

```text
tau ::= ...
      | TCon(name, tau list)
```

Examples:

- `int list` is `TCon(list, [int])`
- `color` is `TCon(color, [])`
- `result(int, string)` is `TCon(result, [int, string])`

For this slice, record and ordinary variant declarations elaborate into named
type constructors through `TCon`.

## 5. Environments

This slice extends the environment model from [checker.md](./checker.md).

We now need:

```text
Gamma : value-name -> sigma
Delta : type-name  -> type_head
Kappa : constructor-name -> constructor_decl list
Lambda : label-name -> label_decl list
```

A few things matter here:

- constructors are term-level names
- labels are not ordinary values, but they are still environment-resolved names
- constructors and labels are not assumed to be globally unique
- lookup may return several candidates

That last point is important.

In practice, constructor and label lookup may need expected-type-guided
disambiguation. Upstream OCaml does exactly this through
`extract_concrete_variant`, `extract_concrete_record`, and then constructor or
label lookup plus disambiguation.

## 6. Declaration Elaboration

The checker must elaborate declarations before term inference tries to use them.

For this slice, we care about three declaration forms.

### Abstract Types

```ocaml
type t
type ('a, 'b) pair
```

These introduce a nominal type head into `Delta`, but no constructor or label
entries.

### Ordinary Variants

```ocaml
type color =
  | Red
  | Rgb of int * int * int
```

This introduces:

- a nominal type head `color`
- one constructor declaration for `Red`
- one constructor declaration for `Rgb`

For a declaration:

```text
type ('a1, ..., 'an) t =
  | C1 of tau11 * ... * tau1k
  | ...
  | Cm of taum1 * ... * taumj
```

each constructor exports a scheme:

```text
Ci :
  Forall(a1, ..., an).
    payload_i -> TCon(t, [a1, ..., an])
```

where:

- `payload_i` is omitted for nullary constructors
- `payload_i` is the tuple payload for non-nullary constructors

So:

```ocaml
type 'a option =
  | None
  | Some of 'a
```

exports:

```text
None : Forall(a). option(a)
Some : Forall(a). a -> option(a)
```

### Records

```ocaml
type point = {
  x: int;
  y: int;
}
```

This introduces:

- a nominal type head `point`
- one label declaration for `x`
- one label declaration for `y`

For a declaration:

```text
type ('a1, ..., 'an) t = {
  l1 : tau1;
  ...
  lk : tauk;
}
```

each label declaration carries:

- its owner type `TCon(t, [a1, ..., an])`
- its field type under those parameters
- its mutability

Unlike constructors, labels are not themselves ordinary value schemes. They are
resolved through record operations.

## 7. Constructor Rules

Ordinary variant constructors are term-level entries derived from declarations.

This section only covers ordinary variants. GADT constructors are out of scope.

### Constructor Expressions

To type-check:

```text
C
```

or:

```text
C(e1, ..., en)
```

the checker must:

1. look up constructor candidates in `Kappa`
2. if an expected type is known and concrete enough, use it to narrow the owner
   type
3. choose one constructor declaration or report ambiguity
4. instantiate the constructor scheme with fresh type variables
5. type-check payload expressions against the instantiated payload types
6. produce the instantiated owner type as the result type

For ordinary constructors, this does not introduce existentials or local type
equalities.

### Constructor Patterns

To type-check a constructor pattern against an expected scrutinee type:

```text
C(p1, ..., pn)
```

the checker must:

1. look up constructor candidates in `Kappa`
2. use the expected scrutinee type to disambiguate when possible
3. instantiate the constructor declaration
4. unify the instantiated constructor result type with the expected scrutinee
   type
5. type-check each payload pattern against the instantiated payload type

For ordinary constructors, this is just matching against the owning nominal
type. No refinement beyond that is part of this slice.

So constructor patterns in this slice do not:

- introduce type equalities
- introduce existentials
- refine the type by matching a special result annotation

Those are all GADT rules and belong in a later document.

## 8. Record Rules

Records are nominal too, but they differ from variants in one big way:
operations are label-based, not constructor-based.

### Record Construction

To type-check:

```text
{ l1 = e1; ...; lk = ek }
```

the checker must:

1. resolve the labels through `Lambda`
2. use an expected record type when available to narrow the owner type
3. require that all labels belong to the same owning record type
4. instantiate the owner type parameters
5. type-check each field expression against the corresponding field type
6. require all required fields of the record type to be present
7. produce the instantiated owner type

So record construction is not “a bag of fields with compatible names”. It is
construction of one specific nominal record type.

### Record Field Access

To type-check:

```text
e.l
```

the checker must:

1. type-check `e`
2. resolve `l` through `Lambda`
3. use the type of `e` to disambiguate the owning record type when possible
4. unify the type of `e` with the owner record type of the chosen label
5. return the corresponding field type

### Record Update

To type-check:

```text
{ e with l1 = e1; ...; lk = ek }
```

the checker must:

1. type-check the base record expression `e`
2. resolve each updated label
3. require all updated labels to belong to the same owner record type
4. unify the base expression with that owner record type
5. type-check each replacement expression against the corresponding field type
6. return the owner record type

This is still one nominal record operation. It is not structural patching.

### Record Patterns

To type-check a record pattern against an expected scrutinee type:

```text
{ l1 = p1; ...; lk = pk }
```

the checker must:

1. resolve each label
2. use the expected scrutinee type to narrow the owner record type when
   possible
3. require all labels to belong to the same record type
4. unify the expected scrutinee type with that owner record type
5. type-check each field pattern against the corresponding field type

Record patterns may also carry openness information:

- closed record patterns mention all fields
- open record patterns mention some fields and explicitly ignore the rest

If the semantic tree preserves that distinction, it should preserve it
explicitly. It is semantically meaningful.

## 9. Disambiguation

This slice needs a real disambiguation rule.

Constructor and label names are not typed in a vacuum. They are resolved in the
current environment, and expected type information can narrow the candidate set.

The contract is:

1. raw lookup may return zero, one, or many candidates
2. expected type may narrow those candidates to one owner type
3. if one candidate remains, use it
4. if none remain, report a wrong-kind or wrong-owner style failure
5. if several remain, report ambiguity

This matters for both records and variants.

Without this rule, the spec drifts toward a fake structural language that is
not actually what OCaml is doing.

## 10. What This Slice Does Not Do

This slice is intentionally ordinary.

It does not cover:

- constructor result annotations
- existential variables in constructors
- GADT pattern refinement
- locally abstract types introduced by pattern matching
- row variables or row closure
- polymorphic variant pressure or finalization
- private or extensible declarations

Those are all later specs.

## 11. Mapping To `typ`

This document is still implementation-agnostic, but it implies a few concrete
things for `typ`.

### `TypeDecl`

The semantic declaration summary for `typ` eventually needs to carry more than
just constructor schemes.

For this slice, the declaration summary must be able to represent:

- type name
- type parameters or at least arity
- declaration kind
- constructor declarations
- label declarations

The current prototype `TypeDecl.t` in
[TypeDecl.mli](/Users/leostera/Developer/github.com/leostera/riot/packages/typ/src/TypeDecl.mli)
is narrower than that. That is fine for the prototype, but it is not enough for
full record support.

### `ItemTree`

Type items in
[ItemTree.mli](/Users/leostera/Developer/github.com/leostera/riot/packages/typ/src/ItemTree.mli)
should continue to be body-stable item shells.

This slice implies that a type item must eventually expose enough declaration
data to populate:

- the type environment
- the constructor environment
- the label environment

### `ModuleTypings`

Module summaries should eventually persist exported:

- nominal type heads
- constructor summaries
- label summaries

not just value exports.

That is how later modules will type-check constructor use and record use without
reopening source.

## 12. Relationship To Upstream OCaml

The main upstream extraction points for this slice are:

- record and variant extraction helpers in
  [typecore.ml](/Users/leostera/Developer/github.com/leostera/riot/vendor/ocaml/typing/typecore.ml#L394)
- record expression typing in
  [typecore.ml](/Users/leostera/Developer/github.com/leostera/riot/vendor/ocaml/typing/typecore.ml#L4559)
- field access in
  [typecore.ml](/Users/leostera/Developer/github.com/leostera/riot/vendor/ocaml/typing/typecore.ml#L5903)
- constructor expressions in
  [typecore.ml](/Users/leostera/Developer/github.com/leostera/riot/vendor/ocaml/typing/typecore.ml#L6473)
- constructor patterns in
  [typecore.ml](/Users/leostera/Developer/github.com/leostera/riot/vendor/ocaml/typing/typecore.ml#L2030)
- record patterns in
  [typecore.ml](/Users/leostera/Developer/github.com/leostera/riot/vendor/ocaml/typing/typecore.ml#L2142)
- declaration elaboration in
  [typedecl.ml](/Users/leostera/Developer/github.com/leostera/riot/vendor/ocaml/typing/typedecl.ml#L349)

The same warning as before still applies:

upstream OCaml is the source material, not the spec.

We are extracting the algorithmic contract from it, not copying its current
internal structure.
