# Typ Extensible Variants

This document specifies open variants, extension constructors, and exceptions
for `typ`.

This builds on top of [nominal_data.md](./nominal_data.md),
[signatures.md](./signatures.md), and [pattern_analysis.md](./pattern_analysis.md).

The point here is simple: extensible variants are not ordinary variants with a
late `+=`.

They form open constructor families whose membership can grow over time, and
that changes declaration rules, lookup rules, and exhaustiveness.

## 1. Scope

This document covers:

- open variant types
- type extensions
- extension constructor declarations
- extension constructor rebinding
- exceptions as a distinguished extensible family
- constructor use in expressions and patterns
- export and summary consequences

This document does not cover:

- polymorphic variants
- GADT constructor refinement
- object exceptions or class machinery

Those are separate slices.

## 2. Declaration Model

An extensible variant starts from an open type declaration.

Later declarations may extend that type with new constructors.

Conceptually:

```ocaml
type t = ..
type t += A
type t += B of int
```

The important rule is:

the constructor family is open, but still nominal.

Constructors do not float free. They extend one specific nominal type path.

### Example

```ocaml
type message = ..
type message += Ping
type message += Data of string
```

`Ping` and `Data` belong to the `message` family specifically. They are not
just globally-scoped constructors with similar names.

## 3. Extension Constructors

Each extension constructor should elaborate to a semantic constructor
declaration carrying at least:

- its identity
- the type family it extends
- its argument shape
- whether it is a fresh declaration or a rebind
- its definition origin

That means extension constructors belong in the same kind of constructor
environment as ordinary constructors, but with open-family metadata.

## 4. Validity Checks

Extending a type is not always legal.

At minimum, `typ` should reject:

- extending a non-extensible type
- extending a private open type in the wrong mode
- arity mismatches between the extension declaration and the extended type
- variance mismatches between extension parameters and the original family

This is one place where the declaration checker, not the expression checker, is
the right layer.

### Pseudocode

```ocaml
let extend_type type_path ext_decl =
  require (is_open_nominal_family type_path);
  require (arity_and_variance_match type_path ext_decl);
  let ext = elaborate_extension_constructor type_path ext_decl in
  Constructor_env.add ext;
  ext
```

## 5. Rebinding

An extension constructor may also be a rebind rather than a fresh declaration.

That means:

- it points to an existing extension constructor path
- it keeps the referenced constructor's semantic identity
- but it introduces a new visible binding path in the current scope

This is different from declaring a fresh constructor. The checker should keep
that distinction explicit.

## 6. Exceptions

Exceptions are the built-in extensible family `exn`.

So exception declarations should reuse the same conceptual machinery as type
extensions:

- declaration or rebind
- environment insertion
- constructor use in expressions
- constructor use in patterns
- exhaustiveness consequences

There is no need for `typ` to invent a completely separate semantic subsystem
for exceptions if the extension-constructor subsystem already exists.

### Example

```ocaml
exception Timeout
exception Error of string
```

should be treated as additions to the open `exn` family, with the same
constructor-style lookup and export rules as any other extensible family.

## 7. Constructor Use

Using an extension constructor in an expression or pattern should follow the
same broad rule as ordinary constructors:

- look up the constructor
- instantiate its type
- check its payload shape
- unify with the expected family type

The difference is that the family is open, so later analysis must not assume
the set of constructors is closed.

## 8. Exhaustiveness

Open constructor families change pattern analysis.

Matching only the currently-known constructors of an extensible family is not
enough to claim durable exhaustiveness.

So `typ` should preserve the distinction between:

- coverage of the currently-known extension constructors
- semantic exhaustiveness over a truly closed family

This is why wildcard cases matter for exceptions and extensible variants.

## 9. Signatures And Exports

Extension constructors are exportable interface items.

That means:

- signatures may declare them
- implementations may define or rebind them
- `ModuleTypings` should persist them
- cross-module lookup and `definition_at` should be able to find their origins

This is one more reason the canonical summary cannot stop at values and plain
types.

## 10. References

The main upstream extraction points here are:

- `typedecl.mli`
  `transl_type_extension` and `transl_type_exception`
- `typedecl.ml`
  legality checks, constructor elaboration, rebinding, and environment updates
- `typedtree.mli`
  `type_extension`, `type_exception`, and `extension_constructor_kind`

The contract we want to keep is:

- extensible families remain nominal
- extension constructors are real semantic declarations
- rebinding is distinct from fresh declaration
- exhaustiveness stays honest about openness
