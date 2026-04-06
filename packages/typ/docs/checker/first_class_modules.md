# Typ First-Class Modules

This document specifies the first-class-module slice for `typ`:

- package types
- packing module expressions into core values
- unpacking packaged values back into module expressions
- unpack patterns and module binders in core syntax
- module-dependent core functions

This builds on top of [checker.md](./checker.md),
[nominal_data.md](./nominal_data.md), and [modules.md](./modules.md).

The point here is simple: first-class modules are the bridge between the core
calculus and the module calculus.

Without them, modules stay in their own world. With them, modules become values
that can move through core expressions, and core expressions gain operations
that can re-enter the module world.

That bridge needs a real spec. It should not be a bag of parser tricks.

## 1. Scope

This slice includes:

- package types
- packing `(module M : S)`
- packing against an expected package type
- unpacking `(val e : S)` back into a module expression
- unpack patterns such as `let (module X : S) = e in ...`
- module binders in function parameters when their package type is known
- dependent result typing for functions over packaged modules

This slice does not include:

- recursive packages
- package subtyping beyond module-type inclusion
- packages carrying submodule constraints other than supported type
  equalities
- existential reasoning beyond what package unpacking already requires
- first-class functors as a separate feature family

Those are all separate extensions. They should be specified separately.

## 2. What This Slice Adds

`modules.md` gave us a separate module calculus.

This slice adds the ability to move between that calculus and the core one.

That means the checker now needs to model three related facts:

1. some core values have package type
2. a package type describes a module type plus a closed set of type equalities
3. unpacking a package reintroduces a module binding whose module type comes
   from that package

This is not “modules but boxed”.

The package boundary throws away some things, keeps some things, and may attach
extra type equalities. That is why package types deserve their own semantic
object.

## 3. Semantic Objects

This slice extends the core type language with package types:

```text
tau ::= ...
      | Package(pack)
      | DepArrow(label, binder, pack, tau)
```

where:

```text
pack ::= Package(module_path, package_constraint list)

package_constraint ::= TypeEq(type_path, tau)
```

`Package(pack)` is the type of a first-class module value.

`DepArrow` is the core-language dependent arrow used for functions whose result
type may depend on the unpacked module argument.

That looks heavyweight, but the intuition is simple:

- `Package(pack)` means “a value containing a module of this package shape”
- `DepArrow` means “a function whose result type may refer to the module
  carried by its packaged argument”

This slice also extends the semantic core and module languages with at least
these bridge forms:

```text
e ::= ...
    | Pack(m, pack option)

m ::= ...
    | Unpack(e, pack option)
```

The optional package annotation exists because OCaml allows both:

- explicit package annotation
- inference from the expected type

## 4. Package Types

A package type is not just a module type with different syntax.

A package type carries:

- a base module path or module type identity
- a closed set of attached type equalities

Conceptually:

```text
(module S with type t = int)
```

becomes:

```text
Package(Package(S, [TypeEq(t, int)]))
```

The important operational rule is:

given a package type, the checker must be able to recover the corresponding
module type.

This is the role upstream gives to `modtype_of_package`.

So this slice requires a total operation:

```text
modtype_of_package(E, pack) : module_type
```

which turns the package boundary back into a module-language type.

## 5. Packing

Packing is the core-to-module bridge.

For:

```text
(module M : S)
```

the checker must:

1. elaborate the package annotation into `Package(pack)`
2. recover the corresponding module type `modtype_of_package(E, pack)`
3. type the module expression `M` against that module type
4. return the core type `Package(pack)`

So packing is not “infer a module, then slap a package wrapper on it”.

It is a checked conversion from a module expression to a core value of package
type.

### Packing Against An Expected Type

OCaml also allows packing without an explicit package annotation when the
expected type is already a package type.

So for:

```text
(module M)
```

in a context expecting `Package(pack)`, the checker must:

1. read the expected package type
2. recover `modtype_of_package(E, pack)`
3. type `M` against that module type
4. return `Package(pack)`

If no annotation exists and no expected package type is available, packing is
ill-typed.

This is the contract behind the familiar “cannot infer package signature”
failure.

## 6. Unpacking

Unpacking is the module-to-core bridge in the other direction.

For:

```text
(val e : S)
```

used as a module expression, the checker must:

1. type the core expression `e`
2. require its type to be `Package(pack)`
3. verify that the package constraints are closed
4. compute `modtype_of_package(E, pack)`
5. return that module type as the type of the unpacked module expression

If the expression is not of package type, the unpack is ill-typed.

If the package type is missing and cannot be recovered from the expected type,
the unpack is ill-typed.

So unpacking is not a cast. It is a checked re-entry into the module calculus.

## 7. Unpack Patterns And Module Binders

This bridge also shows up in binding positions.

For:

```text
let (module X : S) = e in body
```

the checker must:

1. type `e` as `Package(pack)`
2. check that `pack` matches the annotated package type `S`
3. recover `modtype_of_package(E, pack)`
4. extend the environment with a module binding `X`
5. type `body` in that extended environment

This is the same semantic operation as unpacking into a module expression, just
in a binder position.

The exact surface syntax can vary. The rule is the same:

a package-typed value may introduce a module binding when the checker knows the
package shape.

## 8. Module-Dependent Core Functions

This is the part many descriptions of first-class modules skip, but upstream
OCaml does not skip it.

The core language may contain functions whose parameter is a packaged module
and whose result type depends on the unpacked module.

Semantically, this is what `DepArrow` is for.

For a function parameter of package type:

```text
fun (module X : S) -> body
```

the checker must:

1. elaborate the package annotation to `pack`
2. recover `modtype_of_package(E, pack)`
3. extend the module environment with `X : modtype_of_package(E, pack)`
4. type the function body in that extended environment
5. compute either:
   - a dependent arrow `DepArrow(...)` if the result still depends on `X`
   - or an ordinary core arrow if the result no longer depends on `X`

This is exactly the bridge between core function typing and module-level
dependency.

## 9. Applying A Module-Dependent Function

If a core function expects a packaged module argument, applying it follows the
same two-way split we already saw for functor application in [modules.md](./modules.md).

### 9.1 Argument With A Stable Path

If the packed argument comes from a module with a stable path, then the checker
may substitute that path into the dependent result type.

So if:

```text
f : DepArrow(_, X, pack, tau(X))
M : pack
```

and `M` has stable path `P`, then:

```text
f (module M) : tau(P)
```

### 9.2 Anonymous Argument

If the packed argument does not expose a stable path, then the checker cannot
leave the result type depending on that temporary module binder.

So it must compute a dependency-erased result, exactly like the
`nondep_supertype` step in functor application.

If the dependency cannot be eliminated soundly, the application is ill-typed.

This is the first-class-module version of the same rule from the module
calculus:

stable paths preserve dependency, anonymous arguments force dependency
elimination.

## 10. Inclusion And Closedness

This slice needs two auxiliary checks.

### Package Inclusion

Given two package types, the checker compares them by converting both back to
module types and then using module-type inclusion.

So package inclusion is not a separate exotic relation. It is module inclusion
seen through the package boundary.

### Package Closedness

Package constraints must be closed enough to be valid outside the local typing
context.

If a package equality mentions escaping or non-closed type information, the
package is ill-typed.

This is the rule behind upstream checks like `check_package_closed`.

## 11. Queries And Summaries

This slice matters for `typ`'s reusable outputs too.

If a symbol is exported with package type, then its `ModuleTypings` and related
query data need to retain:

- the package shape
- the attached type equalities
- the exact definition origin and span
- enough module-type information to support later inclusion and
  `definition_at`

Otherwise later consumers would be forced to reopen source text or compiler
artifacts just to understand a packaged value.

That would be missing the point.

## 12. Mapping To `typ`

This slice implies a few architectural constraints for `typ`.

1. The core type representation needs a real package constructor.

It cannot treat first-class modules as strings or opaque builtins.

2. The semantic layer needs explicit bridge nodes for pack and unpack.

These are real semantic operations, not surface trivia.

3. Snapshot queries that ask for a type at a pack/unpack site should see the
real package or recovered module type.

4. Stored module summaries must be able to describe exported packaged values in
machine-readable form.

## 13. Relationship To Upstream OCaml

This slice is extracted from upstream OCaml's first-class-module support, but
again, not by transliterating implementation details directly.

The main extraction points are:

- `typing/types.mli`
  for `Tpackage` and `Tfunctor`
- `typing/typecore.ml`
  for packing, unpack patterns, module-dependent functions, and dependent
  application
- `typing/typemod.ml`
  for `modtype_of_package`, package inclusion, package closedness, and unpack
  as a module expression

What we want to preserve is the contract:

- packages are real core types
- packing is checked against a package type
- unpacking recovers a module type from a package
- module binders introduced by unpacking extend the module environment
- dependency survives only when the argument carries a stable path

That is the bridge `typ` needs to keep intact.
