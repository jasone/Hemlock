# Effects and mutability

Hemlock statically determines all effects and mutability transitively associated
with a type, whether direct or indirect.  Internal effects and mutability are
prohibited in global and message values, though external effects (i.e. outside
the Hemlock execution environment) are allowed.  Internal effects are lexically
bound to a particular actor's local heap, and therefore cannot be supported in
the global heap.  These restrictions are evaluated at compile time, which means
that the programming model is unaffected by type erasure.  The absence of
dynamic validation overhead enables fast message passing, as well as fully
automated migration of compatible long-lived values to the global heap.

Non-parametric types have fixed effects and mutability, whereas parametric types
also have transitive effects and/or mutability that allow differing
parametrizations to be any combination of effectless/effectful and/or
immutable/mutable.  For example, `'a array` is directly immutable, but it may be
transitively mutable depending on the particulars of the type supplied to the
`'a` parameter.  Were we to attempt passing `'a array` in a message, the
compiler would be unable to prove that the message is compatible with the
aforementioned constraints.  Therefore we must specify transitive constraints in
the type, e.g. `'a <.  \& array`.  Similarly, finalization is only supported for
transitively mutable values, e.g. `/& 'a`, because referential transparency of
immutable values makes finalization ill-defined.

Type syntax is rather involved in its most general form, but most of its
complexities only come into play with functorized module types.  Following are
some examples from the Basis library.

```hemlock
'a list
'a /r array
(\& 'k, 'v, 'cmp) map
(\& 'a, 'cmp) set

val finalize: (/& 'a >-> unit) -> /& 'a >-> unit

type 'a elm =
| Nil
| Cons of 'a * ('a, >e) stream
and ('a, >e) stream = ('a elm, >e) Lazy.t
val force: ('a, >e) lazy_t >e-> 'a
```

Parametric type `'a` carries with it effect and mutability parameters, which can
be explicitly written as `>a /a 'a`.  For example, the set type could be written
as `(>a /a\& 'a, >cmp /cmp 'cmp) set`.  The `/a` parameter is constrained to
prohibit mutability, lest key mutation corrupt the set.

Regarding effects versus mutability consider that `'a &array` is a mutable type,
but actual mutation is an effect.

```hemlock
val length: 'a /_ array -> usize
val set: 'a -> usize -> 'a !&array -> unit
```

The `length` function has no effect even though it can operate on a mutable
array, whereas the `set` function has a mutation effect on the array, as
indicated by `!`.

Effects are categorized as internal/external and read/write.  Internal effects
are only tracked for mutable data; indeed reading immutable data is effectless,
and writing immutable data is prohibited.  Internal effects impact mutable
program execution state whereas external effects impact state outside the
program, e.g. file input/output (I/O).  We cannot in general accurately model
external read-only effects, so all external effects are modeled as combined
read-write effects.  We do model internal read-only environment effects, but we
require that internal write effects imply read effects.  These modeling
limitations reduce the total effects combinations from 16 down to 6, `[.!][$]`.

- `.`: Internal read effect
- `!`: Internal read-write effect
- `$`: External read-write effect

Effects are tracked at partial application granularity, though with the
exception of defaults, for optional parameters it typically suffices to think in
terms of whole function applications.  Furthermore, although types can in
principle have sophisticated parametric effects, the need rarely arises outside
of function signatures.  Even function signatures rarely require the full power
of parametric effects.

```hemlock
val unzip_map: ('a >e-> 'c) -> ('b >f-> 'd) -> ('a * 'b) list
  >e>f-> 'c list * 'd list
```

Here `unzip_map` has the combined effects of two callback functions.  Although
this generalizes to an arbitrary number of callback functions, the common cases
are zero and one.  If there is only one parametric effect in a function
signature, the effect parameter name can be omitted, e.g.

```hemlock
val iter2: f:('a -> 'b >-> unit) -> 'a &t -> 'b &t >-> unit
```

Parametric mutability exists primarily to constrain function parameter types,
e.g. so that an input type dictates an output type.  Consider this monomorphic
identity function:

```hemlock
val id: >e /m t -> >e /m t
```

`id` subsumes several less general signatures, e.g.

```hemlock
val id_mutable: &t -> &t
val id_effectful: >e t -> >e t
val id_pure: t -> t
```

This basic concept enables more sophisticated parametric mutability within type
parameters.  For example, suppose we want a record type to contain two `'a
array` fields of distinct parametric mutabilities.

```hemlock
type ('a, /m, /n) t = {
  m: 'a /m array;
  n: 'a /n array;
}
val init: 'a /m array -> 'a /n array -> ('a, /m, /n) t
```

In more involved cases, parametric types require parameters which have no direct
bearing on the type's effects nor mutability.  Such parameters are segregated
into a separate `[...]` parameter list, e.g.

```hemlock
type ('a, /m, 'b, /n, >e) ['accum, 'c] t = {
  m: 'a /m outer;
  n: 'b /n outer;
  f: usize -> 'accum -> 'a -> 'b >e-> 'accum * 'c;
  index: usize;
}
```

## Special function-like builtins

Ordinary calls to pure functions can be optimized away if their return values
are not used.  For example:

```hemlock
let f x y =
  assert (y <> 0); (* Assertions may be disabled at compile time. *)
  x / y

let y = f 4 2 in
let _ = f 1 0 in (* May be optimized away. *)
...
```

However, there are some functions which may halt that we always want to call
such as `abort`, as well as the expansion of `assert`.  The most straightforward
way to achieve this is to implement `halt` as an effectful function:

```hemlock
val halt: string !-> 'a (* The truth really hurts. *)
```

However, this has a most unfortunate ripple effect on APIs; pure functions
become effectful simply because they contain assertions.  A more ergonomic
solution is to make `halt` a keyword that behaves as a builtin function with an
effectless signature:

```hemlock
val halt: string -> 'a
```

Yet `halt` cannot be optimized out; if the basic block containing the `halt`
call is executed, the actor halts.  In fact, `halt` is effectful, but its effect
can never be observed by the actor which calls it, so no correctness issues
arise from treating it specially.

The following are built in as keywords:

```hemlock
halt: string -> 'a
assert: <expr> -> 'a (* May be disabled at compile time. *)
demand: <expr> -> 'a (* Like assert, but cannot be disabled. *)
```

In addition to the built-ins above, `abort` is an effectful wrapper around
`halt`.

```hemlock
val abort: unit !-> 'a
val not_reached: unit -> 'a
val not_implemented: string -> 'a
```

`abort` deserves special explanation, in that it can be used to intentionally
make a halt effectful.  This commonly comes into play if a callback function
needs to have the option of aborting execution, without otherwise producing a
result.  A `halt`-calling *basic block* cannot be optimized out, whereas an
`abort`-calling *function* cannot be optimized out.

The specialness of `halt` can be unintentionally lost behind abstraction layers,
so some care is required in idiomatic uses.  The following would fail to behave
as desired:

```hemlock
let not_reached () =
  halt "Unreachable code reached"

# ...
match expr with
| 0 -> false
| 1 -> true
| _ -> not_reached () # Optimized out!
```

Instead we have to directly call `halt`:

```hemlock
# ...
match expr with
| 0 -> false
| 1 -> true
| _ -> halt "unreachable"
```

Were we to instead implement `not_reached` in terms of `abort`, then its callers
would be effectful.  Thus we have idiomatic `halt` uses rather than `halt`-based
abstractions, e.g.

```hemlock
halt "unreachable"
halt "unimplemented"
```

It would be possible to add additional keywords for these use cases, but the
bloat would not improve code readability.

## Syntax summary

- Effects
  - `>e`: Parametric effect.
  - `>_`: Effects are contextually irrelevant, and therefore arbitrarily
    permitted.
  - `.`: Internal environment read effect.
  - `!`: Internal environment read-write effect if used as e.g. `!->`; parameter
    mutation effect if used as e.g. `!&t`.
  - `$`: External environment read-write effect.
  - `<.`, `<!`, `<$`, `<.$`, `<!$`: Effects constraint: transitively prohibited
    effect(s).
- Mutability
  - `/m`: Parametric mutability.
  - `/_`: Mutability is contextually irrelevant, and therefore both immutable
    and mutable values are supported.
  - `&`: Non-parametric transitive mutability, whether direct and/or indirect.
    `!&t` indicates an effect on the value, which must therefore be mutable.
  - `/&`: Mutability constraint: transitively required mutability.
  - `\&`: Mutability constraint: transitively prohibited mutability.
- Parametric types:
  - `'a`: Parametric type, with implicit effects and mutability parameters,
    equivalent to `>a &a 'a`.
  - `'a &t`: Parametric type with transitive mutability.
  - `val f: 'a -> 'a /t t`: The function produces an isolated value with
    parametric mutability.  The value's mutability is either immutable or
    mutable as determined by type unification in the caller's context.
  - `('a, /m, >e) ['c, 'd] &t`: Mutable type with transitive effects and
    mutability determined by `('a, /m, >e)`, and additional parameters which
    have no impact on the type's transitive effects and mutability determined by
    `['c, 'd]`.
  - `'a <.  \& t`: Parametric type constrained to exclude internal effects and
    mutability.
