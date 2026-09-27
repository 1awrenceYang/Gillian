(** SERVPIPS externs of the JS compiler / runtime package (design WP3: E4,
    E10, E11, E14, E18; special forms of section 5.2 I2-SF).

    Every handler is registered in {!ServpipsExterns} when this module is
    initialised ({!init} is called from [External.ml], which links it). The
    JS spelling of an extern is [__servpips_<name>(...)] (a special form of
    the JS compiler); [servpips_arith], [servpips_tonumber],
    [servpips_rejected] are emitted by the compiler / the JSIL runtime.

    {2 Internal register object [$lservpips]}

    [main] allocates the object [$lservpips] (JSIL literal location, not
    reachable from JS) right after the initial heap, when the program uses
    [__servpips_at]/[__servpips_site] or when [--servpips] is on. Its cells
    hold raw GIL values (no property descriptors), are part of the path state
    (copied with it) and are:
    - [site], [callee], [seq]: the call-site register written by
      [__servpips_at] before the call instruction; [callee] is [null] when
      the register is empty/consumed;
    - [outcome]: the outcome recorded by [__servpips_emit("outcome", o)]
      ([null] if none);
    - ["k:<name>"]: per-path occurrence counters (numbers) of
      [servpips_arith] havoc names.

    {2 Metadata conventions (JS objects)}

    Metadata cells read by the JSIL runtime hooks (Internals.jsil,
    Object.jsil) and written by the externs below or by the LazyJSON memory
    (WP2):
    - [@sp_model]: [true] for model objects and their prototypes. A [[Get]]
      or HasProperty that misses on the whole prototype chain of an object
      whose chain contains a model object ends the path [unsupported], unless
      the name is [then], [toJSON], [inspect] or [constructor]. Enumerating a
      model object is [unsupported].
    - [@sp_resolver]: a JS function [R]. When [[GetOwnProperty]](o, p) misses
      on [o], the runtime calls [R(o, p)] (this = undefined, JS calling
      convention, exceptions propagate) and looks [p] up again. A resolver
      defines [p] with [__servpips_define] or records its absence with
      [__servpips_absent].
    - [@sp_open]: [true] (anything but [false]) for objects whose key set is
      unknown: enumeration is [unsupported].
    - [@sp_lazy]: present on materialised LazyJSON values (WP2); enumeration
      of such an object is [unsupported] unless its [@class] is ["Array"]
      (then the memory's GetAllProps decides).
    - [@sp_lazykeys]: the keys [k] of the object whose cell was created
      lazily and not written since: "[k] exists" iff its value is not
      [undefined]. Representation: a GIL list of string literals
      ([{{ "a", "b" }}]); a GIL set of strings is accepted too. Absent means
      no such key. The existence tests of the runtime (JSIL
      [i__getOwnPropertyE] / [i__getPropertyE], used by HasProperty ([in],
      Array methods), [hasOwnProperty], [propertyIsEnumerable] and
      [getOwnPropertyDescriptor]) treat such a key as absent when its value
      is [undefined] (forking when that is unknown); [[Get]], [[CanPut]],
      [[DefineOwnProperty]] and [[Delete]] see the (phantom) cell as is,
      which gives the same results, so reading a member never forks.
    - [@sp_kind]: ["blob"|"date"|"stream"|"set"] (value trees).

    {2 Externs}

    - [servpips_site(fn)]: if the register holds a callee [c] (not consumed)
      and [c] is [fn], or [c] is a bound function whose (transitive)
      [@targetFunction] is [fn], returns the site string and consumes the
      register; otherwise returns [null]. Symbolically undetermined equality
      forks.
    - [servpips_emit("call", info, params)]: emits the [call] event. [info]
      is a JS object with own data properties [site], [callee], [api], [sdk]
      (strings), [k] (number), [sent] (boolean) and optionally [phase]
      (["init"|"handler"|"after-settle"], default ["handler"]). [params] is
      serialised as a value tree VT (section 5.1). The value trees of E17
      belong to the LazyJSON memory (WP2, [ServpipsLazy.Ext.serialize]);
      until the packages are integrated a conservative built-in serialiser
      is used (LazyJSON objects become [opaque], property order is integer
      keys then heap order).
    - [servpips_emit("note", code, msg, data)]: emits a [note] event
      ([data] as a value tree, [null] if undefined).
    - [servpips_emit("end", status, reason)]: ends the path
      ([Servpips.Path_end]); status in [returned|threw|truncated|unknown|
      unsupported|error].
    - [servpips_emit("outcome", o)]: records [o] (in
      [$lservpips.outcome]) and emits [note{code:"outcome", msg:o}]; [o] in
      [resolved|rejected|pending|init-threw|no-handler|threw-sync].
    - [servpips_fresh(name, type, kind [, meta])]: a fresh logical variable
      ([Str|Num|Bool] typed, a spec variable like [symb_*]) with a [decl]
      event; [meta] (optional JS object) may give [site], [k], [parent],
      [key], [parent_lvar], [parent_aloc], [shape]. Unsupported under
      concrete execution.
    - [servpips_assume(b)]: [b] must be a GIL boolean; adds it to the path
      condition (the path is dropped when it becomes unsatisfiable).
    - [servpips_fn("<name>", a1, ...)]: builtin function application
      (section 4.5); names are checked against {!builtin_names}. Native GIL
      operations ([and or not = => typeof toNumber toString], and [ite] on
      booleans) are built directly; the others become [FuncApp] (their SMT
      encoding is WP1's). [ite(c, a, b)] with non-Boolean branches (the
      models' value-level conditional): a literal [c] selects a branch; two
      string / two finite-number branches give the builtins [ite.str] /
      [ite.num] (SMT [ite], no fork); other branch types fork on [c]; [c]
      must be a GIL boolean (otherwise [unsupported]). The defined conversions [js.tostring],
      [js.toboolean], [js.looseeq] (decision D-R2-2) are evaluated on
      literal arguments and simplified for values of known type, in both
      modes (under concrete execution a result that is not a literal is
      [unsupported]).
    - [servpips_define(o, k, v)]: writes the data property [k] of [o]
      ([{d, v, true, true, true}]) directly (no [[Put]]) and adds [k] to
      [@sp_lazykeys]. [servpips_absent(o, k)]: writes a tombstone and removes
      [k] from [@sp_lazykeys]. Both use the LazyJSON memory actions
      [SpDefine] / [SpAbsent] when the memory provides them (raw writes that
      are not program writes), otherwise [SetCell] and the metadata.
    - [servpips_mark(o, flag, value)]: sets metadata [@sp_<flag>]; flag in
      [model|resolver|open|kind].
    - [servpips_is_concrete(v)]: [true] iff [v] is a literal (after
      simplification); never forks.
    - [servpips_arith(op, a, b, site)] (compiler): design section 4.4 rules
      1-5 (exact bounded integer arithmetic, division/modulo by a possibly
      zero divisor, non-finite literal operands, havoc
      [arith(<op>)@<file:line:col>#k] with [decl]/[note{havoc}]). Exactness
      (rule 2) also holds for [+]/[-] with an integer literal operand [c]
      ([|c| <= 2^53]) and an integer other operand of magnitude at most
      [2^53 - |c|], and for [*] with an integer literal [c] and an integer
      other operand of magnitude at most [2^53 / |c|] (the result is an
      integer of magnitude <= 2^53). Overflow (rule 5, decision D-R2-1):
      when [|a op b| >= MAX] (the largest double) is satisfiable for a
      havoc result, the overflow case is not explored: it is reported as
      [note{arith-overflow}] and an [end{unsupported, "arith-overflow"}]
      (path condition before the operation), and the path continues with
      the (finite) havoc value only; there are no +/-Infinity branches.
      Callers bound their numbers with sound range facts (lengths, dates,
      DynamoDB numbers, ...) so that no overflow is satisfiable.
    - [servpips_conv(op, v [, w])] (JSIL runtime, decision D-R2-2): called
      by [i__isPrimitive] (op ["isPrimitive"]), [i__toString] (["toString"]),
      [i__toBoolean] (["toBoolean"]) and [i__abstractEquality]
      (["looseEq"], two values). Returns [none] (the procedure continues as
      upstream) without [--servpips], under concrete execution, for
      literals and values of known type, and for a value that can be no
      primitive other than [undefined]/[null] (e.g. an optional object).
      Otherwise the value's JS type is a union that includes a primitive
      type other than undefined/null, and the result is computed without
      forking per type:
      - ["isPrimitive"]: [true] where the value is primitive, [false]
        where it is an object, [none] where it is not a JS value (at most
        three branches);
      - ["toString"]: where the value is primitive, [js.tostring(v)] (or [v]
        itself if it can only be a string); [none] where it is not
        primitive (ToPrimitive of an object calls its methods);
      - ["toBoolean"]: [js.toboolean(v)] where the value is a JS value
        (objects are true; no method is called, so objects do not fork);
        [none] where it is not;
      - ["looseEq"]: [a == null] / [a == undefined] is [(a = null) or (a =
        undefined)] for any value (no fork); otherwise [js.looseeq(a, b)]
        where both values are primitive, [none] where one is not (or
        either is a non-finite number literal).
      The builtins are those of [Smt.Servpips_functions] (design 4.5 table,
      [`Defined]). Every branch condition is checked for satisfiability
      first; a side found unsatisfiable is reported as a [prune] event.
    - [servpips_tonumber(s)] (JSIL [i__toNumber] on strings): literal ->
      concrete ToNumber; symbolic under [--servpips] -> the four branches of
      section 4.4 (NaN / +Infinity / -Infinity / [ToNumberOp s]) with their
      axioms; without [--servpips], [ToNumberOp s] (upstream behaviour).
    - [servpips_rejected(reason)] (JSIL [put], [deleteProperty],
      [i__putValue] rejection): ends the path [unsupported(reason)];
      otherwise (inactive, see below) returns [undefined] and the runtime
      continues as upstream.
    - [servpips_enabled()] (JSIL runtime): [true] iff the SERVPIPS semantics
      is on ([--servpips], wpst or exec). [i__callTarget] (Internals.jsil)
      uses it to call bound functions from the runtime (Array higher-order
      functions, sort comparators, [Function.prototype.call/apply],
      getters/setters, DefaultValue, the resolver hook) with the ES5
      [[Call]] of bound functions ([i__boundCall] / [i__callFunction]);
      without [--servpips] such calls fail as upstream (no [@scope]).
    - Runtime hooks called by the JSIL runtime: [servpips_resolver(l)] (the
      [@sp_resolver] of [l] or [empty]), [servpips_lazykey(l, p)] (is [p] in
      the [@sp_lazykeys] of [l]; a GIL boolean, symbolic for a symbolic
      [p]), [servpips_model_miss(l, p)] (after a miss on the whole prototype
      chain), [servpips_enum_check(l)] (before enumerating [l]).

    The runtime hooks, [servpips_rejected] and the symbolic branching of
    [servpips_tonumber] are {e inactive} (upstream behaviour, heap untouched)
    unless [--servpips] is on or the compiled program uses a SERVPIPS special
    form ([JS2JSIL_Compiler.servpips_forms_used]); this keeps programs
    without SERVPIPS forms identical to upstream and lets concrete-execution
    model tests (which use the forms) see the hooks.

    Solver or encoding failures inside a handler end the path [unsupported]
    (never a silent drop). *)

(** Names accepted by [servpips_fn], with their arity ([None]: at least 2
    arguments for [and]/[or]; [path.join/<n>] takes [n]). *)
val builtin_names : (string * int option) list

(** Registers the handlers (idempotent). *)
val init : unit -> unit
