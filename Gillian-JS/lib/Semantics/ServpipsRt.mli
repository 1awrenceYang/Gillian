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
      no such key. [[GetOwnProperty]] returns [undefined] for a key in
      [@sp_lazykeys] whose value is [undefined] (so [in], [hasOwnProperty],
      [propertyIsEnumerable], [getOwnPropertyDescriptor], [[CanPut]],
      [[Delete]] follow).
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
      encoding is WP1's).
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
      [arith(<op>)@<file:line:col>#k] with [decl]/[note{havoc}], overflow
      branches [note{overflow-fork}]).
    - [servpips_tonumber(s)] (JSIL [i__toNumber] on strings): literal ->
      concrete ToNumber; symbolic under [--servpips] -> the four branches of
      section 4.4 (NaN / +Infinity / -Infinity / [ToNumberOp s]) with their
      axioms; without [--servpips], [ToNumberOp s] (upstream behaviour).
    - [servpips_rejected(reason)] (JSIL [put], [deleteProperty],
      [i__putValue] rejection): under [--servpips] ends the path
      [unsupported(reason)]; otherwise returns [undefined] and the runtime
      continues as upstream.

    Solver or encoding failures inside a handler end the path [unsupported]
    (never a silent drop). *)

(** Names accepted by [servpips_fn], with their arity ([None]: at least 2
    arguments for [and]/[or]; [path.join/<n>] takes [n]). *)
val builtin_names : (string * int option) list

(** Registers the handlers (idempotent). *)
val init : unit -> unit
