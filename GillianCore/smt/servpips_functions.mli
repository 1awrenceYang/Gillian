(** @canonical Gillian.Smt.Servpips_functions

    SERVPIPS builtin functions (design section 4.5, interface I6).

    A builtin is applied in GIL as [FuncApp (name, args)]. The engine knows
    each builtin by name:
    - its GIL signature ({!lookup}): argument and result types among [Str]
      ([StringType]), [Num] ([NumberType]) and [Bool] ([BooleanType]);
    - its SMT encoding: [`Native] builtins map to SMT-LIB string/regex
      primitives (with [to_int]/[to_real] conversions for [Num] positions);
      [`Uf] builtins are uninterpreted functions, declared with [declare-fun]
      in every query that uses them;
    - its concrete semantics ({!eval_concrete}), for [`Native] builtins only:
      literal arguments are evaluated exactly as the SMT-LIB function on the
      same GIL literal (a GIL string is a byte string; each byte is one SMT
      character). Uninterpreted functions are never evaluated.

    Builtins are available with or without [--servpips] (their names are
    reserved: they contain a dot and cannot be GIL identifiers).

    Table (names are frozen, I6):
    {v
    str.replace_all(s,p,r)   Str,Str,Str -> Str   native (str.replace_all s p r)
    str.contains(s,p)        Str,Str -> Bool      native (str.contains s p)
    str.prefixof(p,s)        Str,Str -> Bool      native (str.prefixof p s)
    str.suffixof(p,s)        Str,Str -> Bool      native (str.suffixof p s)
    str.indexof(s,p,i)       Str,Str,Num -> Num   native (to_real (str.indexof s p (to_int i)))
    str.substr(s,i,n)        Str,Num,Num -> Str   native (str.substr s (to_int i) (to_int n))
    str.from_int(i)          Num -> Str           native (str.from_int (to_int i))
    str.to_int(s)            Str -> Num           native (to_real (str.to_int s))
    str.in_re.numlit(s)      Str -> Bool          native (str.in_re s R_numlit)
    js.tonumber.isnan(s)     Str -> Bool          UF
    js.tonumber.ispinf(s)    Str -> Bool          UF
    js.tonumber.isninf(s)    Str -> Bool          UF
    js.tonumber.num(s)       Str -> Num           UF
    decodeURIComponent(s)    Str -> Str           UF
    decodeURIComponent.ok(s) Str -> Bool          UF
    String.prototype.toLowerCase(s)  Str -> Str   UF
    String.prototype.toUpperCase(s)  Str -> Str   UF
    path.basename(s), path.dirname(s), path.extname(s), path.normalize(s)
                             Str -> Str           UF
    path.join/<n>(s1..sn)    Str^n -> Str         UF (any n >= 0)
    Date.prototype.toISOString(t)    Num -> Str   UF
    JSON.quote(s)            Str -> Str           UF
    js.num2str(x)            Num -> Str           UF (used by the ToStringOp encoding)
    js.toUint32(x)           Num -> Num           UF (used by the ToUint32Op encoding)
    js.tostring(v)           Any -> Str           defined (decision D-R2-2)
    js.toboolean(v)          Any -> Bool          defined (decision D-R2-2)
    js.looseeq(a,b)          Any,Any -> Bool      defined (decision D-R2-2)
    js.isarray(v)            Any -> Bool          defined (round 3): false for a
                                                  non-object, js.isarray.other(v)
                                                  (UF) for an object location
    ite.str(c,a,b)           Bool,Str,Str -> Str  native (ite c a b)
    ite.num(c,a,b)           Bool,Num,Num -> Num  native (ite c a b)
    v}

    [ite.str] / [ite.num] are the value-level conditional of
    [__servpips_fn("ite", c, a, b)] when both branches are strings /
    numbers (GIL has no conditional expression; Boolean branches use
    [and]/[or]). On a literal condition, or equal branches, they are
    reduced to a branch.

    {b Defined builtins} (decision D-R2-2: conversions of a value whose JS
    type is a union, without forking per type). Their arguments may have
    any type ([Any], [None] in {!spec.args}); their SMT encoding is a
    [define-fun] over [Extended_GIL_Literal], an [ite] over the type
    constructors of the argument:
    - [js.tostring(v)]: ES ToString of a {e primitive} [v]: a string is
      itself, a (finite) number [n] is the [ToStringOp] encoding of [n] (the
      check's [numToStr] shape: [str.from_int] for integers of magnitude
      below 1e21, [js.num2str] otherwise), [true]/[false] are ["true"] /
      ["false"], [null] is ["null"], [undefined] is ["undefined"]. For any
      other value (objects, GIL-internal values) the result is unspecified
      (the uninterpreted [js.tostring.other(v)]): ToString of an object calls
      JS methods, so the JSIL runtime only builds [js.tostring(v)] when the
      path condition implies that [v] is primitive.
    - [js.toboolean(v)]: ES ToBoolean of any JS value: [undefined], [null]
      are false, a boolean is itself, a number is [v <> 0] (NaN only exists
      as a literal and is evaluated), a string is [v <> ""], an object
      (location) is true; unspecified ([js.toboolean.other(v)]) for
      GIL-internal values.
    - [js.looseeq(a, b)]: ES IsLooselyEqual ([a == b]) of two {e primitive}
      values: same type: strict equality; [null]/[undefined] equal each
      other and nothing else; number and string: the string's ToNumber is
      finite ([str.in_re.numlit], not [js.tonumber.ispinf], not
      [js.tonumber.isninf]) and its value ([ToNumberOp] encoding) equals the
      number; a boolean is compared as the number 0/1 (recursively, with
      the rules above); two locations are equal iff identical; [null] /
      [undefined] and a location are not equal. Unspecified
      ([js.looseeq.other(a, b)]) when an object is compared with a boolean,
      number or string (ToPrimitive calls JS methods) or for GIL-internal
      values.
    - [js.isarray(v)] (round 3): ES Array.isArray of a JS value: false for
      every non-object; for an object (location) the uninterpreted
      [js.isarray.other(v)] -- the solver does not see the class of a
      location; [__servpips_fn("js.isarray", o)] decides it from the
      [@class] metadata when the heap gives a literal class. The converter
      translates it exactly ([(_ is js.arr) v]).
    In SMT, [js.tostring(x)] of an argument of known native type is encoded
    as the conversion of that type (no [define-fun]); [js.tostring(x)] of a
    logical variable that the query restricts to non-number types (a
    top-level conjunct [typeOf x == T1 \/ x == l \/ ...] with no number
    alternative) uses [js.tostring.nonum] (the same body without the numeric
    branch, equal to [js.tostring] in every model of the query).
    On literal arguments all three are evaluated ({!eval_concrete}, exact ES
    semantics including NaN/Infinity/-0); where the result is unspecified
    they are not evaluated.

    [R_numlit] is the ES2023 StringNumericLiteral grammar over UTF-16 code
    units (StrWhiteSpaceChar = WhiteSpace ∪ LineTerminator of ES2023 with the
    Unicode Zs set of Node 18/20); its SMT-LIB text is {!numlit_regex_text}
    and is reported in the [hello] event ([builtins."str.in_re.numlit"]). *)

open Gil_syntax

type smt = [ `Native of string | `Uf | `Defined ]

type spec = {
  args : Type.t option list;
      (** argument types; [None]: any type (only the [`Defined] builtins) *)
  ret : Type.t;  (** result type *)
  smt : smt;
      (** [`Native f]: SMT-LIB primitive [f]; [`Uf]: uninterpreted;
          [`Defined]: engine-side definition over GIL values (see above) *)
}

(** Signature and encoding of a builtin; [None] if [name] is not a builtin. *)
val lookup : string -> spec option

val is_builtin : string -> bool

(** Is [name] a builtin whose result is [Bool]? (Used by
    [Expr.is_boolean_expr].) *)
val is_bool : string -> bool

(** [Func.t] records for the builtins (parameters [x0..], typed;
    definition [Lit Nono], never unfolded), to be added to [prog.funcs].
    [path.join/<n>] is listed for n = 0..16 (larger n are still accepted by
    {!lookup}). These records are also installed in
    [Prog_env.Function_env.builtins]. *)
val funcs_for_prog : unit -> (string * Func.t) list

(** Concrete evaluation of a [`Native] builtin on literal arguments, with the
    SMT-LIB semantics (conversions [to_int] = floor), and of a [`Defined]
    builtin with its ES semantics. [None] when [name] is not a native or
    defined builtin, an argument has the wrong type, a [Num] argument is
    not finite (native builtins), the exact result is not representable as
    a GIL literal (e.g. [str.to_int] of more than 2^53), or the result of a
    defined builtin is unspecified (see above). *)
val eval_concrete : string -> Literal.t list -> Literal.t option

(** ES ToString of a primitive literal ([None] for other literals). *)
val js_tostring : Literal.t -> string option

(** ES ToBoolean of a JS literal (locations are objects: [true]). *)
val js_toboolean : Literal.t -> bool option

(** ES IsLooselyEqual of two literals ([None] when ToPrimitive of an object
    would be needed, or for non-JS literals). *)
val js_looseeq : Literal.t -> Literal.t -> bool option

(** The StrWhiteSpaceChar code units, as inclusive ranges. *)
val ws_ranges : (int * int) list

(** Concrete [str.in_re.numlit] on a GIL (byte) string: the SMT regex
    [R_numlit] matched with one character per byte. *)
val numlit_matches : string -> bool

(** The regular expression [R_numlit] as an SMT-LIB term (only
    [str.from_code] applications to integer literals, no string literals).
*)
val numlit_regex : unit -> Sexplib.Sexp.t

(** [Sexplib.Sexp.to_string (numlit_regex ())]: the exact text sent to the
    solver. *)
val numlit_regex_text : unit -> string

(** The [builtins] object of the [hello] event:
    [{"str.in_re.numlit": numlit_regex_text ()}]. *)
val hello_json : unit -> Yojson.Safe.t
