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
    v}

    [R_numlit] is the ES2023 StringNumericLiteral grammar over UTF-16 code
    units (StrWhiteSpaceChar = WhiteSpace ∪ LineTerminator of ES2023 with the
    Unicode Zs set of Node 18/20); its SMT-LIB text is {!numlit_regex_text}
    and is reported in the [hello] event ([builtins."str.in_re.numlit"]). *)

open Gil_syntax

type smt = [ `Native of string | `Uf ]

type spec = {
  args : Type.t list;  (** argument types *)
  ret : Type.t;  (** result type *)
  smt : smt;  (** [`Native f]: SMT-LIB primitive [f]; [`Uf]: uninterpreted *)
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
    SMT-LIB semantics (conversions [to_int] = floor). [None] when [name] is
    not a native builtin, an argument has the wrong type, a [Num] argument is
    not finite, or the exact result is not representable as a GIL literal
    (e.g. [str.to_int] of more than 2^53). *)
val eval_concrete : string -> Literal.t list -> Literal.t option

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
