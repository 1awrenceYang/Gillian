/* SERVPIPS perf (P0 soundness): (S <> undefined /\ u = S) \/ u = ToNumberOp(N)
   and then a typeof dispatch on N. For every type of N other than string the
   second disjunct is ill-typed and counts as false (upstream: the whole
   disjunction was untypable; the exception ended the configuration and its
   siblings with an error), so every type of N has a path, the number case
   included. */
// servpips-example: wpst
var s = symb();
var n = symb();
var u = symb();
var U = undefined;
__servpips_assume(__servpips_fn("not", __servpips_fn("=", __servpips_fn("typeof", n), __servpips_fn("typeof", {}))));
__servpips_assume(__servpips_fn("or",
  __servpips_fn("and", __servpips_fn("not", __servpips_fn("=", s, U)), __servpips_fn("=", u, s)),
  __servpips_fn("=", u, __servpips_fn("toNumber", n))));
__servpips_debug_vt(typeof n, u);
