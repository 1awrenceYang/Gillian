/* SERVPIPS perf (P0 soundness): the same definition without the N guard in
   the second disjunct, (S <> undefined /\ u = S) \/ u = ToNumberOp(N). The
   type guard "N is a string" of ToNumberOp(N) belongs to its disjunct (the
   SMT encoding asserted it for the whole query), and a disjunct whose
   reduction fails after substituting N = undefined is false (upstream: the
   whole disjunction became false). The n === undefined path survives. */
// servpips-example: wpst
var s = symb();
var n = symb();
var u = symb();
var U = undefined;
__servpips_assume(__servpips_fn("or",
  __servpips_fn("and", __servpips_fn("not", __servpips_fn("=", s, U)), __servpips_fn("=", u, s)),
  __servpips_fn("=", u, __servpips_fn("toNumber", n))));
if (n === undefined) {
  __servpips_debug_vt("only S", u);
} else {
  __servpips_debug_vt("N present", u);
}
