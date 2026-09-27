/* SERVPIPS perf (P0 soundness, convert2 V8): a disjunctive definition in the
   shape of the unmarshall model, (S <> undefined /\ u = S) \/
   (N <> undefined /\ u = ToNumberOp(N)), must not make N a string: the path
   where only S is present (N undefined) survives, and no end reports N : Str.
   Upstream Gillian added N : Str to the type environment when the term
   ToNumberOp(N) was typed (at its construction and whenever it was
   evaluated), so the n === undefined branch was dropped. */
// servpips-example: wpst
var s = symb();
var n = symb();
var u = symb();
var U = undefined;
__servpips_assume(__servpips_fn("or",
  __servpips_fn("and", __servpips_fn("not", __servpips_fn("=", s, U)), __servpips_fn("=", u, s)),
  __servpips_fn("and", __servpips_fn("not", __servpips_fn("=", n, U)), __servpips_fn("=", u, __servpips_fn("toNumber", n)))));
if (n === undefined) {
  __servpips_debug_vt("only S", u);
} else {
  __servpips_debug_vt("N present", u);
}
