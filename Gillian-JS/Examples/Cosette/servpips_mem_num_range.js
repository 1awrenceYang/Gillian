// servpips-example: wpst
/* SERVPIPS WP2 round 3 (aws2 design problem 6b): number ranges of a shape
   (I3 extension: min, max, exclusiveMin, exclusiveMax, int). A value whose
   mask is that number alone gets its type and its range facts (e.g. the
   numbers of DynamoDB: |n| < 1e126), and comparisons beyond the range are
   infeasible; in a union the range is dropped (weaker, no typed comparison
   under a disjunction): u > 10 is feasible. */
__servpips_shapes('{"N":{"type":"number","exclusiveMin":-1e126,"exclusiveMax":1e126},"I":{"type":"number","min":0,"max":10,"int":true},"U":{"type":"union","of":[{"type":"string"},{"ref":"I"}]}}');
var n = __servpips_lazy("n", "N", "input");
var i = __servpips_lazy("i", "I", "input");
var u = __servpips_lazy("u", "U", "input");
__servpips_debug_vt("n", n, true);
if (n >= 1e126) __servpips_debug_vt("unreachable n", n);
if (i > 10 || i < 0 || i === 2.5) __servpips_debug_vt("unreachable i", i);
if (typeof u === "number") {
  if (u > 10) __servpips_debug_vt("u > 10 (union: range dropped)", u);
  else __servpips_debug_vt("u number", u, true);
} else {
  __servpips_debug_vt("u string", u, true);
}
