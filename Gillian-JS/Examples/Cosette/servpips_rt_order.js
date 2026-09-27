// servpips-example: wpst
/* SERVPIPS WP3 test (round 3): order facts through __servpips_fn("<" | "<="
   | "is_int", ...) are the native GIL FLessThan / FLessThanEqual / IsInt:
   one atom each, no JS comparison fork (a JS "<=" goes through
   i__abstractComparison). Literal arguments are evaluated (IEEE, NaN is
   unordered); a non-number argument ends the path unsupported. */
var n = __servpips_fresh("n", "Num", "input");
var m = __servpips_fresh("m", "Num", "input");
var s = __servpips_fresh("s", "Str", "input");
var c = __servpips_fresh("case", "Num", "input");
function show(tag, v) { __servpips_emit("note", tag, "", v); }
if (c === 0) {
  /* range facts as single atoms; the JS comparisons they decide do not fork */
  __servpips_assume(__servpips_fn("<=", 0, n));
  __servpips_assume(__servpips_fn("<", n, 10));
  __servpips_assume(__servpips_fn("is_int", n));
  if (n < 0 || n >= 10) { show("out-of-range", n); }
  show("n+1", n + 1);
} else if (c === 1) {
  show("lit", [__servpips_fn("<", 1, 2), __servpips_fn("<=", 2, 2), __servpips_fn("<", 2, 1),
               __servpips_fn("<", NaN, 1), __servpips_fn("<=", 1, NaN), __servpips_fn("is_int", 1.5),
               __servpips_fn("is_int", -3), __servpips_fn("FLessThan", -1, 0), __servpips_fn("FLessThanEqual", 0, -0),
               __servpips_fn("IsInt", 0)]);
} else if (c === 2) {
  /* a symbolic order value is a GIL boolean: branching on it forks once */
  if (__servpips_fn("<", n, m)) { show("lt", 1); } else { show("ge", 0); }
} else if (c === 3) {
  show("not-a-number", __servpips_fn("<", s, 1));
} else if (c === 4) {
  /* JS relational operators on symbolic numbers: with --servpips
     i__abstractComparison compares nx < ny directly (no fork on nx = ny
     first), so each comparison splits into exactly two paths */
  if (n <= m) { show("n<=m", 1); } else { show("n>m", 0); }
} else if (c === 5) {
  show("values", [n < m, n > m]);
} else if (c === 6) {
  /* concrete corner cases: -0 / +0, infinities, NaN, strings */
  show("concrete", [0 < -0, -0 <= 0, -0 >= 0, Infinity < Infinity, Infinity <= Infinity,
                    -Infinity < Infinity, NaN < 1, NaN >= NaN, 1 > NaN, "a" < "b", "10" < "9", "10" < 9, null < 1, undefined < 1]);
}
