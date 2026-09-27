/* SERVPIPS perf (test262 R3): the deterministic GIL constants of the ES5
   initial heap (Number.MAX_SAFE_INTEGER, Number.EPSILON, Number.MAX_VALUE,
   Math.PI) are numbers in symbolic execution: comparisons with a symbolic
   number branch normally (upstream: reduction error / SMT "constants" failure
   ended both branches unsupported) and arithmetic on them is concrete. */
// servpips-example: wpst
var n = symb_number();
if (n > Number.MAX_SAFE_INTEGER) {
  __servpips_debug_vt("big", 1);
} else {
  __servpips_debug_vt("small", 1);
}
__servpips_debug_vt("pi", n < Math.PI);
__servpips_debug_vt("eps", 2 / Number.EPSILON);
__servpips_debug_vt("max", Number.MAX_VALUE > 1e308);
