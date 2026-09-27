/* SERVPIPS perf (test262 R2, R6): M_sgn keeps its copysign meaning, which the
   JSIL runtime uses to tell -0 from +0 (SameValue in defineProperty,
   Math.min/Math.max); s-nth accepts the index -0. Expected: min -Infinity,
   max Infinity, threw true, c "a". */
// servpips-example: wpst
var a = Math.min(0, -0);
var b = Math.max(-0, 0);
__servpips_debug_vt("min", 1 / a);
__servpips_debug_vt("max", 1 / b);
var o = {};
Object.defineProperty(o, "x", { value: -0, writable: false, configurable: false });
var threw = false;
try {
  Object.defineProperty(o, "x", { value: 0 });
} catch (e) {
  threw = true;
}
__servpips_debug_vt("threw", threw);
__servpips_debug_vt("c", "abc".charAt(-0));
