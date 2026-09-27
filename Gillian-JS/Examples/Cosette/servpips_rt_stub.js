// servpips-example: wpst
/* SERVPIPS WP3 test (probes RV4/RV9, design D12): an unmodelled ES2015+
   member installed as a stub that ends the path unsupported
   (__servpips_emit("end", ...), as the models' __sp.unsupported does) is not
   swallowed by a surrounding try/catch: the path ends, no "threw" or
   "returned" result is reported for it. */
var c = __servpips_fresh("case", "Num", "input");
Array.prototype.includes = function () {
  __servpips_emit("end", "unsupported", "stub: Array.prototype.includes");
};
var r = "none";
if (c === 0) {
  try { r = [1, 2].includes(1); } catch (e) { r = "threw"; }
  __servpips_emit("note", "after-includes", r);
} else {
  try { r = [1, 2].join("-"); } catch (e) { r = "threw"; }
  __servpips_emit("note", "after-join", r);
}
