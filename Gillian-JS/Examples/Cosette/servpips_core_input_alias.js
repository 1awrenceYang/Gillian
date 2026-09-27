/* SERVPIPS perf: a LazyJSON input never aliases a program object
   (Reduction.servpips_input_not_loc). Comparing an input with a sentinel
   object, as the Babel regenerator runtime does with every awaited value, has
   one outcome; a regenerator-like loop on that comparison terminates; the
   identity of the input with itself (after materialisation) is unaffected. */
// servpips-example: wpst
__servpips_shapes('{"v":2,"shapes":{"R":{"type":"object","props":{"Item":{"type":"object","optional":true,"additional":{"type":"any","optional":true}}},"additional":"absent"}}}');
var sentinel = {};
var r = __servpips_lazy("response(DynamoDB.GetItem@app.js:1:1:1#1)", "R", "response");
var item = r.Item;
if (item !== sentinel) {
  __servpips_debug_vt("distinct", item === undefined);
} else {
  __servpips_debug_vt("aliased", 0);
}
var u = item;
var n = 0;
while (u === sentinel && n < 50) {
  n++;
}
__servpips_debug_vt("loop", n);
if (item) {
  __servpips_debug_vt("self", item === r.Item);
  __servpips_debug_vt("global", item === this);
}
