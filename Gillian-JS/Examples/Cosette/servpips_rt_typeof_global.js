// servpips-example: wpst
/* SERVPIPS WP3 test (integration e11149c, i__hasBinding): under --servpips
   the global object is a model object (@sp_model), yet an identifier bound
   in no scope is resolved without the model-miss hook: typeof gives
   "undefined" (as in Babel's '"function" == typeof Symbol'), a read throws
   a ReferenceError; member accesses on the global object (glob.X, "X" in
   glob) keep the hook and end the path unsupported. */
var c = __servpips_fresh("case", "Num", "input");
var glob = this;
__servpips_mark(glob, "model", true);
function show(tag, v) { __servpips_emit("note", tag, "", v); }
if (c === 0) {
  show("typeof", [typeof Symbol, typeof NotAGlobal, typeof Object, "function" == typeof Symbol]);
} else if (c === 1) {
  try { NotAGlobal; show("no-error", 0); } catch (e) { show("ReferenceError", e instanceof ReferenceError); }
} else if (c === 2) {
  show("member", glob.NotAGlobal);
} else if (c === 3) {
  show("in", "NotAGlobal" in glob);
} else if (c === 4) {
  show("bound", [typeof glob.Object, Object === glob.Object]);
}
