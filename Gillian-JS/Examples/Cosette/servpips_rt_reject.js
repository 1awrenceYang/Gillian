// servpips-example: wpst
/* SERVPIPS WP3 test (design E10, D7): a rejected [[Put]] or [[Delete]]
   (whose outcome differs between strict and sloppy code) ends the path
   unsupported instead of throwing or silently failing. */
var c = __servpips_fresh("case", "Num", "input");
function show(tag, v) { __servpips_emit("note", tag, "", v); }
if (c === 0) {
  var frozen = Object.freeze({ a: 1 });
  try { frozen.a = 2; show("put-frozen-returned", frozen.a); } catch (e) { show("put-frozen-threw", 0); }
} else if (c === 1) {
  var o = {};
  Object.defineProperty(o, "x", { value: 1, configurable: false });
  try { delete o.x; show("delete-returned", o.x); } catch (e) { show("delete-threw", 0); }
} else if (c === 2) {
  try { "abc".foo = 1; show("primitive-returned", 0); } catch (e) { show("primitive-threw", 0); }
} else if (c === 3) {
  var ok = { a: 1 };
  ok.a = 2; delete ok.a;
  show("ordinary", [ok.a, "a" in ok]);
}
