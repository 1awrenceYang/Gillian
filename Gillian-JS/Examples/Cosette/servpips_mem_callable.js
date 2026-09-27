// servpips-example: wpst
/* SERVPIPS WP2 round 3 (aws2 design problem 11): typeof and IsCallable of a
   lazy value that is not materialised on the path do not materialise it (no
   lazy class is callable: GetMetadata hands out the value's metadata
   location, and reading @call there is "absent" without choosing a class).
   A json value: one path per JS type, the object one not split into
   Object / Array; reading a member afterwards materialises it (one branch
   per class). A value with a four-class table (M / L / Set / B, as an
   unmarshall view): calling it throws a TypeError on one path, and so does
   the ToPrimitive of "x" + v when its valueOf / toString members are
   (lazy, non-callable) input values. */
__servpips_shapes('{"V":{"type":"object","additional":{"type":"ddb-out","optional":true}}}');
var c = __servpips_fresh("case", "Num", "input");
function R(o, k) { return undefined; }
if (c === 0) {
  var j = __servpips_lazy("j", "json", "input");
  var t = typeof j;
  __servpips_debug_vt("typeof", [t, j]);
  if (t === "object" && j !== null) {
    var k = j.k;
    __servpips_debug_vt("member", k);
  }
} else if (c === 1) {
  var u = __servpips_lazy("u", "V", "skolem", [
    { label: "M", cls: "Object", resolver: R },
    { label: "L", cls: "Array", resolver: R },
    { label: "Set", cls: "Object", resolver: R },
    { label: "B", cls: "Object", resolver: R }
  ]);
  try { u(); } catch (e) { __servpips_debug_vt("call", [typeof u, e instanceof TypeError]); }
} else if (c === 2) {
  var w = __servpips_lazy("w", "V", "input");
  var vo = __servpips_member(w, "valueOf");
  var ts = __servpips_member(w, "toString");
  if (typeof vo === "object" && vo !== null && typeof ts === "object" && ts !== null) {
    try { var s = "x" + w; } catch (e) { __servpips_debug_vt("toPrimitive", e instanceof TypeError); }
  }
}
