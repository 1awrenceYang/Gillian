// servpips-example: wpst
/* SERVPIPS WP3 test (design E11, section 3.2 / 4.3): an object with a
   resolver (@sp_resolver) and an unknown key set (@sp_open), in the shape of
   the unmarshall view. [[GetOwnProperty]] misses call the resolver, which
   defines the member (__servpips_define: direct cell + @sp_lazykeys) or
   records its absence (__servpips_absent). For a key in @sp_lazykeys the
   property exists iff its value is not undefined: in, hasOwnProperty,
   propertyIsEnumerable, getOwnPropertyDescriptor follow; enumeration ends
   the path unsupported. */
var c = __servpips_fresh("case", "Num", "input");
var b = __servpips_fresh("present", "Bool", "decision");
var V = {};
var calls = 0;
function R(o, k) {
  calls = calls + 1;
  if (k === "a") { __servpips_define(o, "a", 1); }
  else if (k === "u") { __servpips_define(o, "u", undefined); }
  else if (k === "m") { __servpips_define(o, "m", b ? "val" : undefined); }
  else { __servpips_absent(o, k); }
}
__servpips_mark(V, "resolver", R);
__servpips_mark(V, "open", true);
function show(tag, v) { __servpips_emit("note", tag, "", v); }
if (c === 0) {
  show("a", [V.a, "a" in V, V.hasOwnProperty("a"), V.propertyIsEnumerable("a"), calls]);
  show("a-again", [V.a, calls]);
} else if (c === 1) {
  show("u", [V.u, "u" in V, V.hasOwnProperty("u"), Object.getOwnPropertyDescriptor(V, "u") === undefined]);
} else if (c === 2) {
  show("zz", [V.zz, "zz" in V, V.hasOwnProperty("zz")]);
} else if (c === 3) {
  /* existence of a member whose value may be undefined forks on the value */
  show("m", ["m" in V, V.m]);
} else if (c === 4) {
  /* a write makes the member an ordinary property */
  V.u = 2;
  show("u-written", ["u" in V, V.u]);
} else if (c === 5) {
  Object.keys(V);
  show("keys-returned", 0);
} else if (c === 6) {
  for (var k in V) { show("for-in", k); }
  show("for-in-returned", 0);
}
