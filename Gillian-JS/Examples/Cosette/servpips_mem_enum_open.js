/* SERVPIPS WP2: enumerating an open input object (Object.keys, for-in) ends
   the path as unsupported ("enumeration of an open object"). */
__servpips_shapes('{"S0":{"type":"object","props":{"a":{"type":"string"}},"additional":{"type":"string","optional":true}}}');
var e = __servpips_lazy("event", "S0", "input");
if (e.a === "k") {
  __servpips_debug_vt("keys", Object.keys(e));
} else {
  var s = "";
  for (var p in e) { s = s + p; }
  __servpips_debug_vt("forin", s);
}
