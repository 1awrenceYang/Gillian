/* SERVPIPS WP2 (enumeration of closed structs, fail-closed cases): more
   than 3 non-index members that may be present (their order would need
   more than 6 paths) and a program write before the first enumeration
   (positions of written keys are not tracked) are unsupported. */
__servpips_shapes('{"O":{"type":"object","props":{"key":{"type":"string"},"size":{"type":"number"}},"required":["key","size"],"additional":"absent"},"B":{"type":"object","props":{"name":{"type":"string"},"arn":{"type":"string"},"ownerIdentity":{"type":"object","props":{"principalId":{"type":"string"}},"required":["principalId"],"additional":"absent"},"x":{"type":"string"}},"required":["name","arn","ownerIdentity","x"],"additional":"absent"}}');
var f = __servpips_lazy("event.flag", "boolean", "input");
if (f) {
  var b = __servpips_lazy("event.bucket", "B", "input");
  __servpips_debug_vt("bucket keys", Object.keys(b));
} else {
  var w = __servpips_lazy("event.object", "O", "input");
  w.key = "k";
  __servpips_debug_vt("written keys", Object.keys(w));
}
