/* SERVPIPS WP2 (enumeration of input arrays of symbolic length): with a
   contract bound (maxLen 2) Object.keys forks over the feasible lengths
   0, 1, 2 (each path adds len = n) and is exact; without a small bound
   (only len <= 2^32-1) it is unsupported: 3 paths, each ending there. */
__servpips_shapes('{"R":{"type":"object","props":{"Labels":{"type":"array","items":{"type":"string"},"maxLen":2},"All":{"type":"array","items":{"type":"string"}}},"required":["Labels","All"],"additional":"absent"}}');
var r = __servpips_lazy("resp", "R", "response");
__servpips_debug_vt("keys", Object.keys(r.Labels), true);
__servpips_debug_vt("unbounded", Object.keys(r.All));
