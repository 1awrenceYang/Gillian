/* SERVPIPS WP2 (design section 3.2, enumeration): a lazy input object of a
   closed struct is enumerated exactly. Index members first (E15); an
   optional member forks on its existence (value undefined: absent); the
   JSON text order of the input being unknown, one path per order of the
   present non-index members (versionId absent: 2 orders, present: 6); a
   later enumeration on the same path gives the same order, and a key the
   program adds afterwards comes last. 8 paths. */
__servpips_shapes('{"O":{"type":"object","props":{"key":{"type":"string"},"size":{"type":"number"},"versionId":{"type":"string","optional":true}},"required":["key","size"],"additional":"absent"},"E":{"type":"object","props":{"a":{"type":"string"},"0":{"type":"number"}},"required":["a","0"],"additional":"absent"}}');
var e = __servpips_lazy("event.e", "E", "input");
var ke = Object.keys(e).join(",");
var o = __servpips_lazy("event.object", "O", "input");
var k1 = Object.keys(o).join(",");
var s = ""; for (var k in o) { s = s + k + ","; }
o.extra = 1;
var k3 = Object.keys(o).join(",");
__servpips_debug_vt("keys", [ke, k1, s, k3], true);
