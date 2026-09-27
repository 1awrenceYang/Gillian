/* SERVPIPS WP2: fixed-length arrays (shape len: L, the Records shards):
   length is the literal 2, elements 0 and 1 are created lazily, index 2 and
   other names are absent, enumeration is exact. */
__servpips_shapes('{"S0":{"type":"object","props":{"Records":{"ref":"S1"}},"required":["Records"],"additional":"absent","closed":true},"S1":{"type":"array","items":{"ref":"S2"},"len":2},"S2":{"type":"object","props":{"eventName":{"type":"string","enum":["INSERT","MODIFY","REMOVE"]}},"additional":{"type":"any","optional":true}}}');
var e = __servpips_lazy("event", "S0", "input");
var rs = e.Records;
__servpips_debug_vt("length", rs.length);
__servpips_debug_vt("Records[2]", rs[2]);
__servpips_debug_vt("Records.foo", rs.foo);
__servpips_debug_vt("keys", Object.keys(rs).join(","));
var n = 0;
for (var i = 0; i < rs.length; i++) { if (rs[i].eventName === "INSERT") n++; }
__servpips_debug_vt("inserts", n);
