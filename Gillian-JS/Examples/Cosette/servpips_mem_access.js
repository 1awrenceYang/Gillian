/* SERVPIPS WP2: object / array / primitive access on lazy values.
   event.a: string member; event.n: optional number; event.o.x: enum member
   of a closed struct; event.o.y: absent (closed struct); event.zz: open
   member (any); event.tags: fixed-length array (len 2) of strings;
   event.a.length: primitive (string) access, no memory action on a lazy
   object. */
__servpips_shapes('{"v":2,"shapes":{"S0":{"type":"object","props":{"a":{"type":"string"},"n":{"type":"number","optional":true},"o":{"ref":"S1"},"tags":{"type":"array","items":{"type":"string"},"len":2}},"required":["a","o","tags"],"additional":{"type":"any","optional":true}},"S1":{"type":"object","props":{"x":{"type":"string","enum":["p","q"]}},"additional":"absent","closed":true}}}');
var event = __servpips_lazy("event", "S0", "input");
__servpips_debug_vt("a", event.a);
__servpips_debug_vt("n", event.n);
__servpips_debug_vt("o.x", event.o.x);
__servpips_debug_vt("o.y", event.o.y);
__servpips_debug_vt("zz", event.zz);
__servpips_debug_vt("tags.length", event.tags.length);
__servpips_debug_vt("tags[1]", event.tags[1]);
__servpips_debug_vt("tags[2]", event.tags[2]);
__servpips_debug_vt("a.length", event.a.length, true);
