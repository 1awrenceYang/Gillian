/* SERVPIPS WP2: reading an input object with a symbolic property name ends
   the path as unsupported ("symbolic property name on input object"). */
__servpips_shapes('{"S0":{"type":"object","props":{"key":{"type":"string"}},"additional":{"type":"string","optional":true}}}');
var e = __servpips_lazy("event", "S0", "input");
var k = e.key;
__servpips_debug_vt("before", k);
__servpips_debug_vt("input", e[k]);
