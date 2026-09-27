/* SERVPIPS WP2: an optional member named like an Object.prototype property
   forks into "own" (value <> undefined) and "absent" (the prototype's value
   is found); a required one does not fork. */
__servpips_shapes('{"S0":{"type":"object","props":{"toString":{"type":"string"},"req":{"type":"object","props":{"constructor":{"type":"number"}},"required":["constructor"],"additional":"absent"}},"required":["req"],"additional":{"type":"string","optional":true}}}');
var e = __servpips_lazy("event", "S0", "input");
__servpips_debug_vt("typeof e.toString", typeof e.toString, true);
__servpips_debug_vt("e.req.constructor", e.req.constructor);
