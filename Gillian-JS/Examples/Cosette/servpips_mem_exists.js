/* SERVPIPS WP2 x WP3 (design section 3.2 existence, E11): existence tests
   of the JSIL runtime over LazyJSON members. A lazily created member that
   was not written exists iff its value is not undefined: "a" in e, the
   own-property test of b, and getOwnPropertyDescriptor fork on that (the
   descriptor test does not fork again once "a" in e decided it); a required
   member always exists; a key a closed struct excludes never exists (no
   fork). After e.c = undefined, c exists; after delete e.a, a does not.
   Writing a new key (c, and toString, a name of Object.prototype) neither
   creates nor declares the input member, nor forks. 4 paths. */
__servpips_shapes('{"S0":{"type":"object","props":{"a":{"type":"string","optional":true},"n":{"type":"number"}},"required":["n"],"additional":{"type":"string","optional":true}},"C":{"type":"object","props":{"k":{"type":"string"}},"required":["k"],"additional":"absent"}}');
var e = __servpips_lazy("event", "S0", "input");
var hop = Object.prototype.hasOwnProperty;
var r = { in_a: "a" in e };
r.own_b = hop.call(e, "b");
r.enum_n = Object.prototype.propertyIsEnumerable.call(e, "n");
var d = Object.getOwnPropertyDescriptor(e, "a");
r.desc_a = d === undefined ? "none" : d.enumerable;
e.c = undefined;
r.in_c = "c" in e;
e.toString = "t";
r.own_ts = hop.call(e, "toString");
delete e.a;
r.in_a2 = "a" in e;
var c = __servpips_lazy("closed", "C", "input");
r.in_x = "x" in c;
__servpips_debug_vt("r", r, true);
__servpips_debug_vt("event", e);
