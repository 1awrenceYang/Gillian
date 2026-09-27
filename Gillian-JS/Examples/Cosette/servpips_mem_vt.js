/* SERVPIPS WP2 (E17): value trees. Program objects list own enumerable data
   properties (non-enumerable ones are omitted, accessors and functions are
   opaque, cycles are opaque), arrays list items, lazy values are lazy nodes
   whose "written" includes nested writes of lazily created members; a
   nested write makes every ancestor non-pristine. */
__servpips_shapes('{"S0":{"type":"object","props":{"o":{"type":"object","props":{"x":{"type":"number"}},"additional":"absent"},"p":{"type":"object","additional":{"type":"string","optional":true}}},"additional":"absent"}}');
var e = __servpips_lazy("event", "S0", "input");
var q = { b: 1, c: [1, "two", { d: 3 }] };
Object.defineProperty(q, "hidden", { value: 5, enumerable: false });
Object.defineProperty(q, "acc", { get: function () { return 1; }, enumerable: true });
q.f = function () {};
__servpips_debug_vt("q", q);
var cyc = { z: 1 }; cyc.self = cyc;
__servpips_debug_vt("cyc", cyc);
__servpips_debug_vt("unmaterialised", __servpips_lazy("x", "json", "input"));
e.o.x = 7;
var pp = e.p;
__servpips_debug_vt("event", e);
__servpips_debug_vt("pristine e", __servpips_is_lazy(e));
__servpips_debug_vt("pristine e.p", __servpips_is_lazy(pp));
__servpips_debug_vt("pristine e.o", __servpips_is_lazy(e.o));
