/* SERVPIPS WP2 (E17): writes into lazy values obtained without a concrete
   member cell are still visible in the ancestors' value trees: a write into a
   prefetched member of an unmaterialised object appears in its "written"
   list; a write into an element read through a symbolic index makes the
   array opaque (it cannot be placed). */
__servpips_shapes('{"S0":{"type":"object","props":{"m":{"type":"object","additional":{"type":"string","optional":true}},"arr":{"type":"array","items":{"type":"object","additional":{"type":"string","optional":true}}}},"additional":"absent"}}');
var e = __servpips_lazy("event", "S0", "input");
var m = __servpips_member(e, "m");
m.w = "x";
__servpips_debug_vt("prefetched write, parent unmaterialised", e);
var e2 = __servpips_lazy("event2", "S0", "input");
var arr = e2.arr;
if (Array.isArray(arr)) {
  var j = arr.length - 1;
  var el = arr[j];
  if (el !== undefined) { el.w = "y"; }
  __servpips_debug_vt("elem write", e2);
}
