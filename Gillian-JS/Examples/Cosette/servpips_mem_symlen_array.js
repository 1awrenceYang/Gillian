/* SERVPIPS WP2 (phase 2): JSON arrays of symbolic length len(<name>):
   index reads fork on i < len; a loop over the array is bounded by the
   is_int/0 <= len facts; enumeration needs a concrete length; a symbolic
   index gives an arbitrary element (elem(<name>)#k) or no element, and the
   same symbolic index read twice gives the same value. */
var a = __servpips_lazy("JSON.parse(event.body)", "json", "input");
if (Array.isArray(a)) {
  if (a.length < 3) {
    var s = 0;
    for (var i = 0; i < a.length; i++) { s = s + 1; }
    __servpips_debug_vt("count", s);
    __servpips_debug_vt("keys", Object.keys(a).join(","));
  } else {
    var j = a.length - 1;
    var last = a[j];
    __servpips_debug_vt("last", last);
    __servpips_debug_vt("same", a[j] === last);
  }
}
