/* SERVPIPS WP2 (M1): writes and deletes on a lazy object are private to the
   path: the sibling path still reads the input member and its value tree
   is the unwritten lazy node. */
__servpips_shapes('{"S0":{"type":"object","props":{"flag":{"type":"boolean"},"a":{"type":"string"}},"additional":"absent"}}');
var e = __servpips_lazy("event", "S0", "input");
if (e.flag) { e.a = "written"; delete e.flag; }
__servpips_debug_vt("a", e.a);
__servpips_debug_vt("flag", e.flag);
__servpips_debug_vt("event", e);
__servpips_debug_vt("pristine", __servpips_is_lazy(e));
__servpips_debug_vt("any", __servpips_is_lazy(e, "any"));
