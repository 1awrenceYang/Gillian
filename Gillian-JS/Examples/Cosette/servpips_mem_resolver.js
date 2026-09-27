/* SERVPIPS WP2 x WP3 (E11 resolver hook, __servpips_define through the
   memory action SpDefine): on a miss, o__getOwnProperty calls the class's
   resolver, which defines the key (a raw write, not a program write; the
   value tree still lists x, whose defined value is not the input's own
   member u.x); "x" in u is true; a key the resolver declares absent does
   not exist; enumerating the view is unsupported. */
function R(o, k) {
  if (k === "gone") { __servpips_absent(o, k); return; }
  __servpips_define(o, k, "v:" + k);
}
var u = __servpips_lazy("u", "object", "skolem", [{ label: "V", cls: "Object", resolver: R, open: true }]);
__servpips_debug_vt("u.x", u.x);
__servpips_debug_vt("x in u", ["x" in u, "gone" in u, u.gone]);
__servpips_debug_vt("u", u);
__servpips_debug_vt("keys", Object.keys(u));
