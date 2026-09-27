// servpips-example: wpst
/* SERVPIPS WP2 round 3 (aws2 design problem 3): existence after a program
   write on an object whose members a resolver defines. A program object
   with a resolver (a program-object view of the models, members defined
   with __servpips_define: @sp_lazykeys, "k exists iff its value is not
   undefined"): after the program writes undefined to a defined key, the key
   exists; after it deletes one, it does not; a key the program did not
   write keeps the rule. The same holds on a lazy view (an object of a
   class with a resolver). */
function R(o, k) {
  if (k === "A" || k === "B" || k === "C") { __servpips_define(o, k, undefined); return undefined; }
  __servpips_absent(o, k);
  return undefined;
}
var v = {};
__servpips_mark(v, "resolver", R);
var a0 = v.A, b0 = v.B, c0 = v.C;
var before = ["A" in v, "B" in v, "C" in v];
v.A = undefined;
delete v.C;
__servpips_debug_vt("program view", [before, "A" in v, "B" in v, "C" in v, v.hasOwnProperty("A"), "Z" in v]);
var u = __servpips_lazy("u", "object", "skolem", [{ label: "V", cls: "Object", resolver: R, open: true }]);
var ua = u.A, ub = u.B;
var ubefore = ["A" in u, "B" in u];
u.A = undefined;
__servpips_debug_vt("lazy view", [ubefore, "A" in u, "B" in u, __servpips_is_lazy(u)]);
