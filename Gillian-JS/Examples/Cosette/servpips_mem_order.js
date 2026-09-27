/* SERVPIPS WP2 (E15): with --servpips own properties are enumerated in
   ES2020 OrdinaryOwnPropertyKeys order, as Node does: array-index keys
   ascending, then the other keys in creation order (Node prints
   2,10,b,a,zz,c / 2,10,b,a,zz,c, / claimId,status,amount); value trees use
   the same order. */
var o = {}; o.b = 1; o.a = 2; o["10"] = 3; o["2"] = 4; o.zz = 5; o.c = 6;
__servpips_debug_vt("keys", Object.keys(o).join(","));
var s = ""; for (var k in o) { s = s + k + ","; }
__servpips_debug_vt("forin", s);
var p = { claimId: "x", status: "y", amount: 1 };
__servpips_debug_vt("lit", Object.keys(p).join(","));
delete p.claimId; p.claimId = "z";
__servpips_debug_vt("re-added", Object.keys(p).join(","));
__servpips_debug_vt("vt", o);
