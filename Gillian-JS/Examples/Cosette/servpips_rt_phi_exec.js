/* SERVPIPS WP3 regression (design E11b), concrete execution: see
   servpips_rt_phi.js. The program must run to completion. */
function d(v) { throw v; }
function h(p, l) { var c, u; for (u = l; p;) { d(42); } }
var caught;
try { h(true, 1); } catch (e) { caught = e; }
if (caught !== 42) { throw new Error("wrong exception value"); }
("ok");
