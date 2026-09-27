// servpips-example: wpst
/* SERVPIPS WP3 regression (design E11b): the JS2JSIL compiler added a
   spurious operand to the error PHI of a function when the init part of a
   for loop is already a value (e.g. an assignment), so that an exception
   thrown later in the function was replaced by another variable ("Undefined
   variable" / wrong value). Upstream fails this program in wpst and exec. */
function d(v) { throw v; }
function h(p, l) { var c, u; for (u = l; p;) { d(42); } }
var caught;
try { h(true, 1); } catch (e) { caught = e; }
__servpips_emit("note", "caught", "", caught);
