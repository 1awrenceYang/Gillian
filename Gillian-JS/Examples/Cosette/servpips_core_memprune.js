/* SERVPIPS perf (E13): a memory action that branches on a symbolic property
   name (one branch per field of an object with a known domain, plus the
   absent case) reports the branches it drops as unsatisfiable as prune
   events (by "solver", dropped side = the field-name equality), as the
   interpreter does for its own branches. Here k <> "a" drops the "a" field. */
/* SERVPIPS-EVENTS: note,end,stats,prune:solver */
// servpips-example: wpst
var o = { a: 1, b: 2 };
var k = symb_string();
if (k !== "a") {
  var v = o[k];
  __servpips_debug_vt("v", v);
}
