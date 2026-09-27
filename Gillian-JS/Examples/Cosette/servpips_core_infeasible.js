/* SERVPIPS WP1 core probe (E13): a configuration that stops because an
   assumption contradicts its path condition is counted as infeasible (no
   end event); stats.leaves = sum of ends + infeasible. */
var x = symb_number();
Assume(x > 5);
if (symb_bool()) {
  Assume(x < 3);
  x = 1;
} else if (symb_bool()) {
  Assume(false);
  x = 2;
}
