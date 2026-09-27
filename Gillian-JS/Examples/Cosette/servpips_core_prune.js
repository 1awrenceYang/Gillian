/* SERVPIPS-EVENTS: end,note,stats,prune:solver */
/* SERVPIPS WP1 core probe (E13): a branch side dropped because the solver
   found it unsatisfiable is reported by a prune event (kept, by, guard,
   guard_orig and the relevant slice of the path condition). */
var x = symb_number();
var r = 0;
if (x > 3) {
  if (x < 2) {
    r = 1;
  } else {
    r = 2;
  }
}
