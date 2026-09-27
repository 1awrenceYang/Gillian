/* SERVPIPS-ARGS: --servpips-step-budget 30000 */
// servpips-example: wpst
/* SERVPIPS WP1 core probe (per-path step budget): a loop that never forks
   and never exits (here a concrete condition that stays true, like the
   regenerator runtime's dispatch loop on an infeasible aliasing path) must
   not block the exploration: that path ends as end{truncated, "step
   budget"} once it has executed more commands than the budget since its
   last branch, and the other path is still explored and returns. The
   loop body forks on nothing, so --unroll does not stop it. */
var sentinel = {};
var t = sentinel;
var n = 0;
if (symb_bool()) {
  while (t === sentinel) {
    n = n + 1;
  }
}
n;
