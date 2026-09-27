/* SERVPIPS-ARGS: --unroll 2 */
/* SERVPIPS WP1 core probe (E9): paths that exceed max_branching are not
   silently dropped: each stopped configuration is reported by
   end{truncated, "max_branching"} and counted in stats.max_branch. */
var c = 0;
if (symb_bool()) { c = c + 1; }
if (symb_bool()) { c = c + 1; }
if (symb_bool()) { c = c + 1; }
