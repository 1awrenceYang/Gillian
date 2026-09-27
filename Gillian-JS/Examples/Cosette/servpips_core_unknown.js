/* SERVPIPS-ARGS: --smt-timeout 1 */
/* SERVPIPS WP1 core probe (E3): a solver query answered unknown (1 ms
   timeout on a non-linear query) keeps the path: the branch is treated as
   satisfiable and a note{unknown-assumed-sat} is emitted. */
var x = symb_number();
var y = symb_number();
var z = symb_number();
Assume(1 < x);
Assume(1 < y);
Assume(1 < z);
var r = 0;
if (x * x * x * y + y * y * y * z + z * z * z * x === 17 * x * y * z + 3) {
  r = 1;
}
