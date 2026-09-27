/* SERVPIPS WP1 core probe (E1): a path on which the program throws an
   uncaught exception ends with end{threw}; the other path with
   end{returned}. */
var x = symb_number();
if (x > 0) {
  throw new Error("boom");
}
