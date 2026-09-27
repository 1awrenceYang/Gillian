/* SERVPIPS WP1 core probe (E3/E5): Math.floor of a symbolic product used to
   abort the whole run (the SMT encoding of m_floor failed). The failing
   configuration may end (unsupported) but the run and its siblings go on;
   with the E5 encoding both branches are explored. */
var r = symb_number();
var n = symb_number();
Assume(0 <= r);
Assume(r < 1);
Assume(0 < n);
var k = 0;
if (symb_bool()) {
  k = 5;
} else if (Math.floor(r * n) === 1) {
  k = 1;
} else {
  k = 2;
}
