// servpips-example: wpst
/* SERVPIPS WP3 test (design E14, section 4.4; probes RV5, RV6): with
   --servpips the numeric operators of the program go through the extern
   servpips_arith. Each case is selected by a symbolic case number, so every
   case is its own set of paths; values are reported as notes. */
var c = __servpips_fresh("case", "Num", "input");
var x = __servpips_fresh("x", "Num", "input");
var y = __servpips_fresh("y", "Num", "input");
var s = __servpips_fresh("s", "Str", "input");
function show(tag, v) { __servpips_emit("note", tag, "", v); }
if (c === 0) {
  /* RV6: IEEE rounding; x*3 is havoc (fresh, unconstrained): both branches
     remain, with a havoc note; the overflow branches are kept too */
  if (x * 3 >= 1) { show("A", x); } else { show("B", x); }
} else if (c === 1) {
  /* RV5: division by a possibly-zero symbolic divisor: y = 0 gives both
     infinities (a = 1 is not 0), y != 0 a havoc value (+ overflow) */
  var q = 1 / y;
  if (q === Infinity) { show("inf", y); } else { show("other", q); }
} else if (c === 2) {
  /* modulo by a possibly-zero divisor: y = 0 gives NaN */
  var r = 5 % y;
  if (r !== r) { show("nan", y); } else { show("num", r); }
} else if (c === 3) {
  /* bounded integers stay exact: string lengths below 100 */
  if (s.length < 100) {
    show("exact", [s.length - 1, s.length + 1, s.length * s.length, s.length / 2, s.length % 3]);
  }
} else if (c === 4) {
  /* concrete operands: IEEE */
  var i = 0; i++; i = i + 2; i *= 3; i = i / 4; i = i % 2;
  show("concrete", [i, 0.1 + 0.2, 1 / 0, -1 / 0, 0 / 0, 7 % -3]);
} else if (c === 5) {
  /* non-finite literal operand with a symbolic one */
  show("inf*x", Infinity * x);
  show("x-inf", x - Infinity);
  show("nan+x", NaN + x);
  show("x/inf", x / Infinity);
} else if (c === 6) {
  /* RV3: after x === 0.1 the operation is concrete (IEEE): 0.1 + 0.2 !== 0.3 */
  if (x === 0.1) {
    if (x + 0.2 === 0.3) { show("rv3-A", x); } else { show("rv3-B", x); }
  }
} else if (c === 7) {
  /* RV7: x + 0.1 is havoc under x >= 0.7: both outcomes of === 0.8 remain */
  if (x >= 0.7) {
    if (x + 0.1 === 0.8) { show("rv7-A", x); } else { show("rv7-B", x); }
  }
} else if (c === 8) {
  /* bounded operands: x * 3 is havoc (x is not known to be an integer) but
     cannot overflow, so there is no overflow branch */
  if (0 <= x && x <= 10) {
    show("bounded", x * 3);
  }
}
