// servpips-example: wpst
/* SERVPIPS WP3 test (decision D-R2-1, design 4.4 rule 5): a havoc'd result
   that may overflow ends the overflow case unsupported("arith-overflow")
   (fail closed) and continues with the finite havoc value only; there are
   no +/-Infinity branches. Sound range facts (as the models assume them for
   Math.random, lengths, Date.now, DynamoDB numbers) make the overflow
   unsatisfiable: no unsupported end. Exactness: integer operands of + / -
   whose exact result has magnitude <= 2^53; for *, an integer literal
   operand c and |x| <= 2^53 / |c|. */
var c = __servpips_fresh("case", "Num", "input");
var x = __servpips_fresh("x", "Num", "input");
var y = __servpips_fresh("y", "Num", "input");
function show(tag, v) { __servpips_emit("note", tag, "", v); }
function isInt(v) { return Math.floor(v) === v; }
if (c === 0) {
  /* Math.random() * length: r in [0, 1), n an integer in [0, 2^32 - 1]:
     havoc (r is not an integer), no overflow */
  if (0 <= x && x < 1 && isInt(y) && 0 <= y && y <= 4294967295) {
    show("random*len", x * y);
  }
} else if (c === 1) {
  /* unbounded product: havoc + one arith-overflow end */
  show("x*y", x * y);
} else if (c === 2) {
  /* Date.now()-like t (integer in [0, 8.64e15]): t + 1000 and t - 60000 are
     exact (literal rule; t exceeds 2^52), t * 1000 and t / 1000 are havoc
     without overflow */
  if (isInt(x) && 0 <= x && x <= 8640000000000000) {
    show("t+1000", x + 1000);
    show("t-60000", x - 60000);
    show("t*1000", x * 1000);
    show("t/1000", x / 1000);
  }
} else if (c === 3) {
  /* length-like n (integer in [0, 2^32 - 1]): n * 1000 exact (literal
     rule), n * n havoc without overflow */
  if (isInt(x) && 0 <= x && x <= 4294967295) {
    show("n*1000", x * 1000);
    show("n*n", x * x);
  }
} else if (c === 4) {
  /* DynamoDB-number-like d (|d| < 1e126): d * d cannot overflow; the next
     product is of a havoc value (unbounded): one arith-overflow end */
  if (-1e126 < x && x < 1e126) {
    var d2 = x * x;
    show("d*d", d2);
    show("d*d*d", d2 * x);
  }
} else if (c === 5) {
  /* a chain of unbounded products stays linear: one arith-overflow end per
     operation, one continuing path */
  show("chain", x * y * x * y * x);
} else if (c === 6) {
  /* +/- exactness (round 3): integer operands whose exact result has
     magnitude <= 2^53. Two Date.now()-like timestamps t0, t1 (integers in
     [0, 8.64e15], stated with the order builtins, no fork): t1 - t0 and
     (t1 - t0) + 5 are exact (the operands exceed 2^52), t1 + t0 (up to
     1.728e16) is havoc without overflow */
  __servpips_assume(__servpips_fn("and", __servpips_fn("is_int", x), __servpips_fn("<=", 0, x),
                                  __servpips_fn("<=", x, 8640000000000000)));
  __servpips_assume(__servpips_fn("and", __servpips_fn("is_int", y), __servpips_fn("<=", 0, y),
                                  __servpips_fn("<=", y, 8640000000000000)));
  var dt = y - x;
  show("t1-t0", dt);
  show("t1-t0+5", dt + 5);
  show("t1+t0", y + x);
}
