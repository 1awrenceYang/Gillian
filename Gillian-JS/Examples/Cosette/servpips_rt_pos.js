// servpips-example: wpst
/* SERVPIPS WP3 test (interface I5): havoc names use the original source
   position given by the frontend. __servpips_pos("<site>", E) compiles E;
   when E is an arithmetic operation (binary + - * / %, compound assignment,
   ++ / --) the havoc constant is named arith(<op>)@<site label>#k after
   "<site>" (decl.site and note.site carry the whole site string); nested
   operations keep their own (wrapped or compiled-file) positions, and the
   form is transparent for any other E. */
var x = __servpips_fresh("x", "Num", "input");
var y = __servpips_fresh("y", "Num", "input");
var c = __servpips_fresh("case", "Num", "input");
function show(tag, v) { __servpips_emit("note", tag, "", v); }
function id(v) { return v; }
__servpips_assume(__servpips_fn("and", __servpips_fn("<", 0, x), __servpips_fn("<", x, 1),
                                __servpips_fn("<", 0, y), __servpips_fn("<", y, 1)));
if (c === 0) {
  show("mul", __servpips_pos("app.js:31:37:69", x * y));
} else if (c === 1) {
  /* nested: the inner product at its own original position, the outer
     sum at the outer one */
  show("nested", __servpips_pos("app.js:40:2:30", __servpips_pos("app.js:40:3:9", x * y) + x));
} else if (c === 2) {
  var z = x;
  __servpips_pos("lib/util.js:7:4:10", z *= y);
  var w = x;
  __servpips_pos("lib/util.js:8:4:12:3", w /= 3);
  show("compound", [z, w]);
} else if (c === 3) {
  /* not an arithmetic operation: transparent; an unwrapped operation keeps
     its compiled-file position */
  show("other", [__servpips_pos("app.js:50:0:5", id(7)), x * 3]);
}
