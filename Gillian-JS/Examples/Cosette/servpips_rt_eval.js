// servpips-example: wpst
/* SERVPIPS WP3 test (test262 R7): direct eval of a value that is not a
   string returns it unchanged (ES5 15.1.2.1 step 1), also when the value is
   symbolic (an object location, a symbolic number); eval of a literal string
   runs the code; eval of a symbolic string still ends the path (error, as
   before: code that is not known cannot be compiled). */
var c = __servpips_fresh("case", "Num", "input");
var n = __servpips_fresh("n", "Num", "input");
var s = __servpips_fresh("s", "Str", "input");
function show(tag, v) { __servpips_emit("note", tag, "", v); }
if (c === 0) {
  var o = new Number(1);
  var p = {};
  show("non-string", [eval(o) === o, eval(p) === p, eval(n) === n, eval(true), eval(null), eval(undefined), eval("1 + 1")]);
} else if (c === 1) {
  show("symbolic-string", eval(s));
}
