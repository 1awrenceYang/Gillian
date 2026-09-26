/* SERVPIPS S0 skeleton test: the generic special form __servpips_<name>(...)
   compiles to the extern servpips_<name> in the entry file AND in required
   modules (including modules required by modules, and nested functions).
   servpips_echo returns its first argument (undefined if none).
   Expected: `gillian-js wpst` prints "Success!" (every assertion holds), with or
   without --servpips.
   With --servpips the log contains only the hello line
   (servpips_s0_echo.expected.jsonl is empty: expected files list the events
   after the hello line, which holds environment-dependent fields). */
var m = require("./servpips_s0_echo_mod.js");

var x = symb_number();
var s = symb_string();

/* entry file, symbolic arguments */
var a = __servpips_echo(x);
Assert(a = x);

/* required module: at module initialisation and inside a function */
var iv = m.init;
Assert(iv = "init");
var b = m.echo(s);
Assert(b = s);

/* module required by a module, nested function, nested special forms */
var c = m.nested(x);
Assert(c = x);

/* arguments are values (GetValue applied) */
var o = { k: 7 };
var d = __servpips_echo(o.k);
Assert(d = 7);

/* arguments are evaluated left to right */
var trace = "";
function t(v) {
  trace = trace + v;
  return v;
}
var e = __servpips_echo(t("a"), t("b"));
Assert((e = "a") and (trace = "ab"));

/* no argument */
var u = __servpips_echo();
Assert(u = undefined);
