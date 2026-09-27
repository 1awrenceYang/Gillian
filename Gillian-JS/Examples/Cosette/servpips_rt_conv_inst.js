// servpips-example: wpst
/* SERVPIPS WP3 test (decision D-R2-2): the converted values of a
   primitive-union input p (computed once, symbolically, without forking per
   type) instantiated at concrete values of p agree with Node: for each
   instance, only the "ok" outcome is feasible (expected values by Node 20:
   String(v), Boolean(v), v == 12, v == "12", v == true). */
__servpips_shapes('{"v":2,"shapes":{"P":{"type":"union","of":[{"type":"string"},{"type":"number"},{"type":"boolean"},{"type":"null"}],"optional":true}}}');
var p = __servpips_lazy("p", "P", "input");
var s = "order " + p;
var b = !!p;
var e1 = p == 12;
var e2 = p == "12";
var e3 = p == true;
function chk(tag, ok) { __servpips_emit("note", ok ? "ok" : "BAD", tag, null); }
function inst(tag, S, B, E1, E2, E3) {
  chk(tag + " String", s === "order " + S);
  chk(tag + " Boolean", b === B);
  chk(tag + " ==12", e1 === E1);
  chk(tag + " =='12'", e2 === E2);
  chk(tag + " ==true", e3 === E3);
}
if (p === "12") { inst("'12'", "12", true, true, true, false); }
else if (p === "") { inst("''", "", false, false, false, false); }
else if (p === "1") { inst("'1'", "1", true, false, false, true); }
else if (p === " 12 ") { inst("' 12 '", " 12 ", true, true, false, false); }
else if (p === "abc") { inst("'abc'", "abc", true, false, false, false); }
else if (p === 12) { inst("12", "12", true, true, true, false); }
else if (p === 0) { inst("0", "0", false, false, false, false); }
else if (p === 1) { inst("1", "1", true, false, false, true); }
else if (p === -7) { inst("-7", "-7", true, false, false, false); }
else if (p === true) { inst("true", "true", true, false, false, true); }
else if (p === false) { inst("false", "false", false, false, false, false); }
else if (p === null) { inst("null", "null", false, false, false, false); }
else if (p === undefined) { inst("undefined", "undefined", false, false, false, false); }
