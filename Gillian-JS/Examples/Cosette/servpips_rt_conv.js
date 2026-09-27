// servpips-example: wpst
/* SERVPIPS WP3 test (decision D-R2-2): ToString, ToBoolean and == of a
   value whose JS type is a union of primitive types do not fork per type:
   the JSIL runtime (extern servpips_conv) builds js.tostring / js.toboolean
   / js.looseeq, and forks only primitive / object. p: string | number |
   boolean | null | undefined; u: string | number | closed object (no own
   members: ToPrimitive uses Object.prototype.toString); o: optional
   object (undefined | object: the upstream procedures, unchanged); s: a
   string. Each case is selected by a symbolic case number; values are
   reported as notes. Before D-R2-2, cases 0-5 forked once per type. */
__servpips_shapes('{"v":2,"shapes":{"P":{"type":"union","of":[{"type":"string"},{"type":"number"},{"type":"boolean"},{"type":"null"}],"optional":true},"C":{"type":"object","props":{},"additional":"absent","closed":true},"U":{"type":"union","of":[{"type":"string"},{"type":"number"},{"ref":"C"}]},"O":{"type":"object","props":{},"additional":"absent","closed":true,"optional":true}}}');
var c = __servpips_fresh("case", "Num", "input");
var p = __servpips_lazy("p", "P", "input");
var u = __servpips_lazy("u", "U", "input");
var o = __servpips_lazy("o", "O", "input");
var s = __servpips_fresh("s", "Str", "input");
function show(tag, v) { __servpips_emit("note", tag, "", v); }
if (c === 0) {
  /* string concatenation, String(), "".concat: one path each */
  show("concat", "order " + p);
  show("String", String(p));
  show("concat()", "".concat("id:", p));
} else if (c === 1) {
  /* truthiness: one conversion, two paths at the branch */
  if (p) { show("truthy", p); } else { show("falsy", p); }
} else if (c === 2) {
  /* default value and negation */
  show("or", p || "dflt");
} else if (c === 3) {
  /* == null / != undefined: no fork at all */
  show("==null", p == null);
  show("!=undefined", p != undefined);
} else if (c === 4) {
  /* == with a number / string / boolean: js.looseeq, one path */
  show("==5", p == 5);
  show("=='5'", p == "5");
  show("==true", p == true);
} else if (c === 5) {
  /* === is structural equality already: one path */
  show("===", p === "x");
} else if (c === 6) {
  /* string | number | object: the primitive side is one path; the object
     side calls Object.prototype.toString */
  show("u-concat", "x" + u);
} else if (c === 7) {
  /* ToBoolean of string | number | object: no fork at the conversion */
  if (u) { show("u-truthy", u); } else { show("u-falsy", u); }
} else if (c === 8) {
  /* optional object (undefined | object): upstream, plain type atoms */
  if (!o) { show("o-absent", o); } else { show("o-present", o); }
} else if (c === 9) {
  /* a string: its own ToString, no builtin */
  show("s-concat", "x" + s);
}
