// servpips-example: wpst
/* SERVPIPS WP3 test (round-2 dataset smoke findings): (1) the value-level
   conditional __servpips_fn("ite", c, a, b) of the models (JSON.stringify
   of a boolean, Math.sign / Math.trunc, String.prototype.indexOf) is typed:
   string / number branches give ite.str / ite.num (no fork), a literal
   condition selects a branch, mixed branches fork; before, FuncApp "ite"
   was untypable ("typeOf ite(#b, "true", "false") ... not typable").
   (2) the length (and a character) of a string built by a builtin or by
   num_to_string is str.len of the term; before, Reduction threw
   "get_length_of_string: ... impossible". */
var b = __servpips_fresh("b", "Bool", "input");
var x = __servpips_fresh("x", "Num", "input");
var s = __servpips_fresh("s", "Str", "input");
var c = __servpips_fresh("case", "Num", "input");
var p = __servpips_lazy("p", "json", "input");
function show(tag, v) { __servpips_emit("note", tag, "", v); }
if (c === 0) {
  var j = __servpips_fn("ite", b, "true", "false");
  show("json-bool", j);
  show("json-bool.length", j.length);
  show("typeof", typeof j);
  show("lit", [__servpips_fn("ite", true, 1, "a"), __servpips_fn("ite", false, 1, "a")]);
  show("num", __servpips_fn("ite", b, 1, x));
} else if (c === 1) {
  if (__servpips_fn("ite", b, "true", "false") === "true") { show("T", b); } else { show("F", b); }
} else if (c === 2) {
  /* mixed branch types: one path per side */
  show("mixed", __servpips_fn("ite", b, 1, "a"));
} else if (c === 3) {
  show("uri.length", __servpips_fn("decodeURIComponent", __servpips_fn("str.replace_all", s, "+", " ")).length);
  show("num2str.length", String(x).length);
  show("num2str[0]", String(x)[0]);
  if (typeof p === "number" || typeof p === "string") { show("tostring.length", ("" + p).length); }
}
