// servpips-example: wpst
/* SERVPIPS WP3 test (round 3): __servpips_fn("js.isarray", v) is
   Array.isArray: decided from the @class of a location the heap knows,
   false for every non-object (literal or of known type), the defined
   builtin (an atom the converter translates exactly) otherwise. */
var c = __servpips_fresh("case", "Num", "input");
var s = __servpips_fresh("s", "Str", "input");
var j = __servpips_lazy("JSON.parse(event.body)", "json", "input");
function show(tag, v) { __servpips_emit("note", tag, "", v); }
function isa(v) { return __servpips_fn("js.isarray", v); }
if (c === 0) {
  show("heap", [isa([]), isa([1, 2]), isa({}), isa(function () {}), isa(new Date(0)), isa(arguments)]);
} else if (c === 1) {
  show("primitives", [isa("a"), isa(1), isa(true), isa(null), isa(undefined), isa(s)]);
} else if (c === 2) {
  /* a lazy JSON value: the builtin agrees with Array.isArray on every path */
  var a = isa(j), b = Array.isArray(j);
  show("lazy", [a, b]);
}
