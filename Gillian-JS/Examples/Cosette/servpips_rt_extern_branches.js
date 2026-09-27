// servpips-example: wpst
/* SERVPIPS integration test (round 2): a servpips extern that returns several
   branches must not let them share the stores of the calling procedures.
   Here servpips_conv forks object / primitive inside i__toString, called by
   String.prototype.concat (JSIL procedure SP_concat, whose accumulator R and
   index idx live in its store). Before the fix every branch continued with
   the same call stack, so the primitive branch's R := R ++ js.tostring(u)
   and idx := idx + 1 were seen by the object branch after its return:
   "".concat("x-").concat(u) gave ("x-" ++ js.tostring(u)) ++ "[object
   Object]" for an object u, and "".concat("a", v, "b") gave ("a" ++
   js.tostring(v) ++ "b") ++ "[object Object]" for an object v. u, v:
   string | number | closed object (no own members: ToPrimitive uses
   Object.prototype.toString). */
__servpips_shapes('{"v":2,"shapes":{"C":{"type":"object","props":{},"additional":"absent","closed":true},"U":{"type":"union","of":[{"type":"string"},{"type":"number"},{"ref":"C"}]}}}');
var u = __servpips_lazy("u", "U", "input");
var v = __servpips_lazy("v", "U", "input");
function show(tag, v) { __servpips_emit("note", tag, "", v); }
show("concat1", "".concat("x-").concat(u));
show("concat2", "".concat("a", v, "b"));
