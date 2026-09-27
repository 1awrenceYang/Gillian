// servpips-example: wpst
/* SERVPIPS WP2 round 3 (aws2 design problems 5 and 6): a JS class table
   registered for a shape id (__servpips_classes) is used by every lazy value
   of that shape created later, at any depth: here a DocumentClient
   attribute value "Doc" is a JSON object, a JSON array, a DynamoDB set (a
   view whose "members" shape DocSet gives the input structure it reads:
   its values) or a Buffer (a view of DocB that its resolver turns into a
   Buffer model object, @sp_kind blob). A member of a JSON-object Doc is a
   Doc again (same four classes). The value tree of the Buffer is a blob
   node (source: the input member b64); of the set, a lazy node whose
   resolver-defined keys are written. A guard mentioning a logical variable
   is refused for a shape table (unsupported). */
__servpips_shapes('{"Doc":{"type":"union","of":[{"type":"string"},{"type":"number"},{"type":"boolean"},{"type":"null"},{"ref":"DocM"},{"ref":"DocL"}]},"DocM":{"type":"object","additional":{"ref":"Doc","optional":true}},"DocL":{"type":"array","items":{"ref":"Doc"}},"DocSet":{"type":"object","props":{"values":{"type":"array","items":{"type":"string"},"minLen":1,"maxLen":2}},"required":["values"],"additional":"absent"},"DocB":{"type":"object","props":{"b64":{"type":"string"}},"required":["b64"],"additional":"absent"},"Item":{"type":"object","additional":{"ref":"Doc","optional":true}}}');
function RSet(o, k) {
  if (k === "wrapperName") { __servpips_define(o, k, "Set"); return; }
  if (k === "values") { __servpips_define(o, k, __servpips_member(o, "values")); return; }
  __servpips_absent(o, k);
}
function RB(o, k) {
  __servpips_define(o, "__sp$src", __servpips_member(o, "b64"));
  __servpips_define(o, "__sp$enc", "base64");
  __servpips_mark(o, "kind", "blob");
  if (k !== "__sp$src" && k !== "__sp$enc") __servpips_absent(o, k);
}
__servpips_classes("Doc", [
  { label: "M", cls: "Object" },
  { label: "L", cls: "Array" },
  { label: "Set", cls: "Object", resolver: RSet, members: "DocSet" },
  { label: "B", cls: "Object", resolver: RB, members: "DocB" }
]);
var c = __servpips_fresh("case", "Num", "input");
var item = __servpips_lazy("response(DynamoDB.DocumentClient.get@app.js:1:1#1).Item", "Item", "response");
var a = item.a;
if (c === 0) {
  if (typeof a === "object" && a !== null) {
    var x = a.x;
    __servpips_debug_vt("a", a);
  }
} else if (c === 1) {
  if (typeof a === "object" && a !== null && a.wrapperName === "Set") {
    __servpips_debug_vt("set", [a.values, a]);
  }
} else if (c === 2) {
  if (typeof a === "object" && a !== null && !Array.isArray(a) && a.wrapperName === undefined && a.__sp$src === undefined) {
    var b = a.b;
    if (typeof b === "object" && b !== null) { Object.prototype.toString.call(b); }
  }
} else if (c === 3) {
  var g = __servpips_fresh("g", "Bool", "input");
  __servpips_classes("Doc", [{ label: "M", cls: "Object", guard: g }]);
}
