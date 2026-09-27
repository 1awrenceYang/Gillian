// servpips-example: wpst
/* SERVPIPS WP2 round 3: hidden members. A program write that is invisible
   to the enumerable own properties and to the JSON text of a lazy value
   (the non-enumerable definition of a member the input does not have, as
   the v2 SDK model's $response on a response object) keeps the value
   pristine: __servpips_is_lazy is true (also in mode "json"), the value
   tree has no written or deleted key, the key exists and reads its value,
   Object.keys omits it and getOwnPropertyNames lists it after the input's
   members (a closed struct: one branch per existence and order of its
   members). Visible writes: an enumerable definition; a non-enumerable one
   of a possible input member (open object: the member is created and
   declared); a name read implicitly (toJSON); redefining a hidden member as
   enumerable. Deleting a hidden member keeps the value pristine. The
   queries answer without materialising or forking, also for a primitive
   union; __servpips_lazy_name(v, "pristine") is the name of a pristine
   value only. */
__servpips_shapes('{"R":{"type":"object","props":{"Item":{"type":"object","optional":true,"additional":{"type":"any","optional":true}},"Count":{"type":"number","optional":true}},"additional":"absent"}}');
var c = __servpips_fresh("case", "Num", "input");
var r = __servpips_lazy("response(DynamoDB.GetItem@app.js:1:1#1)", "R", "response");
var meta = { data: "d" };
if (c === 0) {
  Object.defineProperty(r, "$response", { value: meta, enumerable: false, writable: false, configurable: false });
  __servpips_debug_vt("pristine", [__servpips_is_lazy(r), __servpips_is_lazy(r, "json"),
    __servpips_lazy_name(r, "pristine"), "$response" in r, r.$response === meta]);
  __servpips_debug_vt("value tree", r);
  __servpips_debug_vt("keys", [Object.keys(r), Object.getOwnPropertyNames(r)]);
} else if (c === 1) {
  Object.defineProperty(r, "$extra", { value: 1, enumerable: true });
  __servpips_debug_vt("enumerable", [__servpips_is_lazy(r), __servpips_lazy_name(r, "pristine"),
    __servpips_lazy_name(r), __servpips_is_lazy(r, "any")]);
} else if (c === 2) {
  var item = r.Item;
  if (item !== undefined) {
    Object.defineProperty(item, "$response", { value: 1, enumerable: false });
    __servpips_debug_vt("open member", [__servpips_is_lazy(item), __servpips_is_lazy(r)]);
  }
} else if (c === 3) {
  Object.defineProperty(r, "toJSON", { value: function () { return 1; }, enumerable: false });
  __servpips_debug_vt("toJSON", __servpips_is_lazy(r));
} else if (c === 4) {
  Object.defineProperty(r, "$h", { value: 1, enumerable: false, configurable: true });
  var before = __servpips_is_lazy(r);
  delete r.$h;
  __servpips_debug_vt("deleted hidden", [before, __servpips_is_lazy(r), "$h" in r]);
  Object.defineProperty(r, "$h", { value: 2, enumerable: false, configurable: true });
  Object.defineProperty(r, "$h", { enumerable: true });
  __servpips_debug_vt("made enumerable", [__servpips_is_lazy(r), r]);
} else if (c === 5) {
  var u = __servpips_lazy("u", "ddb-out", "skolem");
  __servpips_debug_vt("union", [__servpips_is_lazy(u), __servpips_lazy_name(u, "json"), u]);
}
