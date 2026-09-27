/* SERVPIPS WP2: prefetched members of a view (an object of a class with a
   resolver), as the S3 GetObject response of the SDK models: the resolver
   reads the underlying input member with __servpips_member (per the class's
   member structure, here a closed struct), and defines the key with the
   input member itself or a wrapper (Body: a Buffer-like blob whose source
   is the string member). A key of no member (closed struct) is undefined
   and does not exist. The value tree of the view reports the wrapped key
   as written (the program sees the wrapper, not the string), the others
   are the input's own members (also a member object materialised by a
   read). A prefetched member of a view that is not yet materialised is the
   same variable. An array view defines its length non-enumerable. */
__servpips_shapes('{"R":{"type":"object","props":{"Body":{"type":"string"},"ContentType":{"type":"string"},"Metadata":{"type":"object","additional":{"type":"string","optional":true}}},"required":["Body"],"additional":"absent"}}');
function blob(src) {
  var b = {};
  __servpips_define(b, "__sp$src", src);
  __servpips_define(b, "__sp$enc", "utf8");
  __servpips_mark(b, "kind", "blob");
  return b;
}
function R(o, k) {
  var c = __servpips_member(o, k);
  if (k === "Body" && c !== undefined) {
    var b = blob(c);
    __servpips_define(o, k, b);
    return b;
  }
  __servpips_define(o, k, c);
  return c;
}
var classes = [{ label: "response", cls: "Object", resolver: R, open: false }];
var r = __servpips_lazy("response(S3.GetObject@app.js:1:1:9#1)", "R", "response", classes);
var pre = __servpips_member(r, "ContentType");
var body = r.Body;
__servpips_debug_vt("body source", body.__sp$src);
__servpips_debug_vt("prefetched = read", pre === r.ContentType);
__servpips_debug_vt("no member", [r.Nope, "Nope" in r]);
if ("ContentType" in r && r.Metadata !== undefined) {
  var owner = r.Metadata.owner;
  __servpips_debug_vt("read only", r);
  r.Metadata.owner = "me";
  __servpips_debug_vt("response", r, true);
}
function RL(o, k) {
  if (k === "length") { __servpips_define(o, "length", 2); return; }
  if (k === "0" || k === "1") { __servpips_define(o, k, "e" + k); return; }
  __servpips_absent(o, k);
}
var a = __servpips_lazy("list", "array", "skolem", [{ label: "L", cls: "Array", resolver: RL }]);
__servpips_debug_vt("array view", [a.length, a.map(function (x) { return x + "!"; }),
  Object.getOwnPropertyDescriptor(a, "length").enumerable, "2" in a]);
