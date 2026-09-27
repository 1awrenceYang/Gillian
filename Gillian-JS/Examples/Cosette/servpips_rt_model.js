// servpips-example: wpst
/* SERVPIPS WP3 test (design E11, D12): model objects (@sp_model). A [[Get]]
   or HasProperty that misses on the whole prototype chain of a model object
   ends the path unsupported, except for then, toJSON, inspect and
   constructor; enumerating a model object is unsupported; ordinary objects
   are unaffected. */
var c = __servpips_fresh("case", "Num", "input");
var proto = { send: function () { return "sent"; } };
__servpips_mark(proto, "model", true);
function Client() {}
Client.prototype = proto;
var client = new Client();
function show(tag, v) { __servpips_emit("note", tag, "", v); }
if (c === 0) {
  show("modelled", [client.send(), client.then === undefined, client.toJSON === undefined]);
} else if (c === 1) {
  show("unmodelled", client.upload);
} else if (c === 2) {
  show("in", "upload" in client);
} else if (c === 3) {
  for (var k in client) { show("key", k); }
} else if (c === 4) {
  var plain = { a: 1 };
  show("plain", [plain.b, "b" in plain, Object.keys(plain).length]);
}
