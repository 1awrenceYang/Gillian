/* SERVPIPS WP2: booking-shaped program. The class facts of every
   materialised lazy value (x == #loc, len(...) facts for arrays) are in the
   path condition of each vt note (withPc = true). */
__servpips_shapes('{"v":2,"shapes":{"S0":{"type":"object","props":{"body":{"type":"string","nullable":true}},"additional":{"type":"any","optional":true}},"R1":{"type":"object","props":{"Item":{"type":"object","optional":true,"additional":{"type":"any","optional":true}}},"additional":"absent"},"R2":{"type":"object","props":{"Items":{"type":"array","items":{"type":"object","additional":{"type":"any","optional":true}}}},"additional":"absent"}}}');
var event = __servpips_lazy("event", "S0", "input");
var body = event.body;
var r1 = __servpips_lazy("response(DynamoDB.GetItem@app.js:18:26:46#1)", "R1", "response");
if (!r1.Item) {
  __servpips_debug_vt("404", body, true);
} else {
  var r2 = __servpips_lazy("response(DynamoDB.Scan@app.js:44:20:40#1)", "R2", "response");
  if (r2.Items.length > 0) {
    __servpips_debug_vt("409", r2.Items[0].id, true);
  } else {
    __servpips_debug_vt("PutItem", { body: body, name: r1.Item.name }, true);
  }
}
