/* SERVPIPS WP2 (LazyJSON, section 3.2): reading six pass-through members of
   an input object does not fork: exactly one path reaches the "call" (one vt
   note), with the six members as child variables (one decl each, parent_aloc
   = the materialised event object). */
__servpips_shapes('{"v":2,"shapes":{"S0":{"type":"object","props":{"body":{"ref":"S1"}},"required":["body"],"additional":{"type":"any","optional":true}},"S1":{"type":"object","additional":{"type":"json","optional":true}}}}');
var event = __servpips_lazy("event", "S0", "input");
var body = event.body;
var item = {
  property_id: body.property_id,
  name: body.name,
  address: body.address,
  city: body.city,
  price: body.price,
  owner: body.owner
};
__servpips_debug_vt("PutItem.Item", item);
