/* SERVPIPS WP2: a json-shaped value (e.g. JSON.parse(event.body)) forks once
   on its type when first used (Bool / Num / Str / null throws / object) and
   once more into the Object and Array classes when first used as an object;
   reading six members adds no further fork. Expected: 5 vt notes (the null
   path throws a TypeError, reported as an error by Gillian). */
var body = __servpips_lazy("JSON.parse(event.body)", "json", "input");
var item = {
  property_id: body.property_id,
  name: body.name,
  address: body.address,
  city: body.city,
  price: body.price,
  owner: body.owner
};
__servpips_debug_vt("PutItem.Item", item);
