// servpips-example: wpst
/* SERVPIPS WP3 test (V5 probe: indirect model invocations): a model entry
   reached through a builtin (Array higher-order functions, now also with a
   bound model method, Function.prototype.call/apply) or through a user
   wrapper sees another callee in the site register, so __servpips_site
   returns null (the model then ends the path unsupported: unattributed),
   never a wrong site. A bound function (also a bound bound function) whose
   target chain reaches the model gets its own site. Notes: code = model
   name, msg = site or "null". */
function report(name, s) {
  __servpips_emit("note", name, s === null ? "null" : s);
}
function get(p) {
  report("get", __servpips_site(get));
  return p;
}
var ddb = { get: get };
var bound = get.bind(ddb);
var nested = bound.bind(null);
function wrap(v) { return get(v); }

/* 1. map with a bound model method: unattributed */
__servpips_at("app.js:1:1:30", ["k"].map(ddb.get.bind(ddb)));
/* 2. forEach with the model method: unattributed */
__servpips_at("app.js:2:1:30", [1].forEach(get));
/* 3. Function.prototype.call / apply of the model method: unattributed */
__servpips_at("app.js:3:1:30", ddb.get.call(ddb, 3));
__servpips_at("app.js:4:1:30", ddb.get.apply(ddb, [4]));
/* 4. call of a bound model method: unattributed */
__servpips_at("app.js:5:1:30", bound.call(null, 5));
/* 5. bound and bound bound model methods called directly: attributed */
__servpips_at("app.js:6:1:30", bound(6));
__servpips_at("app.js:7:1:30", nested(7));
/* 6. map with a bound bound model method: unattributed */
__servpips_at("app.js:8:1:30", [8].map(nested));
/* 7. a user wrapper: the register names the wrapper: unattributed */
__servpips_at("app.js:9:1:30", wrap(9));
/* 8. a sort comparator that is a bound model method: unattributed */
__servpips_at("app.js:10:1:30", [2, 1].sort(function (a, b) { return get(a) - b; }.bind(null)));
