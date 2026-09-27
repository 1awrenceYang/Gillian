// servpips-example: wpst
/* SERVPIPS WP3 test (design E4 / D8): the call-site register.
   __servpips_at("<site>", CALL) writes (site, callee, seq) after the callee
   and the arguments are evaluated, right before the call; a model entry
   __servpips_site(self) returns the site and consumes the register when the
   registered callee is itself (or a bound function targeting it), and null
   otherwise. Every model invocation below reports what it saw as a note
   (code = model name, msg = site or "null"). */
function report(name, s) {
  __servpips_emit("note", name, s === null ? "null" : s);
}
/* two model functions in the shape of s3.putObject({Key: uuidv4()}) */
function putObject(params) {
  report("putObject", __servpips_site(putObject));
  report("putObject-again", __servpips_site(putObject));
  return params;
}
function uuidv4() {
  report("uuidv4", __servpips_site(uuidv4));
  return "u";
}
function get(p) {
  report("get", __servpips_site(get));
  return p;
}
var s3 = { putObject: putObject };
var ddb = { get: get };
function user(x) { return x; }

/* nested annotated calls: the argument's site does not overwrite the outer one */
__servpips_at("app.js:10:2:40", s3.putObject({ Key: __servpips_at("app.js:10:23:31", uuidv4()) }));
/* an indirect invocation through a builtin (arr.map(ddb.get)) is unattributed */
__servpips_at("app.js:11:2:22", ["k"].map(ddb.get));
/* unannotated call after a consumed register */
get(1);
/* stale register holding another callee (an annotated user call) */
__servpips_at("app.js:13:2:9", user(0));
get(2);
/* bound function whose target is the model */
var bound = get.bind(ddb);
__servpips_at("app.js:15:2:10", bound(3));
/* constructor call */
function Client() { report("Client", __servpips_site(Client)); }
var c = __servpips_at("app.js:17:10:22", new Client());
