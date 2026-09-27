// servpips-example: wpst
/* SERVPIPS WP3 test: __servpips_fresh (decl), __servpips_assume,
   __servpips_fn (native GIL operations), __servpips_emit (call with a value
   tree, note, outcome, end) and __servpips_is_concrete. */
var body = __servpips_fresh("event.body", "Str", "input");
var n = __servpips_fresh("event.n", "Num", "input", { site: "app.js:3:4:9", k: 1 });
var flag = __servpips_fresh("decision(x)", "Bool", "decision");
__servpips_assume(__servpips_fn("not", __servpips_fn("=", body, "")));
__servpips_assume(__servpips_fn("or", flag, __servpips_fn("=", body, "abc")));
var params = { TableName: "T", Key: { id: body }, "2": n, "10": true, list: [1, "x"], f: function () {} };
__servpips_emit("outcome", "resolved");
if (n > 3) {
  __servpips_emit("call", { site: "app.js:5:6:30", callee: "ddb.get", api: "DynamoDB.GetItem",
                            sdk: "v2-document", k: 1, sent: true }, params);
} else {
  __servpips_emit("note", "small", "n<=3", { v: n });
  __servpips_emit("end", "returned", "done early");
}
var c1 = __servpips_is_concrete(3), c2 = __servpips_is_concrete(n);
__servpips_emit("note", "conc", (c1 ? "t" : "f") + (c2 ? "t" : "f"));
