// servpips-example: wpst
/* SERVPIPS WP3 test (WP4 design problem 1): bound functions
   (Function.prototype.bind) called by the JSIL runtime: Array higher-order
   functions, sort comparators, Function.prototype.call/apply, a bound
   getter, a bound toString in ToPrimitive, and nested binds (also called
   directly). Upstream, the runtime reads the missing @scope of a bound
   function and the path fails. The values (notes) are those of Node 20. */
var s = __servpips_fresh("s", "Str", "input");
function show(tag, v) { __servpips_emit("note", tag, "", v); }
var obj = { k: 10 };
function add(x) { return this.k + x; }
var addK = add.bind(obj);
show("map", [1, 2, 3].map(addK));
var acc = { sum: 0 };
function accum(x) { this.sum = this.sum + x; }
[1, 2, 3].forEach(accum.bind(acc));
show("forEach", acc.sum);
function gt(t, x) { return x > t; }
show("filter", [1, 5, 2, 7].filter(gt.bind(null, 3)));
show("some/every", [[1, 2].some(gt.bind(null, 1)), [2, 3].every(gt.bind(null, 2))]);
function sum(a, b) { return a + b + this.k; }
show("reduce", [[1, 2, 3].reduce(sum.bind(obj), 0), [1, 2, 3].reduceRight(sum.bind(obj))]);
function cmp(a, b) { return this.desc ? b - a : a - b; }
show("sort", [3, 1, 2].sort(cmp.bind({ desc: true })));
show("call/apply", [addK.call({ k: 1 }, 7), addK.apply({ k: 1 }, [8])]);
var twice = addK.bind({ k: 1000 }, 5);
show("nested", [twice(), [0].map(twice)[0], twice.call(null)]);
var o2 = {};
Object.defineProperty(o2, "g", { get: function () { return this.k; }.bind(obj) });
show("getter", o2.g);
var o3 = { toString: function () { return "T" + this.k; }.bind(obj) };
show("toString", "" + o3);
function pre(p, x) { return p + x; }
show("symbolic", [s].map(pre.bind(null, "id:")));
