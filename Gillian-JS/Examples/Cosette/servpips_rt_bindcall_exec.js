// servpips-example: exec
/* SERVPIPS WP3 test: servpips_rt_bindcall.js under concrete execution (exec --servpips); exit code 0 iff every case agrees with Node 20. */
// servpips-example: args=--servpips
var R = [];
function t(name, f) { try { R.push(name + "=" + String(f())); } catch (e) { R.push(name + "!" + (e && e.name)); } }
function add(a, b) { return (this && this.base || 0) + a + b; }
var o = { base: 100 };
t("max.call", function () { return Math.max.call(null, 1, 5, 2); });
t("max.apply", function () { return Math.max.apply(null, [1, 5, 2]); });
t("max.bind", function () { return Math.max.bind(null, 7)(3); });
t("slice.call", function () { return Array.prototype.slice.call({ length: 2, 0: "a", 1: "b" }).join("|"); });
t("push.bind", function () { var a = [1]; var p = [].push.bind(a); p(2, 3); return a.join("|"); });
t("hop.call", function () { return Object.prototype.hasOwnProperty.call(o, "base"); });
t("join.apply", function () { return Array.prototype.join.apply(["x", "y"], ["-"]); });
t("bound.call", function () { return add.bind(o, 1).call({ base: 5 }, 2); });
t("bound.apply", function () { return add.bind(o).apply({ base: 5 }, [1, 2]); });
t("bound.bind", function () { return add.bind(o, 1).bind(null, 2)(); });
t("bound.bind.call", function () { return add.bind(o, 1).bind(null).call(null, 3); });
t("call.call", function () { return Function.prototype.call.call(add, o, 1, 2); });
t("apply.call", function () { return Function.prototype.apply.call(add, o, [1, 2]); });
t("call.bind", function () { var c = Function.prototype.call.bind(add); return c(o, 1, 2); });
t("native.bind.call", function () { return Math.max.bind(null, 1).call(null, 9); });
t("native.bind.apply", function () { return Math.min.bind(null, 4).apply(null, [9, 2]); });
t("bound.new", function () { function P(a, b) { this.s = a + b; } var B = P.bind(null, 1); return new B(2).s; });
t("bound.length", function () { return add.bind(null, 1).length; });
t("native.bind.length", function () { return Math.max.bind(null).length; });
t("forEach.call", function () { var s = 0; Array.prototype.forEach.call({ length: 2, 0: 1, 1: 2 }, function (x) { s += x; }); return s; });
t("map.bound.native", function () { return ["1", "2"].map(Number.bind(null)).join("|"); });
t("apply.arguments", function () { return (function () { return Math.max.apply(null, arguments); })(3, 8, 1); });
t("concat.call", function () { return String.prototype.concat.call("a", "b", 1); });
t("bound.toString", function () { return typeof add.bind(null).toString; });
t("native.bind.new", function () { var B = Math.max.bind(null); return new B(); });
t("bound.bound.new", function () { function P(a, b, c) { this.s = a + b + c; } var B = P.bind(null, 1).bind(null, 2); var x = new B(3); return x.s + ":" + (x instanceof P); });
var S = R.join(",");
var EXPECTED = "max.call=5,max.apply=5,max.bind=7,slice.call=a|b,push.bind=1|2|3,hop.call=true,join.apply=x-y,bound.call=103,bound.apply=103,bound.bind=103,bound.bind.call=104,call.call=103,apply.call=103,call.bind=103,native.bind.call=9,native.bind.apply=2,bound.new=3,bound.length=1,native.bind.length=2,forEach.call=3,map.bound.native=1|2,apply.arguments=8,concat.call=ab1,bound.toString=function,native.bind.new!TypeError,bound.bound.new=6:true";
if (S !== EXPECTED) throw new Error(S);
