// servpips-example: wpst
/* SERVPIPS WP3 test (round 3): with the SERVPIPS semantics every compiled
   function has the ES2015 own property name (9.2.11 SetFunctionName:
   non-writable, non-enumerable, configurable; right after length): the
   declared name, "" for an anonymous function expression (the frontend's
   Babel pass names the inference positions var f = function () {}). The
   expected string is the output of Node 20 on the same code. Built-in
   functions of the ES5 runtime (Array.prototype.map, ...) still have no
   name. */
var R = [];
function decl(a, b) {}
var anon = [function () {}][0];
var named = function inner() {};
var o = { m: function m() {} };
function d(fn) { var p = Object.getOwnPropertyDescriptor(fn, "name"); return p ? [p.value, p.writable, p.enumerable, p.configurable].join("/") : "none"; }
R.push(d(decl), d(anon), d(named), d(o.m), decl.name, typeof decl.name, anon.name === "");
var names = Object.getOwnPropertyNames(function g() { "use strict"; });
R.push(names[0] + "|" + names[1]);
var cnt = 0; for (var k in decl) { cnt++; }
R.push(Object.keys(decl).length, cnt, "name" in decl);
try { (function () { "use strict"; decl.name = "x"; })(); R.push("assigned"); } catch (e) { R.push(e.name); }
Object.defineProperty(decl, "name", { value: "renamed" }); R.push(decl.name);
R.push(delete named.name, named.hasOwnProperty("name"), named.name === Function.prototype.name);
var S = R.join(",");
var EXPECTED = "decl/false/false/true,/false/false/true,inner/false/false/true,m/false/false/true,decl,string,true,length|name,0,0,true,TypeError,renamed,true,false,true";
__servpips_emit("note", S === EXPECTED ? "ok" : "BAD", "", S);
