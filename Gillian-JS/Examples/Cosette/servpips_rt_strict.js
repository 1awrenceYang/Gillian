// servpips-example: wpst
/* SERVPIPS WP3 test (coordinator decision E10-strict): a rejected [[Put]] or
   [[Delete]] with Throw = true (strict code, or a built-in that throws) throws
   the TypeError exactly; in sloppy code it ends the path unsupported. The
   strictness is the per-function directive prologue (this file is sloppy,
   functions containing "use strict" and the helper module are strict);
   __servpips_strict() reports the compiler's view of it. */
var m = require("./servpips_rt_strict_mod.js");
var c = __servpips_fresh("case", "Num", "input");
function show(tag, v) { __servpips_emit("note", tag, "", v); }
function strictFn() { "use strict"; return [__servpips_strict(), (function () { return __servpips_strict(); })()]; }
function sloppyFn() { return __servpips_strict(); }
var frozen = Object.freeze({ a: 1 });
var nc = {};
Object.defineProperty(nc, "x", { value: 1, configurable: false });
if (c === 0) {
  show("strictness", [__servpips_strict(), sloppyFn(), strictFn(), m.strict, m.inner()]);
} else if (c === 1) {
  /* strict function: put on a frozen object, delete of a non-configurable
     property, put on a primitive base all throw TypeError */
  var r = (function () {
    "use strict";
    var out = [];
    try { frozen.a = 2; out.push("put-returned"); } catch (e) { out.push(e instanceof TypeError); }
    try { delete nc.x; out.push("delete-returned"); } catch (e) { out.push(e instanceof TypeError); }
    try { "abc".foo = 1; out.push("prim-returned"); } catch (e) { out.push(e instanceof TypeError); }
    return out;
  })();
  show("strict-rejections", r);
} else if (c === 2) {
  /* strict module code called from sloppy code */
  show("strict-module", m.put(frozen));
} else if (c === 3) {
  /* sloppy: put on a frozen object ends unsupported */
  try { frozen.a = 2; show("sloppy-put-returned", frozen.a); } catch (e) { show("sloppy-put-threw", 0); }
} else if (c === 4) {
  try { delete nc.x; show("sloppy-delete-returned", nc.x); } catch (e) { show("sloppy-delete-threw", 0); }
} else if (c === 5) {
  try { "abc".foo = 1; show("sloppy-prim-returned", 0); } catch (e) { show("sloppy-prim-threw", 0); }
} else if (c === 6) {
  /* built-ins with Throw = true throw in sloppy code too */
  var fa = Object.freeze([1]);
  var t = [];
  try { fa.push(2); t.push("push-returned"); } catch (e) { t.push(e instanceof TypeError); }
  try { fa.pop(); t.push("pop-returned"); } catch (e) { t.push(e instanceof TypeError); }
  show("builtin-throw", t);
} else if (c === 7) {
  /* array length truncation stops at a non-configurable element (15.4.5.1,
     its internal [[Delete]] with Throw = false is handled by the algorithm):
     the length write itself is rejected -> TypeError in strict code */
  var r7 = (function () {
    "use strict";
    var a = [1, 2, 3];
    Object.defineProperty(a, "1", { value: 9, configurable: false });
    try { a.length = 0; return ["returned", a.length]; } catch (e) { return [e instanceof TypeError, a.length]; }
  })();
  show("strict-length", r7);
} else if (c === 8) {
  /* the same in sloppy code: silent, length stops at 2 */
  var a8 = [1, 2, 3];
  Object.defineProperty(a8, "1", { value: 9, configurable: false });
  a8.length = 0;
  show("sloppy-length", a8.length);
} else if (c === 9) {
  var ok = { a: 1 };
  ok.a = 2; delete ok.a;
  show("ordinary", [ok.a, "a" in ok]);
} else if (c === 10) {
  /* the other rejections of [[CanPut]]: a new property of a non-extensible
     object, an accessor without setter; delete of a non-configurable
     built-in property: TypeError in strict code */
  var ne = Object.preventExtensions({ a: 1 });
  var acc = {};
  Object.defineProperty(acc, "g", { get: function () { return 1; }, configurable: true });
  var r10 = (function () {
    "use strict";
    var out = [];
    try { ne.b = 2; out.push("ne-returned"); } catch (e) { out.push(e instanceof TypeError); }
    try { acc.g = 2; out.push("acc-returned"); } catch (e) { out.push(e instanceof TypeError); }
    try { delete Math.PI; out.push("builtin-delete-returned"); } catch (e) { out.push(e instanceof TypeError); }
    return out;
  })();
  show("strict-other", r10);
} else if (c === 11) {
  var ne11 = Object.preventExtensions({ a: 1 });
  ne11.b = 2;
  show("sloppy-nonextensible-returned", ne11.b);
} else if (c === 12) {
  var acc12 = {};
  Object.defineProperty(acc12, "g", { get: function () { return 1; } });
  acc12.g = 2;
  show("sloppy-accessor-returned", acc12.g);
}
