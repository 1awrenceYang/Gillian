"use strict";
/* helper module of servpips_rt_strict.js: strict module code */
exports.strict = __servpips_strict();
exports.inner = function () { return __servpips_strict(); };
exports.put = function (o) {
  try { o.a = 5; return "put-returned"; } catch (e) { return "put-threw:" + (e instanceof TypeError); }
};
