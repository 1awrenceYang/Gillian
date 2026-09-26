/* Helper module of servpips_s0_echo.js and servpips_s0_exec.js (not a test by
   itself). */
var inner = require("./servpips_s0_echo_mod2.js");

/* special form at module top level (module initialisation) */
exports.init = __servpips_echo("init");

exports.echo = function (v) {
  return __servpips_echo(v);
};

exports.nested = function (v) {
  return inner.twice(v);
};
