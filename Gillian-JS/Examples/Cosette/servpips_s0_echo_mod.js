/* Helper module of servpips_s0_echo.js and servpips_s0_exec.js (not a test by
   itself). */
var inner = require("./servpips_s0_echo_mod2.js");

exports.echo = function (v) {
  return __servpips_echo(v);
};

exports.nested = function (v) {
  return inner.twice(v);
};
