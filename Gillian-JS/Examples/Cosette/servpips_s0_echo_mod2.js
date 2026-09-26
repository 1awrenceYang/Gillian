/* Helper module of servpips_s0_echo_mod.js (not a test by itself). */
exports.twice = function (v) {
  function f(w) {
    return __servpips_echo(w);
  }
  return f(__servpips_echo(v));
};
