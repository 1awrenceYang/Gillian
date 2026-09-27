/* helper module of servpips_rt_require.js */
var init = __servpips_fresh("module.init", "Str", "env");
__servpips_assume(__servpips_fn("=", init, "cfg"));
exports.make = function () {
  var s = __servpips_fresh("module.value", "Str", "input");
  __servpips_assume(__servpips_fn("not", __servpips_fn("=", s, "")));
  __servpips_emit("note", "init", "", init);
  return s;
};
