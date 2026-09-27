// servpips-example: wpst
/* SERVPIPS WP3 test (design E18, section 4.4): ToNumber of a concrete
   string is the concrete StringToNumber; ToNumber of a symbolic string goes
   through servpips_tonumber, which forks NaN / +Infinity / -Infinity /
   (ToNumberOp s) with the builtins of section 4.5.
   NOTE: the symbolic branches need the builtin table of the engine core
   (str.in_re.numlit, js.tonumber.*). Until it is integrated the symbolic
   case ends "unsupported" (engine failure), which the expected file records;
   regenerate it after the integration (probes RV1, RV2 then show NaN paths). */
var c = __servpips_fresh("case", "Num", "input");
var s = __servpips_fresh("s", "Str", "input");
if (c === 0) {
  __servpips_emit("note", "concrete", "", [Number("12"), Number("abc"), +"-1.5", Number("")]);
} else if (c === 1) {
  /* RV1 */
  var n = Number(s);
  if (n === n) { __servpips_emit("note", "self-eq", "", n); }
  else { __servpips_emit("note", "self-neq", "", n); }
} else if (c === 2) {
  /* RV2 */
  if (isNaN(Number(s))) { __servpips_emit("note", "nan", ""); }
  else { __servpips_emit("note", "num", ""); }
}
