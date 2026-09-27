// servpips-example: wpst
/* SERVPIPS WP3 test (test262 R4): under --servpips the Cosette pre-parser is
   token-aware. Only calls of the bare identifiers Assert / Assume are
   stringified; the words in comments, strings and longer identifiers
   (AssertionError, AssumeRole), a lone " in a comment or a single-quoted
   string, and method calls o.Assert(1) are left alone (the upstream
   pre-parser raises on the first ones and turns o.Assert(1) into
   o.Assert("1")). */
// a comment with Assert(not code) and a lone " quote
/* a block comment: Assume(neither) AssertionError AssumeRole( */
var AssertionError = function (m) { this.m = m; };
var AssumeRole = 'STS.AssumeRole(x)';
var q = '"';
var o = { Assert: function (v) { return typeof v; }, Assume: function (v) { return v + 1; } };
var t1 = o.Assert(1);
var t2 = o . Assume(41);
var t3 = o["Assert"](true);
var s = __servpips_fresh("s", "Num", "input");
Assume(s > 5);
var d = 10 / 2 / (1);
var m = "a\"b(c)";
Assume((s < 100) and (not (s = 7)));
Assert(s > 5);
__servpips_emit("note", "values", "", [t1, t2, t3, q, AssumeRole, new AssertionError("x").m, d, m]);
