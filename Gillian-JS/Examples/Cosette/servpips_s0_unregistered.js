/* SERVPIPS S0 skeleton test: calling an unregistered servpips extern ends the
   path with Servpips.Path_end {status = "unsupported"; reason = "unregistered
   servpips extern servpips_nope"}; sibling paths continue.
   Expected: `gillian-js wpst --servpips` prints "Success!" (the path with
   x > 0 is ended before the assertion, the other path satisfies it) and the
   SERVPIPS log contains, after the hello line, exactly one `end` event (see
   servpips_s0_unregistered.expected.jsonl) whose pc contains x > 0. */
var m = require("./servpips_s0_echo_mod.js");

var x = symb_number();
var r = m.echo(0);
if (x > 0) {
  r = __servpips_nope(x);
}
Assert(r = 0);
