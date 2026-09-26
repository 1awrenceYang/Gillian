/* SERVPIPS S0 skeleton test, concrete execution (`gillian-js exec`): the
   special forms also work under the concrete interpreter.
   Expected: the program runs to completion and returns "ok". */
var m = require("./servpips_s0_echo_mod.js");

var r = m.echo(42) + __servpips_echo(1, 2);
if (r !== 43) {
  throw new Error("servpips_echo returned a wrong value");
}
var nested = m.nested("v");
if (nested !== "v") {
  throw new Error("nested servpips_echo returned a wrong value");
}
("ok");
