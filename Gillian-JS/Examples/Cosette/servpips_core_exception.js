/* SERVPIPS WP1 core probe (E3): an OCaml exception raised while executing
   one configuration (here: eval of a symbolic string) ends only that
   configuration with end{error}; sibling configurations continue. */
var x = symb_number();
var s = symb_string();
var r = 0;
if (x > 0) {
  r = eval(s);
} else {
  r = 2;
}
