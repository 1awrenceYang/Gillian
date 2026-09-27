/* SERVPIPS WP2 (E17, RV8): a property with a symbolic name is kept in the
   "sym" list of the object's value tree (the branch where the name is new);
   the other branches are Gillian's usual case split over existing names. */
var k = symb_string();
var o = { a: 1 };
o[k] = 2;
__servpips_debug_vt("rv8", o);
