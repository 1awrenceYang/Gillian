/* SERVPIPS perf: the length of a string term that is neither a literal nor a
   concatenation (here Number::toString of a symbolic number, a UF; also
   builtin applications such as decodeURIComponent(...)) stays symbolic
   (s-len); indexing it branches on the length. Without --servpips-aware
   reduction this ended the path with "get_length_of_string: ... impossible". */
// servpips-example: wpst
var n = symb_number();
var s = String(n);
__servpips_debug_vt("len", s.length);
var t = "k" + n;
__servpips_debug_vt("len2", t.length);
__servpips_debug_vt("c0", t[0]);
__servpips_debug_vt("c1", s[1]);
