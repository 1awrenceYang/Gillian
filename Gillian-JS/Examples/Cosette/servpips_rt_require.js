// servpips-example: wpst
/* SERVPIPS WP3 test: the special forms work in required modules
   (assume, fresh, emit, fn in servpips_rt_require_mod.js), at module
   initialisation and inside functions. */
var m = require("./servpips_rt_require_mod.js");
var v = m.make();
if (v === "") { __servpips_emit("note", "empty", ""); } else { __servpips_emit("note", "nonempty", "", v); }
