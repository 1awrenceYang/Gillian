/* SERVPIPS WP2 (phase 2, joint with WP1's encodings): Array.prototype.some
   and map over an input array of symbolic length bounded by its shape's
   contract (maxLen 2, e.g. Labels <= MaxLabels). The runtime bounds its
   loops by ToLength(len) = num_to_int(len(<name>)) (exact in SMT under
   --servpips), so the loops stop at the contract bound: one path per
   length and element outcome (6), and the array built by map has as many
   items as the path condition says (its length is entailed). */
__servpips_shapes('{"R":{"type":"object","props":{"Labels":{"type":"array","items":{"type":"string"},"maxLen":2}},"required":["Labels"],"additional":"absent"}}');
var r = __servpips_lazy("response(Rekognition.DetectLabels@app.js:10:1:20#1)", "R", "response");
var found = r.Labels.some(function (x) { return x === "Cat"; });
var lengths = r.Labels.map(function (x) { return x.length; });
__servpips_debug_vt("found", found);
__servpips_debug_vt("lengths", lengths);
