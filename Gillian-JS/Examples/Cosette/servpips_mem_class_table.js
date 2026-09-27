/* SERVPIPS WP2: JS-provided class table (as the unmarshall view of design
   section 4.3): one branch per class, the guard is in the path condition,
   the object gets the resolver's metadata and no automatic members (its
   domain is known: u.foo is absent without the WP3 resolver hook); when the
   guards do not cover the path, the first memory access ends the path as
   unsupported. typeof u does not materialise u (mem3: no lazy class is
   callable), so it is not such an access. */
__servpips_shapes('{"AV":{"type":"object","props":{"S":{"type":"string","optional":true},"M":{"type":"object","optional":true,"additional":{"type":"any","optional":true}},"L":{"type":"array","optional":true,"items":{"type":"any"}}},"additional":"absent"}}');
var av = __servpips_lazy("av", "AV", "input");
var M = __servpips_member(av, "M");
var L = __servpips_member(av, "L");
function R(o, k) { return undefined; }
var u = __servpips_lazy("unmarshall(av)", "ddb-out", "skolem", [
  { label: "M", cls: "Object", guard: !(M === undefined), resolver: R, open: true },
  { label: "L", cls: "Array", guard: !(L === undefined), resolver: R }
]);
if (M !== undefined || L !== undefined) {
  if (typeof u === "object" && u !== null) {
    __servpips_debug_vt("u.foo", u.foo, true);
  }
} else {
  if (typeof u === "object" && u !== null) { __servpips_debug_vt("typeof u", u); var z = u.foo; }
}
