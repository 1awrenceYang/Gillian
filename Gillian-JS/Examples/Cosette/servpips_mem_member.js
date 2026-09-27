/* SERVPIPS WP2: prefetched members (__servpips_member) do not materialise
   their parent; the same child variables fill the cells when the parent is
   later used as an object; __servpips_lazy_name gives the names. */
__servpips_shapes('{"AV":{"type":"object","props":{"S":{"type":"string","optional":true},"N":{"type":"string","optional":true}},"additional":"absent"},"IMG":{"type":"object","additional":{"ref":"AV","optional":true}}}');
var img = __servpips_lazy("event.NewImage", "IMG", "input");
var av = __servpips_member(img, "id");
var s = __servpips_member(av, "S");
var bad = __servpips_member(av, "X");
__servpips_debug_vt("prefetch", [av, s, bad]);
__servpips_debug_vt("names", [__servpips_lazy_name(img), __servpips_lazy_name(av), __servpips_lazy_name(s), __servpips_lazy_name(1)]);
var av2 = img.id;
__servpips_debug_vt("same av", av2 === av);
if (av2 !== undefined) {
  __servpips_debug_vt("same S", av2.S === s);
}
