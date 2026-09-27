/* SERVPIPS WP2 (E15), concrete execution with --servpips: own properties
   are enumerated in ES2020 OrdinaryOwnPropertyKeys order as in Node
   (array-index keys ascending, then the other keys in creation order; a
   deleted and re-added key comes last), like the symbolic heap
   (servpips_mem_order.js). Without --servpips exec keeps the upstream
   (lexicographic) order. The program must run to completion. */
// servpips-example: args=--servpips
function check(what, got, want) {
  if (got !== want) { throw new Error(what + ": " + got + " instead of " + want); }
}
var o = {}; o.b = 1; o.a = 2; o["10"] = 3; o["2"] = 4; o.zz = 5; o.c = 6;
check("keys", Object.keys(o).join(","), "2,10,b,a,zz,c");
var s = ""; for (var k in o) { s = s + k + ","; }
check("for-in", s, "2,10,b,a,zz,c,");
var p = { claimId: "x", status: "y", amount: 1 };
check("literal", Object.keys(p).join(","), "claimId,status,amount");
delete p.claimId; p.claimId = "z";
check("re-added", Object.keys(p).join(","), "status,amount,claimId");
p.status = "w";
check("overwritten", Object.keys(p).join(","), "status,amount,claimId");
var big = {}; big["4294967295"] = 1; big["4294967294"] = 2; big.x = 3; big["01"] = 4; big["1"] = 5;
check("index bound", Object.getOwnPropertyNames(big).join(","), "1,4294967294,4294967295,x,01");
("ok");
