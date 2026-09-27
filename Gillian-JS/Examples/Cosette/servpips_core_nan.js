/* SERVPIPS WP1 core probe (E7, E8, E16): IEEE comparisons with NaN and
   Infinity, ES2023 StringToNumber and Number::toString give the same results
   in wpst --servpips as in Node (and in exec --servpips). Every check that
   fails throws, which ends the path with an error; the expected output is one
   returned path. */
var u;
if ((u >= 0.7) !== false) throw 1;
if ((NaN >= 0.7) !== false) throw 2;
if ((NaN === NaN) !== false) throw 3;
if (isNaN(u) !== true) throw 4;
if (String(0.1 + 0.2) !== "0.30000000000000004") throw 5;
if ((u < 0.7) !== false) throw 6;
if (String(123) !== "123") throw 7;
if (Number("0x1F") !== 31) throw 8;
if (!isNaN(Number("1_000"))) throw 9;
if (Number("  12  ") !== 12) throw 10;
if (Number("0b11") !== 3) throw 11;
if (Number("") !== 0) throw 12;
if (!isNaN(Number("-0x10"))) throw 13;
if (Number(" -Infinity ") !== -Infinity) throw 14;
if (String(1e21) !== "1e+21") throw 15;
if (String(-1.5e-7) !== "-1.5e-7") throw 16;
if (String(123.456) !== "123.456") throw 17;
if (String(1 / 3) !== "0.3333333333333333") throw 18;
var x = symb_number();
if ((NaN < x) !== false) throw 19;
if (x === NaN) throw 20;
if (!(x < Infinity)) throw 21;
if (-Infinity >= x) throw 22;
