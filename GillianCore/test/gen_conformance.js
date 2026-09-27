#!/usr/bin/env node
// SERVPIPS V1b: expected values of the GIL concrete operations, computed by
// Node on synthetic inputs (no analysed program is involved). Run it inside
// the Lambda base image of the target runtime, e.g.
//   docker run --rm --entrypoint node -v $PWD:/w -w /w public.ecr.aws/lambda/nodejs:20 \
//     GillianCore/test/gen_conformance.js > GillianCore/test/servpips_conformance.json
// Environment: N_STR (random strings, default 3000), N_NUM (random doubles,
// default 6000), SEED (default 20260927).
// Doubles are written as 16-hex-digit IEEE-754 bit patterns (exact, incl. -0
// and NaN), strings as JSON strings (UTF-8 in the file).
'use strict';

const N_STR = Number(process.env.N_STR || 3000);
const N_NUM = Number(process.env.N_NUM || 6000);
let seed = Number(process.env.SEED || 20260927) >>> 0;

// xorshift32, deterministic
function rnd() {
  seed ^= seed << 13; seed >>>= 0;
  seed ^= seed >>> 17;
  seed ^= seed << 5; seed >>>= 0;
  return seed / 4294967296;
}
const pick = (a) => a[Math.floor(rnd() * a.length)];
const buf = new DataView(new ArrayBuffer(8));
function bits(x) {
  buf.setFloat64(0, x);
  return buf.getBigUint64(0).toString(16).padStart(16, '0');
}
function fromBits(hex) {
  buf.setBigUint64(0, BigInt('0x' + hex));
  return buf.getFloat64(0);
}

// ---------------------------------------------------------------- strings
const fixedStrings = [
  '1_000', 'inf', 'nan', '0x1p3', '  12  ', '0b11', '0o7', '1e400', 'Infinity', '-Infinity', '', ' ',
  '1.', '.5', '+.5e1', '0x', '12abc', ' 12', '1e', '0X1F', '-0x10', 'Infinityx', '  -Infinity ',
  '0', '-0', '+0', '00012', '1e-400', '4.9e-324', '2.4703282292062328e-324', '1.7976931348623157e308',
  '1.7976931348623159e308', '9007199254740993', '0x20000000000001', '0x20000000000003', '0x1fffffffffffff8',
  '0b' + '1'.repeat(60), '0o' + '7'.repeat(25), '0x' + 'f'.repeat(20), '.', '+', '-', 'e5', '+-1', '1e+',
  ' 12 ', ' -3 ', '﻿7', '　1.5　', ' 2 ', '᠎1', '\v\f9\r\n',
  '1 2', '1..2', '0x1g', '0o8', '0b2', 'NaN', 'Infinity ', ' +Infinity', 'infinity', '12e3', '12E-3', '-.0',
  '0.1', '0.30000000000000004', '123456789012345678901234567890',
];
const alphabet = ['0', '1', '2', '5', '7', '9', '.', 'e', 'E', '+', '-', 'x', 'X', 'o', 'O', 'b', 'B', 'a',
  'f', 'F', ' ', '\t', '\n', ' ', ' ', '﻿', '_', 'I', 'n', 'Infinity', '0x', '0o', '0b', '00'];
const strings = fixedStrings.slice();
for (let i = 0; i < N_STR; i++) {
  const len = 1 + Math.floor(rnd() * 7);
  let s = '';
  for (let j = 0; j < len; j++) s += pick(alphabet);
  strings.push(s);
}
// ToNumber results; numlit(s) <=> !isNaN(Number(s))
const stringToNumber = strings.map((s) => [s, bits(Number(s))]);

// ---------------------------------------------------------------- doubles
const specialNums = [0, -0, 1, -1, 0.1, 0.2, 0.1 + 0.2, 0.5, 1.5, 2.5, -2.5, -0.5, 0.49999999999999994, 1e21,
  1e21 - 65536, 999999999999999900000, 1e-7, 1e-6, 1.5e-7, 123456789, 1 / 3, 2 / 3, 5e-324, 2.2250738585072014e-308,
  1.7976931348623157e308, 9007199254740992, 9007199254740993, 4294967295, 4294967296, 4294967297, -4294967297,
  2147483647, 2147483648, -2147483648, -2147483649, 65535, 65536, 65537, -65537, 1e300, -1e300, 123.456,
  0.000001234, 1.2e-7, 100, 1e20, 1e22, 5e-7, Infinity, -Infinity, NaN];
const nums = specialNums.slice();
for (let i = 0; i < N_NUM; i++) {
  const r = rnd();
  let x;
  if (r < 0.4) {
    // uniform bit patterns
    const hi = Math.floor(rnd() * 4294967296), lo = Math.floor(rnd() * 4294967296);
    x = fromBits(hi.toString(16).padStart(8, '0') + lo.toString(16).padStart(8, '0'));
    if (Number.isNaN(x)) x = rnd();
  } else if (r < 0.7) {
    // short decimals k / 10^m
    x = Math.floor(rnd() * 1e6) / Math.pow(10, Math.floor(rnd() * 12)) * (rnd() < 0.5 ? -1 : 1);
  } else if (r < 0.85) {
    // integers around 2^31, 2^32, 2^53
    const base = pick([2 ** 31, 2 ** 32, 2 ** 53, 2 ** 16, 1e21]);
    x = (base + Math.floor(rnd() * 20) - 10) * (rnd() < 0.5 ? -1 : 1);
  } else {
    x = (rnd() - 0.5) * Math.pow(10, Math.floor(rnd() * 40) - 20);
  }
  nums.push(x);
}
const numberToString = nums.map((x) => [bits(x), String(x)]);
const unary = (f) => nums.map((x) => [bits(x), bits(f(x))]);
const toUint16 = (x) => String.fromCharCode(x).charCodeAt(0);

const pairs = [];
for (let i = 0; i < Math.min(nums.length, 3000); i++) {
  const a = nums[i], b = pick(nums.concat([0, -0, 2, -3, 0.5, Infinity]));
  pairs.push([bits(a), bits(b), bits(a % b)]);
}

process.stdout.write(JSON.stringify({
  generator: 'GillianCore/test/gen_conformance.js',
  node: process.version,
  seed: Number(process.env.SEED || 20260927),
  string_to_number: stringToNumber,
  number_to_string: numberToString,
  to_int32: unary((x) => x | 0),
  to_uint32: unary((x) => x >>> 0),
  to_uint16: unary(toUint16),
  floor: unary(Math.floor),
  ceil: unary(Math.ceil),
  round: unary(Math.round),
  sign: unary(Math.sign),
  abs: unary(Math.abs),
  fmod: pairs,
}) + '\n');
