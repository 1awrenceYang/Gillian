#!/usr/bin/env node
// SERVPIPS decision D-R2-2: expected values of ToString, ToBoolean and
// IsLooselyEqual (==) of primitive values, computed by Node on synthetic
// inputs (no analysed program is involved). Run it inside the Lambda base
// image of the target runtime, e.g.
//   docker run --rm --entrypoint node -v $PWD:/w -w /w public.ecr.aws/lambda/nodejs:20 \
//     GillianCore/test/gen_conv_conformance.js > GillianCore/test/servpips_conv_conformance.json
// Values are encoded as {"t":"str","v":s} (ASCII), {"t":"num","v":<16 hex
// digits of the IEEE-754 bit pattern>}, {"t":"bool","v":b}, {"t":"null"},
// {"t":"undef"}.
'use strict';

const buf = new DataView(new ArrayBuffer(8));
function bits(x) {
  buf.setFloat64(0, x);
  return buf.getBigUint64(0).toString(16).padStart(16, '0');
}

const strings = ['', 'abc', '0', '5', '12', '007', '-5', '+5', ' 12 ', '\t7\n', '1.5', '.5', '5.', '1e3',
  '0x10', '0b11', '0o7', 'Infinity', '-Infinity', '+Infinity', 'NaN', 'true', 'false', 'null',
  'undefined', ' ', '  \t', '-0', '123456789012345', '1234567890123456', '1 2', '[object Object]',
  '1', '0.0', '4294967296', '1e21'];
const numbers = [0, -0, 1, -1, 5, 12, 7, 1.5, -2.25, 0.1, 0.5, 1e21, 1e20, 123456789, 9007199254740992,
  1e-7, 1e300, 2147483648, 4294967296, 1000, NaN, Infinity, -Infinity];
const values = [];
for (const s of strings) values.push(s);
for (const n of numbers) values.push(n);
values.push(true, false, null, undefined);

function enc(v) {
  if (v === undefined) return { t: 'undef' };
  if (v === null) return { t: 'null' };
  switch (typeof v) {
    case 'string': return { t: 'str', v };
    case 'number': return { t: 'num', v: bits(v) };
    case 'boolean': return { t: 'bool', v };
    default: throw new Error('unexpected value');
  }
}

const out = {
  generator: 'GillianCore/test/gen_conv_conformance.js',
  node: process.version,
  values: values.map(enc),
  tostring: values.map((v) => String(v)),
  toboolean: values.map((v) => Boolean(v)),
  // looseeq[i][j] = values[i] == values[j]
  looseeq: values.map((a) => values.map((b) => a == b)), // eslint-disable-line eqeqeq
};
process.stdout.write(JSON.stringify(out) + '\n');
