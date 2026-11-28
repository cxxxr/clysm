// tests/site/samples.mjs — run the samples of the playground (site/) with
// its image, as the page does, on Node.js:
//   tools/build-site.sh && node tests/site/samples.mjs
// Each sample must end without an error, except "Conditions and restarts",
// whose last form shows an error reaching the top level.  The canvas is a
// stand-in that keeps what the program put on it.
import { readFileSync } from 'node:fs';
import { loadClysm } from '../../build/site/clysm.mjs';
import { samples } from '../../site/samples.mjs';

const expected = {
  numbers: { output: /^2026 is two thousand twenty-six, or MMXXVI$/m, values: /^\(21 316227766016837933199 3\/4\)$/ },
  clos: { output: /^total: 24\.57$/m },
  conditions: { output: /^as zero: \(10 20 0 30 0 40\)$/m, error: /^BAD-ENTRY: cannot parse "oops"$/ },
  macros: { output: /^77,031 takes the most steps: 350$/m },
  'loop-format': { values: /^\(:MAX 9 :SUM 31\)$/ },
  closures: { values: /^10000000$/ },
  speed: { output: /^\(fib 32\) = 2178309 in \d+ ms$/m, values: /^200000$/ },
  canvas: { canvas: true },
  javascript: { output: /^sorted 1 3 5 7 9 with \d+ calls of a Lisp function$/m, values: /^400$/ },
  compiler: { output: /^\(bonjour bonjour\)$/m },
};

let image = null;
globalThis.ImageData = class { constructor(data, width, height) { Object.assign(this, { data, width, height }); } };
globalThis.pgCanvas = { getContext: () => ({ putImageData: (i) => { image = i; } }) };

let output = '';
await loadClysm(readFileSync(new URL('../../build/site/clysm.wasm', import.meta.url)), (text) => { output += text; });

let failures = 0;
for (const sample of samples) {
  output = '';
  image = null;
  let r;
  try {
    r = globalThis.pgRun(sample.code);
  } catch (e) {
    r = { values: '', error: `thrown: ${e}` };
  }
  const want = expected[sample.id] || {};
  const problems = [];
  if (!want.error && r.error) problems.push(`error: ${r.error}`);
  if (want.error && !want.error.test(r.error || '')) problems.push(`error ${JSON.stringify(r.error)}`);
  if (want.output && !want.output.test(output)) problems.push(`output ${JSON.stringify(output)}`);
  if (want.values && !want.values.test(r.values)) problems.push(`values ${JSON.stringify(r.values)}`);
  if (want.canvas && !(r.canvas === 1 && image && image.data.length === image.width * image.height * 4))
    problems.push('nothing was drawn');
  if (!expected[sample.id]) problems.push('no expectations in tests/site/samples.mjs');
  if (problems.length) {
    failures++;
    console.log(`FAIL ${sample.id}\n  ${problems.join('\n  ')}`);
  } else {
    console.log(`ok   ${sample.id}`);
  }
}
if (failures) {
  console.log(`${failures} of ${samples.length} samples failed`);
  process.exit(1);
}
console.log(`all ${samples.length} samples ran`);
