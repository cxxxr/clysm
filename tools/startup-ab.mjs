#!/usr/bin/env node
// The start-up items of tools/bench-startup.sh for several builds or settings,
// interleaved: a whole process each, one after the other for every variant in
// each round, so that the noise of the machine falls on all alike (process
// start-up differs by 10-20% from run to run on a busy machine, more than
// most changes to it).  Prints the least and the median time of each item.
//   tools/bench-slot.sh node tools/startup-ab.mjs ROUNDS ITEMS VARIANT...
// ITEMS: all, or a comma list of start,script,asdf,ql,alex,ppcre,babel
// VARIANT: sbcl, or NAME=ROOT[;ENV=VALUE...][;+NODE-FLAG...]: the tree ROOT
//   (its build/clysm-native.wasm and .img, its host/run.mjs), with the
//   environment and node's flags given, e.g.
//     main=/path/to/main  "new=$PWD"  "slow=$PWD;CLYSM_SLOW_EXIT=1"  "semi=$PWD;+--min-semi-space-size=16"
// The fasls go to BENCH_CACHE (by default build/bench-cache/ of this tree), as
// with tools/bench-startup.sh; each variant is run once first to make them.
import { spawnSync } from 'node:child_process';
import fs from 'node:fs';
import os from 'node:os';
import { fileURLToPath } from 'node:url';

const root = fileURLToPath(new URL('..', import.meta.url));
const rounds = Number(process.argv[2]);
if (!(rounds > 0) || process.argv.length < 5) {
  console.error('usage: node tools/startup-ab.mjs ROUNDS ITEMS VARIANT...');
  process.exit(1);
}
const cache = process.env.BENCH_CACHE || root + 'build/bench-cache';
fs.mkdirSync(cache, { recursive: true });
const QL = (process.env.QUICKLISP_HOME || os.homedir() + '/quicklisp/') + 'setup.lisp';
const script = cache + '/startup-ab-script.lisp';
fs.writeFileSync(script, '(defun fact (n) (if (< n 2) 1 (* n (fact (1- n)))))\n(format t "~D~%" (length (princ-to-string (fact 100))))\n');
const ql = (lib) => [`(load "${QL}")`, `(funcall (intern "QUICKLOAD" "QL") :${lib} :silent t)`];
const ITEMS = { start: ['t'], script: null, asdf: ['(require :asdf)'], ql: [`(load "${QL}")`],
                alex: ql('alexandria'), ppcre: ql('cl-ppcre'), babel: ql('babel') };
const names = process.argv[3] === 'all' ? Object.keys(ITEMS) : process.argv[3].split(',');
const variants = process.argv.slice(4).map((s) => {
  const [head, ...opts] = s.split(';');
  const i = head.indexOf('=');
  const name = i < 0 ? head : head.slice(0, i), dir = i < 0 ? null : head.slice(i + 1).replace(/\/$/, '');
  const env = { ...process.env, LC_ALL: 'C', XDG_CACHE_HOME: cache }, flags = [];
  for (const o of opts) {
    if (o.startsWith('+')) flags.push(o.slice(1));
    else { const j = o.indexOf('='); env[o.slice(0, j)] = o.slice(j + 1); }
  }
  if (dir && !env.CLYSM_NO_HEAP_IMAGE) env.CLYSM_IMAGE ??= dir + '/build/clysm-native.img';
  return { name, dir, env, flags, times: {}, failed: 0 };
});
const command = (v, item) => {
  const forms = ITEMS[item];
  if (!v.dir) {
    return forms ? ['sbcl', ['--noinform', '--no-userinit', '--non-interactive', ...forms.flatMap((f) => ['--eval', f])]]
      : ['sbcl', ['--script', script]];
  }
  const base = [...v.flags, v.dir + '/host/run.mjs', v.dir + '/build/clysm-native.wasm'];
  return ['node', forms ? [...base, '--no-userinit', '--eval', ...forms] : [...base, script]];
};
const run = (v, item) => {
  const [c, a] = command(v, item);
  return spawnSync(c, a, { stdio: ['ignore', 'ignore', 'ignore'], env: v.env });
};
for (const v of variants) for (const n of names) run(v, n);
for (let r = 0; r < rounds; r++) {
  for (const n of names) {
    for (const v of variants) {
      const t0 = process.hrtime.bigint();
      const result = run(v, n);
      (v.times[n] ??= []).push(Number(process.hrtime.bigint() - t0) / 1e6);
      if (result.status !== 0) v.failed++;
    }
  }
}
const stat = (t) => { t.sort((a, b) => a - b); return [t[0], t[t.length >> 1]]; };
const cell = (a, b) => (a + ' / ' + b).padStart(22);
console.log('item (ms)'.padEnd(10) + variants.map((v) => cell(v.name + ' least', 'median')).join(''));
const ratio = variants.map(() => [0, 0]);
for (const n of names) {
  const row = variants.map((v) => stat(v.times[n]));
  row.forEach((s, i) => { ratio[i][0] += Math.log(s[0] / row[0][0]); ratio[i][1] += Math.log(s[1] / row[0][1]); });
  console.log(n.padEnd(10) + row.map((s) => cell(s[0].toFixed(1), s[1].toFixed(1))).join(''));
}
// (the geometric mean of the times over those of the first variant)
console.log('x first'.padEnd(10) + ratio.map((g) => cell(Math.exp(g[0] / names.length).toFixed(3), Math.exp(g[1] / names.length).toFixed(3))).join(''));
for (const v of variants) if (v.failed) console.log(v.name + ': ' + v.failed + ' runs FAILED');
