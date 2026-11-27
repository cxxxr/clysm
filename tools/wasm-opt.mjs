// Binaryen's wasm-opt on a compiled module (bin/clysm library --wasm-opt,
// bin/clysm compile --wasm-opt, bin/clysm.mjs test --wasm-opt), or by hand:
//   node tools/wasm-opt.mjs [--wasm-opt=ARGS] FILE.wasm [-o OUT.wasm]
//   node tools/wasm-opt.mjs --find         (the wasm-opt it would run, or why there is none)
// The module is the whole program (after --shake and --preinit-through),
// so wasm-opt sees every function: it copies small ones into their callers,
// merges the equal ones and drops what nothing reaches (10 % less of a
// shaken library after compression).
//
// Clysm is not changed by it and does not ship Binaryen: wasm-opt is the
// one CLYSM_WASM_OPT_BIN names, else that of the npm package binaryen
// (bin/wasm-opt, run by this Node.js) where it is installed: beside Clysm
// (a devDependency of the project that has Clysm as one) or in the working
// directory's project.  Without one the build fails.
//
// What refers to the functions by their index is made again for the module
// wasm-opt wrote, where the indices are other ones:
// * the names and places of the functions (the custom sections name and
//   clysm.sources, which host/common.mjs maps stack traces with).  Each
//   function is given the name f<index> before, wasm-opt keeps the names
//   (-g), and the two sections are written for the indices those names have
//   after: the names are the compiler's, whatever characters they have and
//   whether or not two functions share one.  A function copied into its
//   callers has no frame of its own in a stack trace (as one the compiler
//   copied), and of functions wasm-opt found equal one is left, with its name.
// * clysm.image-defaults (the globals a heap image fills, of a module that
//   is not shaken) is left out: the image then starts from a heap image as
//   it is, without the variant of it that this section makes.
// * clysm.build (the name of the build, which a heap image is valid for) is
//   made of the new octets: a heap image saved by the module before is not
//   one of the module after.
// A module compiled in an image (clysm.codes) is not a program and is refused.
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { spawnSync } from 'node:child_process';
import { createRequire } from 'node:module';
import { fileURLToPath } from 'node:url';

// The features of WebAssembly the compiler's modules use (src/wasm.lisp),
// named one by one: with all of Binaryen's features enabled it writes what
// engines do not read yet (exact reference types, compact imports), and
// what "all" is changes with its version.
export const features = ['gc', 'reference-types', 'tail-call', 'bulk-memory', 'exception-handling',
  'multivalue', 'simd', 'nontrapping-float-to-int', 'sign-ext', 'mutable-globals'];
// (the smallest module; --wasm-opt=ARGS gives others in their place)
export const defaultArgs = ['-Oz'];

const fail = (message) => { throw new Error(`wasm-opt: ${message}`); };

// What --wasm-opt[=ARGS] (VALUE: true, or ARGS) or the environment's
// CLYSM_WASM_OPT (1, or ARGS) asks for: null (nothing), or wasm-opt's arguments.
export function wasmOptRequest(value, env = process.env) {
  const given = value ?? (env.CLYSM_WASM_OPT || null);
  if (given === null || given === false || given === '0') return null;
  if (given === true || given === '1' || given === '') return defaultArgs;
  return String(given).split(/\s+/).filter((a) => a);
}

// [command, first arguments] of wasm-opt.
export function findWasmOpt(env = process.env) {
  const scriptOrBinary = (file) => {
    // (the npm package's is a script of Node.js: run by this one, as on Windows)
    const head = Buffer.alloc(64);
    const fd = fs.openSync(file, 'r');
    try { fs.readSync(fd, head, 0, 64, 0); } finally { fs.closeSync(fd); }
    return /^#!.*\bnode\b/.test(head.toString('latin1').split('\n')[0]) ? [process.execPath, [file]] : [file, []];
  };
  if (env.CLYSM_WASM_OPT_BIN) {
    const file = path.resolve(env.CLYSM_WASM_OPT_BIN);
    if (!fs.existsSync(file)) fail(`CLYSM_WASM_OPT_BIN names ${file}, which does not exist`);
    return scriptOrBinary(file);
  }
  for (const from of [import.meta.url, path.join(process.cwd(), 'package.json')]) {
    try {
      const require = createRequire(from);
      const manifest = require.resolve('binaryen/package.json');
      const bin = JSON.parse(fs.readFileSync(manifest, 'utf8')).bin?.['wasm-opt'];
      const file = bin && path.join(path.dirname(manifest), bin);
      if (file && fs.existsSync(file)) return scriptOrBinary(file);
    } catch (e) { /* not installed there */ }
  }
  return fail('--wasm-opt needs Binaryen\'s wasm-opt, and there is none: install the npm package binaryen '
    + 'in the project (npm install --save-dev binaryen) or name a wasm-opt with CLYSM_WASM_OPT_BIN');
}

const leb = (n) => { const out = []; do { let b = n & 127; n = Math.floor(n / 128); if (n) b |= 128; out.push(b); } while (n); return out; };
const reader = (bytes, start) => {
  let p = start;
  const u32 = () => { let x = 0, s = 1, c; do { c = bytes[p++]; x += (c & 127) * s; s *= 128; } while (c & 128); return x; };
  return { u32, at: () => p, skip: (n) => { p += n; },
           bytes: (n) => { const b = bytes.subarray(p, p + n); p += n; return b; } };
};
// The sections of a module: { id, name (of a custom one), all (its octets), payload (after its name) }.
function sectionsOf(bytes) {
  if (bytes.length < 8 || bytes.readUInt32LE(0) !== 0x6d736100) fail('not a WebAssembly module');
  const out = [];
  const r = reader(bytes, 8);
  while (r.at() < bytes.length) {
    const start = r.at(), id = bytes[start];
    r.skip(1);
    const size = r.u32(), body = r.at(), end = body + size;
    if (end > bytes.length) fail('a section of the module ends after it');
    let name = null, payload = bytes.subarray(body, end);
    if (id === 0) { const l = r.u32(); name = r.bytes(l).toString('latin1'); payload = bytes.subarray(r.at(), end); }
    out.push({ id, name, all: bytes.subarray(start, end), payload });
    r.skip(end - r.at());
  }
  return out;
}
const custom = (name, payload) => {
  const n = Buffer.from(name, 'latin1');
  const body = Buffer.concat([Buffer.from(leb(n.length)), n, payload]);
  return Buffer.concat([Buffer.from([0]), Buffer.from(leb(body.length)), body]);
};
// The names of the functions (subsection 1 of the section name): a Map of index -> octets.
function functionNames(payload) {
  const names = new Map();
  const r = reader(payload, 0);
  while (r.at() < payload.length) {
    const id = payload[r.at()];
    r.skip(1);
    const size = r.u32(), end = r.at() + size;
    if (id === 1) for (let n = r.u32(); n > 0; n--) { const i = r.u32(); names.set(i, r.bytes(r.u32())); }
    r.skip(end - r.at());
  }
  return names;
}
const nameSection = (names) => {
  const out = [...leb(names.size)].map((b) => Buffer.from([b]));
  for (const i of [...names.keys()].sort((a, b) => a - b)) {
    const n = names.get(i);
    out.push(Buffer.from(leb(i)), Buffer.from(leb(n.length)), n);
  }
  const sub = Buffer.concat(out);
  return custom('name', Buffer.concat([Buffer.from([1]), Buffer.from(leb(sub.length)), sub]));
};
// clysm.sources: the files, then (function index, file, line) (src/wasm.lisp).
function sourcesOf(payload) {
  const r = reader(payload, 0);
  const files = [], places = new Map();
  for (let n = r.u32(); n > 0; n--) files.push(r.bytes(r.u32()));
  for (let n = r.u32(); n > 0; n--) { const i = r.u32(), file = r.u32(), line = r.u32(); places.set(i, [file, line]); }
  return { files, places };
}
const sourcesSection = (files, places) => {
  // (the files that still have a function, in their order)
  const used = [...new Set([...places.values()].map(([f]) => f))].sort((a, b) => a - b);
  const out = [Buffer.from(leb(used.length))];
  for (const f of used) out.push(Buffer.from(leb(files[f].length)), files[f]);
  out.push(Buffer.from(leb(places.size)));
  for (const i of [...places.keys()].sort((a, b) => a - b)) {
    const [f, line] = places.get(i);
    out.push(Buffer.from([...leb(i), ...leb(used.indexOf(f)), ...leb(line)]));
  }
  return custom('clysm.sources', Buffer.concat(out));
};
// (as with-build-section of src/driver.lisp names a build by its octets)
const buildSection = (bytes) => {
  let a = 2166136261, b = 3735928559;
  for (let i = 0; i < bytes.length; i++) {
    a = Math.imul(a ^ bytes[i], 16777619) >>> 0;
    b = Math.imul(b ^ bytes[i], 2246822519) >>> 0;
  }
  const hex = (x) => x.toString(16).toUpperCase().padStart(8, '0');
  return custom('clysm.build', Buffer.from(hex(a) + hex(b), 'latin1'));
};

// The module BYTES after wasm-opt ARGS, with the sections above made for it.
// OPTIONS.log is told what was done, in a line.
export function wasmOpt(bytes, args = defaultArgs, options = {}) {
  if (!Buffer.isBuffer(bytes)) bytes = Buffer.from(bytes);
  const [command, first] = options.command ?? findWasmOpt(options.env);
  const sections = sectionsOf(bytes);
  if (sections.some((s) => s.name === 'clysm.codes')) fail('a module compiled in an image is not optimised (only a whole program is)');
  const named = sections.filter((s) => s.name === 'name');
  const placed = sections.filter((s) => s.name === 'clysm.sources');
  const names = named.length ? functionNames(named[0].payload) : new Map();
  const sources = placed.length ? sourcesOf(placed[0].payload) : { files: [], places: new Map() };
  const hadBuild = sections.some((s) => s.name === 'clysm.build');
  // (the functions that have a name or a place: all of them, as the compiler writes the sections)
  const marks = new Map([...names.keys(), ...sources.places.keys()].map((i) => [i, Buffer.from(`f${i}`)]));
  const own = new Set(['name', 'clysm.sources', 'clysm.image-defaults', 'clysm.build']);
  const input = Buffer.concat([bytes.subarray(0, 8), ...sections.filter((s) => !own.has(s.name)).map((s) => s.all),
                               ...(marks.size ? [nameSection(marks)] : [])]);
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'clysm-wasm-opt-'));
  let output;
  try {
    const from = path.join(dir, 'in.wasm'), to = path.join(dir, 'out.wasm');
    fs.writeFileSync(from, input);
    // (--mvp-features: no feature but those named, whatever the version's defaults are;
    // ARGS after them, so that they can name others)
    const all = [...first, '--mvp-features', ...features.map((f) => `--enable-${f}`), ...args,
                 ...(marks.size ? ['-g'] : []), from, '-o', to];
    const r = spawnSync(command, all, { encoding: 'utf8', maxBuffer: 1 << 26 });
    if (r.error) fail(`${command} did not run: ${r.error.message}`);
    if (r.status !== 0 || !fs.existsSync(to)) {
      fail(`${[command, ...all].join(' ')} failed${r.status === null ? ` (${r.signal})` : ` with status ${r.status}`}:\n${(r.stderr || r.stdout || '').trim().split('\n').slice(0, 20).join('\n')}`);
    }
    output = fs.readFileSync(to);
  } finally { fs.rmSync(dir, { recursive: true, force: true }); }
  const after = sectionsOf(output);
  const newNames = new Map(), newPlaces = new Map();
  for (const s of after.filter((x) => x.name === 'name')) {
    for (const [j, mark] of functionNames(s.payload)) {
      // (f<index>; a part of one that wasm-opt made a function of has its name after a $)
      const m = /(?:^|\$)f(\d+)$/.exec(mark.toString('latin1'));
      const i = m ? Number(m[1]) : null;
      if (i !== null && names.has(i)) newNames.set(j, names.get(i));
      else if (i === null && names.size) newNames.set(j, mark);
      if (i !== null && sources.places.has(i)) newPlaces.set(j, sources.places.get(i));
    }
  }
  const body = Buffer.concat([output.subarray(0, 8), ...after.filter((s) => s.name !== 'name').map((s) => s.all),
                              ...(named.length ? [nameSection(newNames)] : []),
                              ...(placed.length ? [sourcesSection(sources.files, newPlaces)] : [])]);
  const result = hadBuild ? Buffer.concat([body, buildSection(body)]) : body;
  try { new WebAssembly.Module(result); } catch (e) { fail(`the module wasm-opt wrote is not valid here: ${e.message}`); }
  options.log?.(`wasm-opt ${args.join(' ')}: ${bytes.length} -> ${result.length} bytes, ${names.size || marks.size} -> ${newNames.size || newPlaces.size} named functions`);
  return result;
}

if (process.argv[1] && path.resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  let file = null, out = null, value = true;
  const argv = process.argv.slice(2);
  for (let i = 0; i < argv.length; i++) {
    const a = argv[i];
    if (a === '-o') out = argv[++i];
    else if (a === '--find') { // (is there a wasm-opt?  Its command, or what to do)
      try { const [command, first] = findWasmOpt(); console.log([command, ...first].join(' ')); process.exit(0); }
      catch (e) { console.error(`clysm: ${e.message}`); process.exit(1); }
    }
    else if (a === '--wasm-opt') value = true;
    else if (a.startsWith('--wasm-opt=')) value = a.slice('--wasm-opt='.length);
    else if (file === null && !a.startsWith('-')) file = a;
    else { console.error(`wasm-opt.mjs: unknown argument ${a}`); process.exit(2); }
  }
  if (!file) { console.error('usage: node tools/wasm-opt.mjs [--wasm-opt=ARGS] FILE.wasm [-o OUT.wasm]'); process.exit(2); }
  try {
    const result = wasmOpt(fs.readFileSync(file), wasmOptRequest(value, {}), { log: (line) => console.log(`; ${line}`) });
    fs.writeFileSync(out ?? file, result);
  } catch (e) { console.error(`clysm: ${e.message}`); process.exit(1); }
}
