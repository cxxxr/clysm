#!/usr/bin/env node
// Clysm without SBCL: the command of the npm package (tools/build-npm.sh).
// The compiler is the self-hosted one, build/clysmc.wasm, run on Node.js.
//
//   clysm compile [OPTION...] FILE... [-o OUT.wasm]
//   clysm library [OPTION...] FILE... -o DIR/NAME.wasm   (NAME.wasm.debug, NAME.mjs, NAME.d.ts too)
//   clysm run [OPTION...] FILE... [-- ARGS]
//     OPTION: --shake --shake-symbols --shake-why REPORT --eval-string --thrown-exits
//             --preinit-through FILE (as bin/clysm compile)
//             --wasm-opt[=ARGS] (the module through Binaryen's wasm-opt, -Oz or ARGS:
//             tools/wasm-opt.mjs; it needs the npm package binaryen, or CLYSM_WASM_OPT_BIN)
//   clysm build [--system NAME]... [--asd FILE.asd]... [--load FILE]... [--asdf] [--shake] [--eval FORM]... [--main FUNCTION [--image]] [-o OUT.wasm] [FILE...]
//                            (an application with its ASDF systems as one module)
//   clysm exec OUT.wasm [ARGS]
//   clysm test [--watch] [-j N] [--dom] [--shake] [--wasm-opt[=ARGS]] [--system NAME [--asd FILE.asd]] FILE...
//   clysm repl | clysm script FILE | clysm eval [--no-userinit] FORM...
//   clysm swank [--port N]   (a SWANK server for SLIME; needs Quicklisp)
import { spawn, spawnSync } from 'node:child_process';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const runHost = path.join(root, 'host', 'run.mjs');
const node = process.execPath;
// as bin/clysm: the young generation of the main thread, where the REPL image runs
const nursery = '--max-semi-space-size=' + (process.env.CLYSM_NURSERY_MB || 256);

function find(...candidates) {
  for (const c of candidates) if (fs.existsSync(c)) return c;
  return null;
}
const compiler = find(path.join(root, 'clysmc.wasm'), path.join(root, 'build', 'clysmc.wasm'));
const replImage = find(path.join(root, 'clysm-native.wasm'), path.join(root, 'build', 'clysm-native.wasm'));
// the REPL image starts from the heap image saved after its toplevel forms
// ran (clysm-native.img, made by tools/build-npm.sh; see "Start-up and
// loading" in docs/INTERNALS.md), unless CLYSM_IMAGE names another,
// CLYSM_SAVE_IMAGE or CLYSM_NO_HEAP_IMAGE is set; a heap image of another
// build of the image is not used (the image then starts as it is)
function replEnv(env = process.env) {
  if (env.CLYSM_IMAGE || env.CLYSM_SAVE_IMAGE || env.CLYSM_NO_HEAP_IMAGE || !replImage) return env;
  const heap = replImage.replace(/\.wasm$/, '.img');
  return fs.existsSync(heap) ? { ...env, CLYSM_IMAGE: heap } : env;
}

function run(args, options = {}) {
  const r = spawnSync(node, args, { stdio: 'inherit', ...options });
  if (r.error) throw r.error;
  return r.status ?? 1;
}

// SBCL hosts the compiler where it is installed (and the sources are here),
// otherwise the self-hosted compiler runs on Node.js
const sbcl = fs.existsSync(path.join(root, 'tools', 'build.lisp'))
  && spawnSync('sbcl', ['--version'], { stdio: 'ignore' }).status === 0;

// the files of an ASDF system, as ASDF finds them (tools/system-files.lisp):
// on SBCL where it hosts the compiler, otherwise in the REPL image, whose
// own ASDF reads the system definitions alike
function systemFiles(name, asd) {
  const script = [path.join(root, 'tools', 'system-files.lisp'), name, ...(asd ? [path.resolve(asd)] : [])];
  if (!sbcl && !replImage) { console.error('clysm: --system needs the REPL image (clysm-native.wasm) or SBCL'); process.exit(1); }
  const r = sbcl
    ? spawnSync('sbcl', ['--script', ...script], { encoding: 'utf8' })
    : spawnSync(node, [nursery, runHost, replImage, ...script], { encoding: 'utf8', env: replEnv() });
  if (r.status !== 0) { process.stderr.write(r.stderr || ''); process.exit(1); }
  return r.stdout.split('\n').filter((l) => l);
}

// The heap of the SBCL that compiles, in megabytes (see bin/clysm): its own default
// (1 GB as it is usually built) is too little for a large program.
const sbclHeap = process.env.CLYSM_SBCL_HEAP_MB || '4096';

function compileQuiet(files, out, shake) {
  const args = sbcl
    ? ['--dynamic-space-size', sbclHeap, '--script', path.join(root, 'tools', 'build.lisp'), ...(shake ? ['--shake'] : []), out, ...files]
    : [runHost, compiler, ...(shake ? ['--shake'] : []), out, ...files, '--runtime', path.join(root, 'runtime')];
  return new Promise((resolve) => {
    const child = spawn(sbcl ? 'sbcl' : node, args, { stdio: ['ignore', 'pipe', 'pipe'], env: compilerEnv() });
    let log = '';
    child.stdout.on('data', (b) => { log += b; });
    child.stderr.on('data', (b) => { log += b; });
    child.on('close', (code) => resolve({ ok: code === 0 && fs.existsSync(out), log }));
  });
}

// the module OUT through wasm-opt (tools/wasm-opt.mjs), where it is
const wasmOptTool = path.join(root, 'tools', 'wasm-opt.mjs');
const wasmOptOption = (a) => a === '--wasm-opt' || a.startsWith('--wasm-opt=');
// (asked before the compiler runs: without a wasm-opt nothing is compiled)
function needWasmOpt(option) {
  if (option && spawnSync(node, [wasmOptTool, '--find'], { stdio: ['ignore', 'ignore', 'inherit'] }).status !== 0) process.exit(1);
}
function optimise(out, option) {
  const status = run([wasmOptTool, option, path.resolve(out)]);
  if (status !== 0) process.exit(status);
}
function optimiseQuiet(out, option) {
  return new Promise((resolve) => {
    const child = spawn(node, [wasmOptTool, option, out], { stdio: ['ignore', 'pipe', 'pipe'] });
    let log = '';
    child.stdout.on('data', (b) => { log += b; });
    child.stderr.on('data', (b) => { log += b; });
    child.on('close', (code) => resolve({ ok: code === 0, log }));
  });
}

function runQuiet(wasm, env) {
  return new Promise((resolve) => {
    const child = spawn(node, [runHost, wasm], { stdio: ['ignore', 'pipe', 'pipe'], env: { ...process.env, ...env } });
    let log = '';
    child.stdout.on('data', (b) => { log += b; });
    child.stderr.on('data', (b) => { log += b; });
    child.on('close', (code) => resolve({ code, log }));
  });
}

// each FILE is a test program, compiled as a whole with the files of the
// system before it; it passes when it exits with status 0
async function test(args) {
  let watch = false, jobs = Math.max(1, os.availableParallelism ? os.availableParallelism() - 1 : 2),
      dom = false, system = null, asd = null, shake = false, wasmOpt = null;
  const files = [];
  for (let i = 0; i < args.length; i++) {
    const a = args[i];
    if (a === '--watch') watch = true;
    else if (a === '-j') jobs = Number(args[++i]);
    else if (a === '--dom') dom = true;
    else if (a === '--system') system = args[++i];
    else if (a === '--asd') asd = args[++i];
    else if (a === '--shake') shake = true;
    else if (wasmOptOption(a)) { wasmOpt = a; needWasmOpt(a); }
    else files.push(path.resolve(a));
  }
  if (!files.length) { console.error('clysm test: no test files'); process.exit(1); }
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'clysm-test-'));
  const once = async () => {
    const base = system ? systemFiles(system, asd) : [];
    const started = Date.now();
    let failed = 0, next = 0;
    const results = new Array(files.length);
    const worker = async () => {
      while (next < files.length) {
        const i = next++;
        const f = files[i];
        const wasm = path.join(dir, `t${i}.wasm`);
        fs.rmSync(wasm, { force: true });
        const built = await compileQuiet([...base, f], wasm, shake);
        if (!built.ok) { results[i] = { f, ok: false, log: built.log, what: 'compile' }; continue; }
        if (wasmOpt) {
          const optimised = await optimiseQuiet(wasm, wasmOpt);
          if (!optimised.ok) { results[i] = { f, ok: false, log: optimised.log, what: 'wasm-opt' }; continue; }
        }
        const ran = await runQuiet(wasm, dom ? { CLYSM_DOM: '1' } : {});
        results[i] = { f, ok: ran.code === 0, log: ran.log, what: `exit ${ran.code}` };
      }
    };
    await Promise.all(Array.from({ length: Math.min(jobs, files.length) }, worker));
    for (const r of results) {
      const name = path.relative(process.cwd(), r.f);
      if (r.ok) console.log(`ok   ${name}`);
      else { failed++; console.log(`FAIL ${name} (${r.what})\n${r.log.replace(/^/gm, '  ')}`); }
    }
    console.log(`${files.length - failed} passed, ${failed} failed (${((Date.now() - started) / 1000).toFixed(1)} s)`);
    return failed;
  };
  let failed = await once();
  if (!watch) { fs.rmSync(dir, { recursive: true, force: true }); process.exit(failed ? 1 : 0); }
  // run again when a test or a file of the system changes
  const watched = new Set([...files, ...(system ? systemFiles(system, asd) : [])].map((f) => path.dirname(f)));
  let timer = null, running = false, again = false;
  const trigger = () => {
    clearTimeout(timer);
    timer = setTimeout(async () => {
      if (running) { again = true; return; }
      running = true;
      do { again = false; console.log('\n— files changed, running again —'); failed = await once(); } while (again);
      running = false;
    }, 200);
  };
  for (const d of watched) fs.watch(d, (event, name) => { if (name && /\.(lisp|asd)$/.test(name)) trigger(); });
  console.log(`watching ${watched.size} directories`);
}

// The compiler keeps much of what it makes until its end (the program's
// forms, their code): a young generation of 256 MB a semi-space, which pays
// for programs whose data dies young, there only costs memory (2.1 GB for
// the 188 files of a large library, 1.6 GB with 16 MB, and a sixth faster).
// CLYSM_NURSERY_MB still decides it when set.
const compilerEnv = () => ({ ...process.env, CLYSM_NURSERY_MB: process.env.CLYSM_NURSERY_MB || '16' });

function compile(files, out, shake, options = []) {
  if (!compiler) {
    console.error('clysm: the compiler (clysmc.wasm) is missing; build it with tools/build-selfhost.sh');
    process.exit(1);
  }
  const args = [runHost, compiler, ...(shake ? ['--shake'] : []), ...options, path.resolve(out),
                ...files.map((f) => path.resolve(f)), '--runtime', path.join(root, 'runtime')];
  const status = run(args, { env: compilerEnv() });
  if (status !== 0) process.exit(status);
}

// the options of the compiler (as bin/clysm and tools/build.lisp take
// them): --shake, and those that imply it, are SHAKE, the others OPTIONS
function parse(args) {
  const files = []; let out = null; let shake = false; let rest = []; const options = []; let wasmOpt = null;
  for (let i = 0; i < args.length; i++) {
    const a = args[i];
    if (a === '-o') out = args[++i];
    else if (a === '--shake') shake = true;
    else if (a === '--shake-symbols' || a === '--eval-string' || a === '--thrown-exits') options.push(a);
    else if (a === '--shake-why') options.push(a, path.resolve(args[++i]));
    else if (wasmOptOption(a)) wasmOpt = a;
    else if (a === '--preinit-through') {
      if (i + 1 >= args.length) { console.error('--preinit-through needs a source file'); process.exit(1); }
      options.push(a, path.resolve(args[++i]));
    } else if (a === '--') { rest = args.slice(i + 1); break; }
    else files.push(a);
  }
  return { files, out, shake, options, rest, wasmOpt };
}

const [cmd, ...args] = process.argv.slice(2);
switch (cmd) {
  case 'compile': {
    const { files, out, shake, options, wasmOpt } = parse(args);
    needWasmOpt(wasmOpt);
    compile(files, out || 'a.wasm', shake, options);
    if (wasmOpt) optimise(out || 'a.wasm', wasmOpt);
    break;
  }
  case 'library': {
    const { files, out, shake, options, wasmOpt } = parse(args);
    needWasmOpt(wasmOpt);
    if (!out) { console.error('clysm library: -o DIR/NAME.wasm is needed'); process.exit(1); }
    fs.mkdirSync(path.dirname(path.resolve(out)), { recursive: true });
    compile(files, out, shake, options);
    process.exit(run([path.join(root, 'tools', 'gen-library.mjs'), path.resolve(out), ...(wasmOpt ? [wasmOpt] : [])]));
    break;
  }
  case 'run': {
    const { files, shake, options, rest, wasmOpt } = parse(args);
    needWasmOpt(wasmOpt);
    const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'clysm-'));
    const out = path.join(dir, 'run.wasm');
    try {
      compile(files, out, shake, options);
      if (wasmOpt) optimise(out, wasmOpt);
      process.exitCode = run([runHost, out, ...rest]);
    } finally { fs.rmSync(dir, { recursive: true, force: true }); }
    break;
  }
  case 'test':
    await test(args);
    break;
  case 'exec': {
    // (a program built with --image starts from its heap image)
    const heap = (args[0] || '').replace(/\.wasm$/, '.img');
    const env = process.env;
    const use = !env.CLYSM_IMAGE && !env.CLYSM_SAVE_IMAGE && !env.CLYSM_NO_HEAP_IMAGE && heap !== args[0]
      && fs.existsSync(heap) && fs.statSync(heap).mtimeMs >= fs.statSync(args[0]).mtimeMs;
    process.exit(run([nursery, runHost, ...args], use ? { env: { ...env, CLYSM_IMAGE: heap } } : {}));
    break;
  }
  case 'repl': case 'script': case 'eval': {
    if (!replImage) { console.error('clysm: the REPL image (clysm-native.wasm) is missing'); process.exit(1); }
    // (clysm eval [--no-userinit] FORM...)
    const forms = cmd === 'eval' && args[0] === '--no-userinit' ? ['--no-userinit', '--eval', ...args.slice(1)]
      : cmd === 'eval' ? ['--eval', ...args] : args;
    process.exit(run([nursery, runHost, replImage, ...forms], { env: replEnv() }));
    break;
  }
  case 'build': {
    // the REPL image builds the program (src/static.lisp)
    if (!replImage) { console.error('clysm: the REPL image (clysm-native.wasm) is missing'); process.exit(1); }
    const status = run([nursery, runHost, replImage, path.join(root, 'tools', 'static-build.lisp'), ...args],
                       { env: replEnv() });
    // --image: the heap of the program after its toplevel forms ran, next to
    // the module (OUT.img), from which `clysm exec` starts it
    if (status === 0 && args.includes('--image')) {
      const o = args.indexOf('-o');
      const out = path.resolve(o >= 0 ? args[o + 1] : 'a.wasm');
      const heap = out.replace(/\.wasm$/, '.img');
      fs.rmSync(heap, { force: true });
      const env = { ...process.env, CLYSM_SAVE_IMAGE: '1', CLYSM_APP_IMAGE: heap };
      delete env.CLYSM_IMAGE;
      const saved = run([runHost, out], { env, stdio: ['ignore', 'inherit', 'inherit'] });
      if (saved !== 0 || !fs.existsSync(heap)) { console.error('clysm build: the heap image was not saved'); process.exit(1); }
      console.error(`; wrote ${heap} (${fs.statSync(heap).size} bytes)`);
    }
    process.exit(status);
    break;
  }
  case 'swank': {
    if (!replImage) { console.error('clysm: the REPL image (clysm-native.wasm) is missing'); process.exit(1); }
    let port = 4005;
    for (let i = 0; i < args.length; i++) {
      if (args[i] === '--port') port = Number(args[++i]);
      else { console.error(`clysm swank: unknown option ${args[i]}`); process.exit(1); }
    }
    if (!Number.isInteger(port)) { console.error('clysm swank: --port needs a number'); process.exit(1); }
    process.exit(run([nursery, runHost, replImage, path.join(root, 'contrib', 'swank', 'start.lisp')],
                     { env: replEnv({ ...process.env, CLYSM_SWANK_PORT: String(port) }) }));
    break;
  }
  default:
    console.log(fs.readFileSync(fileURLToPath(import.meta.url), 'utf8').split('\n').slice(1, 17)
      .map((l) => l.replace(/^\/\/ ?/, '')).join('\n'));
    process.exit(cmd ? 1 : 0);
}
