#!/usr/bin/env node
// Clysm without SBCL: the command of the npm package (tools/build-npm.sh).
// The compiler is the self-hosted one, build/clysmc.wasm, run on Node.js.
//
//   clysm compile [--shake] FILE... [-o OUT.wasm]
//   clysm library [--shake] FILE... -o DIR/NAME.wasm   (NAME.mjs, NAME.d.ts too)
//   clysm run [--shake] FILE... [-- ARGS]
//   clysm exec OUT.wasm [ARGS]
//   clysm test [--watch] [-j N] [--dom] [--system NAME [--asd FILE.asd]] FILE...
//   clysm repl | clysm script FILE | clysm eval FORM
import { spawn, spawnSync } from 'node:child_process';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const runHost = path.join(root, 'host', 'run.mjs');
const node = process.execPath;

function find(...candidates) {
  for (const c of candidates) if (fs.existsSync(c)) return c;
  return null;
}
const compiler = find(path.join(root, 'clysmc.wasm'), path.join(root, 'build', 'clysmc.wasm'));
const replImage = find(path.join(root, 'clysm-native.wasm'), path.join(root, 'build', 'clysm-native.wasm'));

function run(args, options = {}) {
  const r = spawnSync(node, args, { stdio: 'inherit', ...options });
  if (r.error) throw r.error;
  return r.status ?? 1;
}

// SBCL hosts the compiler where it is installed (and the sources are here),
// otherwise the self-hosted compiler runs on Node.js
const sbcl = fs.existsSync(path.join(root, 'tools', 'build.lisp'))
  && spawnSync('sbcl', ['--version'], { stdio: 'ignore' }).status === 0;

function systemFiles(name, asd) {
  if (!sbcl) { console.error('clysm: --system needs SBCL (it runs ASDF to find the files)'); process.exit(1); }
  const r = spawnSync('sbcl', ['--script', path.join(root, 'tools', 'system-files.lisp'), name,
                               ...(asd ? [path.resolve(asd)] : [])], { encoding: 'utf8' });
  if (r.status !== 0) { process.stderr.write(r.stderr || ''); process.exit(1); }
  return r.stdout.split('\n').filter((l) => l);
}

function compileQuiet(files, out, shake) {
  const args = sbcl
    ? ['--script', path.join(root, 'tools', 'build.lisp'), ...(shake ? ['--shake'] : []), out, ...files]
    : [runHost, compiler, ...(shake ? ['--shake'] : []), out, ...files, '--runtime', path.join(root, 'runtime')];
  return new Promise((resolve) => {
    const child = spawn(sbcl ? 'sbcl' : node, args, { stdio: ['ignore', 'pipe', 'pipe'] });
    let log = '';
    child.stdout.on('data', (b) => { log += b; });
    child.stderr.on('data', (b) => { log += b; });
    child.on('close', (code) => resolve({ ok: code === 0 && fs.existsSync(out), log }));
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
      dom = false, system = null, asd = null, shake = false;
  const files = [];
  for (let i = 0; i < args.length; i++) {
    const a = args[i];
    if (a === '--watch') watch = true;
    else if (a === '-j') jobs = Number(args[++i]);
    else if (a === '--dom') dom = true;
    else if (a === '--system') system = args[++i];
    else if (a === '--asd') asd = args[++i];
    else if (a === '--shake') shake = true;
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

function compile(files, out, shake) {
  if (!compiler) {
    console.error('clysm: the compiler (clysmc.wasm) is missing; build it with tools/build-selfhost.sh');
    process.exit(1);
  }
  const args = [runHost, compiler, ...(shake ? ['--shake'] : []), path.resolve(out),
                ...files.map((f) => path.resolve(f)), '--runtime', path.join(root, 'runtime')];
  const status = run(args);
  if (status !== 0) process.exit(status);
}

function parse(args) {
  const files = []; let out = null; let shake = false; let rest = [];
  for (let i = 0; i < args.length; i++) {
    const a = args[i];
    if (a === '-o') out = args[++i];
    else if (a === '--shake') shake = true;
    else if (a === '--') { rest = args.slice(i + 1); break; }
    else files.push(a);
  }
  return { files, out, shake, rest };
}

const [cmd, ...args] = process.argv.slice(2);
switch (cmd) {
  case 'compile': {
    const { files, out, shake } = parse(args);
    compile(files, out || 'a.wasm', shake);
    break;
  }
  case 'library': {
    const { files, out, shake } = parse(args);
    if (!out) { console.error('clysm library: -o DIR/NAME.wasm is needed'); process.exit(1); }
    fs.mkdirSync(path.dirname(path.resolve(out)), { recursive: true });
    compile(files, out, shake);
    process.exit(run([path.join(root, 'tools', 'gen-library.mjs'), path.resolve(out)]));
    break;
  }
  case 'run': {
    const { files, shake, rest } = parse(args);
    const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'clysm-'));
    const out = path.join(dir, 'run.wasm');
    try {
      compile(files, out, shake);
      process.exitCode = run([runHost, out, ...rest]);
    } finally { fs.rmSync(dir, { recursive: true, force: true }); }
    break;
  }
  case 'test':
    await test(args);
    break;
  case 'exec':
    process.exit(run([runHost, ...args]));
    break;
  case 'repl': case 'script': case 'eval': {
    if (!replImage) { console.error('clysm: the REPL image (clysm-native.wasm) is missing'); process.exit(1); }
    process.exit(run([runHost, replImage, ...(cmd === 'eval' ? ['--eval'] : []), ...args]));
    break;
  }
  default:
    console.log(fs.readFileSync(fileURLToPath(import.meta.url), 'utf8').split('\n').slice(1, 11)
      .map((l) => l.replace(/^\/\/ ?/, '')).join('\n'));
    process.exit(cmd ? 1 : 0);
}
