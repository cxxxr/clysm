import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import { pathToFileURL } from 'node:url';
import { spawnSync } from 'node:child_process';
const dir = path.resolve(process.argv[2]);
const mode = process.argv[3];
const url = (name) => pathToFileURL(path.join(dir, name)).href;
const wasm = path.join(dir, 'entrypoints.wasm');
if (!mode) {
  const embedded = fs.readFileSync(path.join(dir, 'entrypoints.library.mjs'), 'utf8').includes('bytesOfBase64');
  for (const next of ['lazy', 'worker', 'cli', 'override', 'late', 'throwing', ...(embedded ? [] : ['failure'])]) {
    const result = spawnSync(process.execPath, [import.meta.filename, dir, next], { encoding: 'utf8' });
    assert.equal(result.status, next === 'failure' ? 1 : 0, `${next}: ${result.stdout}\n${result.stderr}`);
    if (next === 'failure') {
      assert.match(result.stderr, /ENOENT/);
      assert.doesNotMatch(result.stderr, /\n\s+at /);
    } else assert.equal(result.stderr, '', next);
  }
  assert.match(fs.readFileSync(path.join(dir, 'cli.mjs'), 'utf8'), /^#!\/usr\/bin\/env node\n/);
  assert.ok(fs.statSync(path.join(dir, 'cli.mjs')).mode & 0o111);
  assert.match(fs.readFileSync(path.join(dir, 'cli.entry.mjs'), 'utf8'), /process\.env\.CLYSM_ENTRY_VALUE/);
  console.log('lib: generated entry initializers ok');
} else if (mode === 'lazy') {
  delete process.env.CLYSM_ENTRY_FLAG;
  fs.renameSync(wasm, `${wasm}.saved`);
  try {
    const worker = await import(url('worker.mjs'));
    assert.deepEqual(Object.keys(worker), []);
    assert.equal(globalThis.__clysm_entry_worker, undefined);
  } finally { fs.renameSync(`${wasm}.saved`, wasm); }
  const { status } = await import(url('api.mjs'));
  assert.equal(status().starts, 0);
  assert.equal(status().worker.url, url('worker.mjs'));
  assert.equal(status().cli, null);
} else if (mode === 'worker') {
  process.env.CLYSM_ENTRY_FLAG = '1';
  globalThis.__clysm_entry_gate = { ready() {} };
  await import(url('worker.mjs'));
  await import(url('worker.mjs'));
  const { status } = await import(url('api.mjs'));
  assert.equal(status().starts, 1);
  assert.equal(globalThis.__clysm_entry_worker.url, url('worker.mjs'));
  assert.equal(status().worker, globalThis.__clysm_entry_worker);
} else if (mode === 'cli' || mode === 'override') {
  process.env.CLYSM_ENTRY_VALUE = 'the deployment';
  delete process.env.CLYSM_ENTRY_UNSET;
  if (mode === 'override') {
    const { useLibrary } = await import(url('entrypoints.library.mjs'));
    useLibrary(fs.readFileSync(wasm), { modules: { 'node:path': { basename: () => 'overridden' } } });
  }
  const cli = await import(url('cli.mjs'));
  assert.equal(cli.process, 42);
  assert.deepEqual(Object.keys(cli).sort(), ['Error', 'globalThis', 'process']);
  const { status } = await import(url('api.mjs'));
  assert.equal(status().starts, 1);
  assert.equal(globalThis.__clysm_entry_cli.basename, mode === 'override' ? 'overridden' : 'b');
  assert.equal(status().cli.url, url('cli.mjs'));
  assert.equal(status().cli.environment.CLYSM_ENTRY_VALUE, 'the deployment');
  assert.equal(status().cli.environment.CLYSM_ENTRY_UNSET, undefined);
  assert.equal(globalThis.__clysm_entry_cli.platform, process.platform);
} else if (mode === 'late') {
  (await import(url('api.mjs'))).status();
  // The CLI error policy reports the late imports, without replacing the
  // already loaded library's modules. Capture its diagnostic for this case.
  let diagnostic = '';
  const write = process.stderr.write;
  process.stderr.write = (text) => { diagnostic += text; return true; };
  try { await import(url('cli.mjs')); } finally { process.stderr.write = write; }
  assert.match(diagnostic, /entry imports must be registered before/);
  assert.equal(process.exitCode, 1);
  process.exitCode = 0;
} else if (mode === 'throwing') {
  const sentinel = { reason: 'initializer threw' };
  globalThis.__clysm_entry_sentinel = sentinel;
  await assert.rejects(import(url('throwing.mjs')), (error) => error === sentinel);
} else if (mode === 'failure') {
  fs.renameSync(wasm, `${wasm}.saved`);
  try { await import(url('cli.mjs')); } finally { fs.renameSync(`${wasm}.saved`, wasm); }
}
