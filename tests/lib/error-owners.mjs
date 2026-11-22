import assert from 'node:assert/strict';
import path from 'node:path';
import fs from 'node:fs';
import os from 'node:os';
import { pathToFileURL } from 'node:url';
import { loadSync } from '../../host/lib.mjs';
// A separate host copy, as two independently installed packages would use.
const copy = fs.mkdtempSync(path.join(os.tmpdir(), 'clysm-error-host-'));
const helpers = globalThis.__clysm_runtime;
// An older host may already own the global helper object. New instances use
// their own bound factory, rather than that global makeFailure implementation.
globalThis.__clysm_runtime = { ...helpers, makeFailure() { throw new Error('old global error factory'); } };
try {
for (const name of ['lib.mjs', 'common.mjs', 'jsffi.mjs', 'process.mjs']) {
  fs.copyFileSync(new URL(`../../host/${name}`, import.meta.url), path.join(copy, name));
}
const { loadSync: otherLoad } = await import(pathToFileURL(path.join(copy, 'lib.mjs')).href);
const file = path.resolve(process.argv[2]);
const a = loadSync(file).exports;
const b = otherLoad(file).exports;
const local = { kind: 'local', message: 'from this instance', same: true };
assert.deepEqual(a.receive(a.fail), local);
assert.deepEqual(b.receive(b.fail), local);
assert.deepEqual(await a.receiveAsync(async () => a.fail()), local);
let original;
try { a.fail(); } catch (e) { original = e; }
const foreign = { kind: 'foreign', message: 'from this instance', thrown: original };
assert.deepEqual(b.receive(() => { throw original; }), foreign);
assert.deepEqual(await b.receiveAsync(async () => { throw original; }), foreign);
assert.throws(() => b.relay(() => { throw original; }), e => e === original);
assert.deepEqual(a.receive(() => b.relay(a.fail)), local);
assert.equal(b.receive(a.fail).kind, 'foreign');
assert.equal(b.receive(a.fail).message, 'from this instance');
assert.equal(b.receive(a.fail).thrown.name, 'LispError');
assert.deepEqual(a.receive(a.fail), local);
// Relaying arbitrary JavaScript values must not inspect proxy descriptors.
const trapError = new Error('descriptor trap');
const proxy = new Proxy(new Error('original proxy'), {
  getOwnPropertyDescriptor() { throw trapError; },
});
assert.throws(() => a.relay(() => { throw proxy; }), e => e === proxy);
assert.equal(b.receive(() => a.relay(() => { throw proxy; })).thrown, proxy);
console.log('lib: conditions stay with their originating instance');

} finally {
  globalThis.__clysm_runtime = helpers;
  fs.rmSync(copy, { recursive: true, force: true });
}
