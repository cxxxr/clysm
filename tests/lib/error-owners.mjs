import assert from 'node:assert/strict';
import path from 'node:path';
import fs from 'node:fs';
import os from 'node:os';
import { pathToFileURL } from 'node:url';
import { loadSync } from '../../host/lib.mjs';
// A separate host copy, as two independently installed packages would use.
const copy = fs.mkdtempSync(path.join(os.tmpdir(), 'clysm-error-host-'));
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
console.log('lib: conditions stay with their originating instance');

} finally { fs.rmSync(copy, { recursive: true, force: true }); }
