import assert from 'node:assert/strict';
import {spawnSync} from 'node:child_process';
const wasm = process.argv[2];
for (const [mode, expected] of [
  ['duplicate', /exports own more than once/],
  ['invalid-name', /JS:REEXPORT needs/],
  ['reserved', /JS:REEXPORT needs/],
  ['empty', /JS:REEXPORT needs/],
  ['invalid-from', /JS:REEXPORT needs/],
  ['invalid-entry', /is no name of a module/],
]) {
  const result = spawnSync(process.execPath, ['tools/gen-library.mjs', wasm], {
    encoding: 'utf8', env: {...process.env, CLYSM_REEXPORT_TEST: mode},
  });
  assert.notEqual(result.status, 0, mode);
  assert.match(result.stderr, expected, mode);
}
console.log('lib: invalid re-exports rejected');
