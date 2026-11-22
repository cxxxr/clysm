import assert from 'node:assert/strict';
import { spawnSync } from 'node:child_process';
for (const [mode, expected] of [
  ['eager', /needs a named entry and a function in a lazy library/],
  ['name', /is no name of a module/],
  ['condition', /invalid JS:ENTRYPOINT condition operator/],
  ['imports', /imports need unique module specifiers/],
  ['environment', /environment names must be unique identifiers/],
  ['duplicate', /more than one initializer/],
]) {
  const result = spawnSync(process.execPath, ['tools/gen-library.mjs', process.argv[2]], {
    encoding: 'utf8', env: { ...process.env, CLYSM_ENTRY_INVALID: mode },
  });
  assert.notEqual(result.status, 0, mode);
  assert.match(result.stderr, expected, mode);
}
console.log('lib: invalid entry initializers rejected');
