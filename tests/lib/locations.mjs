import assert from 'node:assert/strict';
import {copyFileSync, mkdirSync, readFileSync, rmSync} from 'node:fs';
import path from 'node:path';
import {pathToFileURL} from 'node:url';
const dir = path.resolve(process.argv[2]);
const mode = process.argv[3];
const module = path.join(dir, 'locations.wasm');
const first = path.join(dir, 'first/module.wasm');
const second = path.join(dir, 'second/module.wasm');
rmSync(path.dirname(first), {recursive:true, force:true});
rmSync(path.dirname(second), {recursive:true, force:true});
const loader = await import(pathToFileURL(path.join(dir, 'locations.library.mjs')));
const api = await import(pathToFileURL(path.join(dir, 'locations.mjs')));
// Import succeeds before either candidate exists. Explicit bytes can replace both.
if (mode === 'override') {
  loader.useLibrary(readFileSync(module), {location: 'file:///explicit/location.wasm'});
  assert.equal(api.location(), 'file:///explicit/location.wasm');
} else if (mode === 'missing') {
  assert.throws(() => api.value(), e => e.code === 'ENOENT' && e.path === first);
} else {
  mkdirSync(path.dirname(second), {recursive: true});
  copyFileSync(module, second);
  if (mode === 'first') {
    mkdirSync(path.dirname(first), {recursive: true});
    copyFileSync(module, first);
  }
  assert.equal(api.location(), pathToFileURL(mode === 'first' ? first : second).href);
}
if (mode !== 'missing') {
  assert.equal(api.value(), 7);
  assert.equal(api.starts(), 1);
  assert.equal(api.starts(), 1);
}
console.log(`lib: module locations ${mode} ok`);
