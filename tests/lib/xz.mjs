import assert from 'node:assert/strict';
import { randomBytes } from 'node:crypto';
import { spawnSync } from 'node:child_process';
import { mkdtempSync, copyFileSync, rmSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join, resolve } from 'node:path';
import { decompress } from '../../host/web/xz.mjs';

function pack(bytes) {
  const result = spawnSync('xz', ['--compress', '--stdout', '--check=crc32', '-9'],
    { input: bytes, maxBuffer: bytes.length * 2 + 4096 });
  assert.ifError(result.error);
  assert.equal(result.status, 0, result.stderr.toString());
  return result.stdout;
}

// Both input and output cross the decoder's 64 KiB buffers. Decode several
// streams with the same native instance, including after malformed input.
for (const bytes of [Buffer.alloc(0), Buffer.from('日本語\0'),
                     Buffer.alloc(2_000_000, 42), randomBytes(270_000)]) {
  const compressed = pack(bytes);
  assert.deepEqual(Buffer.from(decompress(compressed)), bytes);
  assert.throws(() => decompress(compressed.subarray(0, -1)));
  assert.throws(() => decompress(Buffer.concat([compressed, Buffer.from([1])])));
  const corrupt = Buffer.from(compressed);
  corrupt[corrupt.length - 12] ^= 1;
  assert.throws(() => decompress(corrupt));
  assert.deepEqual(Buffer.from(decompress(compressed)), bytes);
}
assert.throws(() => decompress(new Uint8Array()));
assert.throws(() => decompress(Buffer.from('not xz')));
console.log('lib: XZ decoder buffers, invalid streams and reuse ok');

const scratch = mkdtempSync(join(tmpdir(), 'clysm-xz-command-'));
try {
  const wasm = join(scratch, 'compressed.wasm');
  copyFileSync('build/lib-test/compressed-xz-eager/compressed.wasm', wasm);
  const result = spawnSync(process.execPath, [resolve('tools/gen-library.mjs'), wasm],
    { env: { ...process.env, PATH: '' }, encoding: 'utf8' });
  assert.equal(result.status, 1);
  assert.match(result.stderr, /XZ library compression requires a working xz command/);
} finally {
  rmSync(scratch, { recursive: true, force: true });
}
console.log('lib: missing XZ encoder diagnostic ok');
