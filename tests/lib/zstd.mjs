import assert from 'node:assert/strict';
import { randomBytes } from 'node:crypto';
import fs from 'node:fs';
import { zstdCompressSync, constants } from 'node:zlib';
import { decompress, bytesOfBase64 } from '../../host/web/zstd.mjs';

const pack = (bytes) => zstdCompressSync(bytes, { params: { [constants.ZSTD_c_compressionLevel]: 19 } });

for (const bytes of [Buffer.alloc(0), Buffer.from('日本語\0'),
                     Buffer.alloc(2_000_000, 42), randomBytes(270_000)]) {
  const compressed = pack(bytes);
  assert.deepEqual(Buffer.from(decompress(compressed)), bytes);
  assert.deepEqual(Buffer.from(bytesOfBase64(compressed.toString('base64'))), bytes);
  assert.throws(() => decompress(compressed.subarray(0, -1)));
  assert.throws(() => decompress(Buffer.concat([compressed, Buffer.from([1])])));
}
assert.throws(() => decompress(new Uint8Array()));
assert.throws(() => decompress(Buffer.from('not zstd')));
console.log('lib: Zstandard decoder, invalid streams ok');

// The generated library copies the decoder and its license, not the others.
for (const mode of ['eager', 'lazy']) {
  const web = `build/lib-test/compressed-zstd-${mode}/clysm/web`;
  for (const f of ['zstd.mjs', 'vendor/fzstd.mjs', 'vendor/fzstd.LICENSE', 'vendor/fzstd.README.md']) {
    assert.ok(fs.existsSync(`${web}/${f}`), f);
  }
  for (const f of ['xz.mjs', 'compressed.mjs', 'vendor/xzwasm.mjs', 'vendor/fflate.mjs']) {
    assert.ok(!fs.existsSync(`${web}/${f}`), f);
  }
}
console.log('lib: Zstandard library files ok');
