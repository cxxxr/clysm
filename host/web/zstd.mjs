// The synchronous loader for JS:LIBRARY-OPTIONS :EMBED T :COMPRESSION :ZSTD.
// A Zstandard decoder in JavaScript (fzstd): it decodes a page's module two
// to three times as fast as the XZ decoder at first use, for about a fifth
// more bytes. Its frames carry no checksum (fzstd does not verify one):
// truncated, trailing or malformed input is rejected, and the module's
// bytes are validated by WebAssembly.Module.
import { decompress as decode } from './vendor/fzstd.mjs';
import { bytesOfBase64 as decodeBase64 } from './lib.mjs';
export { loadSync } from './lib.mjs';

export function decompress(bytes) {
  // (fzstd takes no input as an empty output; a generated library always
  // has a frame)
  if (!bytes.length) throw new Error('Empty embedded Zstandard data');
  return decode(bytes);
}

export function bytesOfBase64(text) {
  return decompress(decodeBase64(text));
}

export function moduleOfBase64(text) {
  return new WebAssembly.Module(bytesOfBase64(text));
}
