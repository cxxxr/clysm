// The synchronous loader for JS:LIBRARY-OPTIONS :EMBED T :COMPRESSION :GZIP.
// Only generated embedded bytes are decoded here; the public useLibrary()
// override still accepts an ordinary Wasm module or its uncompressed bytes.
import { gunzipSync } from './vendor/fflate.mjs';
import { bytesOfBase64 as decodeBase64 } from './lib.mjs';
export { loadSync } from './lib.mjs';

export function bytesOfBase64(text) {
  return gunzipSync(decodeBase64(text));
}

export function moduleOfBase64(text) {
  return new WebAssembly.Module(bytesOfBase64(text));
}
