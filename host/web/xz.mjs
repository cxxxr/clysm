// Synchronous XZ decoding for generated embedded libraries. The tiny
// external decoder is embedded too: no fetch, eval, worker or JSPI.
import { embeddedDecoder, XzContext } from './vendor/xzwasm.mjs';
import { bytesOfBase64 as decodeBase64 } from './lib.mjs';
export { loadSync } from './lib.mjs';

let decoder;
export function decompress(bytes) {
  decoder ??= new WebAssembly.Instance(new WebAssembly.Module(decodeBase64(embeddedDecoder)));
  const context = new XzContext(decoder);
  const chunks = [];
  let offset = 0, size = 0;
  try {
    for (;;) {
      let atEnd = false;
      if (context.needsMoreInput()) {
        const part = bytes.subarray(offset, offset + context.bufSize);
        context.supplyInput(part);
        offset += part.length;
        // A zero-length input tells xzwasm this is the final chunk. Even
        // an empty, valid stream may need this call after its footer.
        atEnd = part.length === 0;
      }
      const { outChunk, finished } = context.getNextOutput();
      chunks.push(outChunk.slice());
      size += outChunk.length;
      if (finished) {
        if (offset !== bytes.length || !context.needsMoreInput()) throw new Error('Trailing bytes in embedded XZ');
        break;
      }
      if (!outChunk.length && atEnd) {
        throw new Error('Truncated embedded XZ');
      }
      context.resetOutputBuffer();
    }
    const result = new Uint8Array(size);
    let position = 0;
    for (const chunk of chunks) {
      result.set(chunk, position);
      position += chunk.length;
    }
    return result;
  } finally {
    context.dispose();
  }
}

export function bytesOfBase64(text) {
  return decompress(decodeBase64(text));
}

export function moduleOfBase64(text) {
  return new WebAssembly.Module(bytesOfBase64(text));
}
