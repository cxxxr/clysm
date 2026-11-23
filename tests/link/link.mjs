// tests/link/run.sh: link the heap image IN into one module (host/link.mjs)
// as cxxxr/lem's desktop build does, check that the module compiles, and
// write the image to OUT:   node tests/link/link.mjs IN OUT
import fs from 'node:fs';
import { linkHeapImage } from '../../host/link.mjs';

const [inPath, outPath] = process.argv.slice(2);
const image = linkHeapImage(fs.readFileSync(inPath));
const length = new DataView(image.buffer, image.byteOffset).getUint32(8, true);
const header = JSON.parse(new TextDecoder().decode(image.subarray(12, 12 + length)));
const start = ((12 + length + 7) & ~7) + header.linked.offset;
new WebAssembly.Module(image.subarray(start, start + header.linked.length));
fs.writeFileSync(outPath, image);
