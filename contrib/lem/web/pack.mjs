// contrib/lem/web/pack.mjs — pack the files that loading Lem reads into
// one gzip-compressed file for the page (the format is in host/web/vfs.mjs):
//   node pack.mjs TRACE.json OUT.pack.gz
// TRACE.json is what host/run.mjs wrote with CLYSM_TRACE_FILES: the files
// read are packed with their contents, the other paths seen with their
// names and times only (ASDF compares the times of sources and fasls).
import fs from 'node:fs';
import zlib from 'node:zlib';

const [traceFile, out] = process.argv.slice(2);
const { read, seen } = JSON.parse(fs.readFileSync(traceFile, 'utf8'));
const readSet = new Set(read);
const header = [];
const contents = [];
for (const path of new Set([...read, ...seen])) {
  if (!path.startsWith('/')) continue;
  let stat;
  try { stat = fs.statSync(path); } catch { continue; }
  if (stat.isDirectory()) { header.push({ path: path.replace(/\/+$/, '') || '/', directory: true }); continue; }
  const content = readSet.has(path);
  header.push({ path, size: content ? stat.size : 0, mtime: stat.mtimeMs, content });
  if (content) contents.push(fs.readFileSync(path));
}
const headerOctets = Buffer.from(JSON.stringify(header));
const length = Buffer.alloc(4);
length.writeUInt32LE(headerOctets.length);
const pack = Buffer.concat([Buffer.from('CLYSMPAK'), length, headerOctets, ...contents]);
fs.writeFileSync(out, zlib.gzipSync(pack, { level: 9 }));
console.log(`${out}: ${header.length} files, ${pack.length} octets, ${fs.statSync(out).size} compressed`);
