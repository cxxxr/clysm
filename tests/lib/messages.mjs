// tests/lib/messages.lisp: the type and message of each unhandled error
import { pathToFileURL } from 'node:url';
import path from 'node:path';

const m = await import(pathToFileURL(path.resolve(process.argv[2])).href);
for (let i = 1; i <= 17; i++) {
  try { m['f' + i](); console.log(i, 'no error'); }
  catch (e) { console.log(i, JSON.stringify(e.lispType), JSON.stringify(e.message)); }
}
