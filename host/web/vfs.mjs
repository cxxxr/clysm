// A file system in memory for a page without a server: the part of Node.js's
// fs that Clysm uses (host/web/clysm.mjs with { fs }), over the files of a
// pack.  Files written under the directories given as `persist` are kept in
// localStorage, so they are there again when the page is loaded again.
//
// A pack is a header and the contents of its files:
//   "CLYSMPAK" (8 octets), the length of the header (4 octets, little
//   endian), the header (JSON: [{ path, size, mtime, content }...], where
//   `content` is false for a file whose contents are not in the pack — only
//   its name and time are), then the contents one after the other.

const encoder = new TextEncoder();
const latin1 = (octets) => {
  let s = '';
  for (let i = 0; i < octets.length; i += 8192) s += String.fromCharCode.apply(null, octets.subarray(i, i + 8192));
  return s;
};
const fromLatin1 = (string) => Uint8Array.from(string, (c) => c.charCodeAt(0) & 255);

function fsError(code, path) {
  const error = new Error(`${code}: ${path}`);
  error.code = code;
  return error;
}

const parentOf = (path) => {
  const i = path.replace(/\/+$/, '').lastIndexOf('/');
  return i <= 0 ? '/' : path.slice(0, i);
};

export function makeVfs(pack, { persist = [], storageKey = 'clysm-vfs:' } = {}) {
  const files = new Map();        // path -> { data: Uint8Array | null, mtime }
  const dirs = new Set(['/']);
  const normalize = (path) => {
    const parts = [];
    for (const part of String(path).split('/')) {
      if (part === '' || part === '.') continue;
      if (part === '..') parts.pop(); else parts.push(part);
    }
    return '/' + parts.join('/');
  };
  const addDirs = (path) => {
    for (let d = parentOf(path); !dirs.has(d); d = parentOf(d)) dirs.add(d);
  };
  const persistent = (path) => persist.some((d) => path.startsWith(d));
  const save = (path) => {
    if (!persistent(path)) return;
    try {
      const f = files.get(path);
      if (f) localStorage.setItem(storageKey + path, JSON.stringify({ mtime: f.mtime, data: latin1(f.data) }));
      else localStorage.removeItem(storageKey + path);
    } catch (e) { console.warn('the file could not be kept:', path, e); }
  };

  if (pack) {
    const view = new DataView(pack.buffer, pack.byteOffset, pack.byteLength);
    if (latin1(pack.subarray(0, 8)) !== 'CLYSMPAK') throw new Error('not a pack of files');
    const headerLength = view.getUint32(8, true);
    const header = JSON.parse(new TextDecoder().decode(pack.subarray(12, 12 + headerLength)));
    let offset = 12 + headerLength;
    for (const { path, size, mtime, content, directory } of header) {
      if (directory) { dirs.add(path); addDirs(path); continue; }
      files.set(path, { data: content ? pack.subarray(offset, offset + size) : null, mtime });
      if (content) offset += size;
      addDirs(path);
    }
  }
  try {
    for (let i = 0; i < localStorage.length; i++) {
      const key = localStorage.key(i);
      if (!key.startsWith(storageKey)) continue;
      const path = key.slice(storageKey.length);
      const { mtime, data } = JSON.parse(localStorage.getItem(key));
      files.set(path, { data: fromLatin1(data), mtime });
      addDirs(path);
    }
  } catch (e) { /* no localStorage */ }

  const file = (path) => {
    const f = files.get(normalize(path));
    if (!f) throw fsError('ENOENT', path);
    return f;
  };
  const stat = (path) => {
    const p = normalize(path);
    if (dirs.has(p)) return { isDirectory: () => true, isFile: () => false, mtimeMs: 0, size: 0 };
    const f = file(p);
    return { isDirectory: () => false, isFile: () => true, mtimeMs: f.mtime, size: f.data ? f.data.length : 0 };
  };
  const write = (path, data) => {
    const p = normalize(path);
    if (!dirs.has(parentOf(p))) throw fsError('ENOENT', path);
    files.set(p, { data, mtime: Date.now() });
    save(p);
  };

  return {
    existsSync: (path) => { const p = normalize(path); return dirs.has(p) || files.has(p); },
    statSync: stat,
    realpathSync: (path) => { stat(path); return normalize(path); },
    readFileSync(path, encoding) {
      const f = file(path);
      if (!f.data) throw fsError('EACCES (not in the pack)', path);
      if (encoding === 'utf8' || encoding === 'utf-8') return new TextDecoder().decode(f.data);
      if (encoding === 'latin1') return latin1(f.data);
      return f.data;
    },
    writeFileSync(path, data, encoding) {
      write(path, typeof data === 'string'
        ? (encoding === 'latin1' ? fromLatin1(data) : encoder.encode(data))
        : new Uint8Array(data));
    },
    mkdirSync(path, options) {
      const p = normalize(path);
      if (!(options && options.recursive) && !dirs.has(parentOf(p))) throw fsError('ENOENT', path);
      dirs.add(p); addDirs(p);
    },
    readdirSync(path) {
      const p = normalize(path);
      if (!dirs.has(p)) throw fsError('ENOTDIR', path);
      const prefix = p === '/' ? '/' : p + '/';
      const names = new Set();
      for (const name of [...files.keys(), ...dirs]) {
        if (name.startsWith(prefix) && name.length > prefix.length) names.add(name.slice(prefix.length).split('/')[0]);
      }
      return [...names].sort();
    },
    renameSync(from, to) {
      const f = file(from);
      files.delete(normalize(from)); save(normalize(from));
      write(to, f.data || new Uint8Array(0));
    },
    unlinkSync(path) { file(path); files.delete(normalize(path)); save(normalize(path)); },
    rmdirSync(path) {
      const p = normalize(path);
      if (!dirs.has(p)) throw fsError('ENOENT', path);
      dirs.delete(p);
    },
  };
}

// A gzip-compressed pack (from fetch, or a data: URL) as octets.
export async function readPack(response) {
  const stream = response.body.pipeThrough(new DecompressionStream('gzip'));
  return new Uint8Array(await new Response(stream).arrayBuffer());
}
