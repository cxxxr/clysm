// Host imports shared by the Node.js and browser hosts.
import { makeJsImports } from './jsffi.mjs';

// Where the functions of a module are defined (its clysm.sources section,
// written by the compiler): state.sourceOf(index) is "file:line" or
// undefined, and state.mapStack(stack) rewrites the Wasm frames of a stack
// trace, "at NAME#12 (wasm://wasm/…:wasm-function[INDEX]:0x…)", as
// "at NAME (file:line)".  A library (bin/clysm library) keeps its name and
// clysm.sources sections in a file of their own, NAME.wasm.debug: DEBUG is
// then a function that returns its bytes (or null), called when a stack is
// first mapped, and state.nameFrames(stack) gives the frames without a name,
// "at wasm://wasm/…:wasm-function[INDEX]:0x…", the name of the function.
export function installSourceMap(state, module, debug = null) {
  // (read when first asked: the places of the REPL image's 18000 functions
  // are 3 MB of strings, made in 2 ms, which every start paid for)
  let sources = null, names = null;
  const u32reader = (b, p0) => {
    let p = p0;
    const u32 = () => { let x = 0, s = 0, c; do { c = b[p++]; x |= (c & 127) << s; s += 7; } while (c & 128); return x >>> 0; };
    return { u32, text: (len) => { const t = new TextDecoder().decode(b.subarray(p, p + len)); p += len; return t; },
             skip: (n) => { p += n; }, at: () => p };
  };
  const debugModule = () => {
    if (debug === null) return null;
    try { const bytes = debug(); debug = null; return bytes ? new WebAssembly.Module(bytes) : null; }
    catch (e) { debug = null; return null; }
  };
  const read = () => {
    sources = new Map(); names = new Map();
    let found = false;
    const readSources = (m) => {
      for (const section of WebAssembly.Module.customSections(m, 'clysm.sources')) {
        found = true;
        const r = u32reader(new Uint8Array(section), 0);
        const files = [];
        for (let n = r.u32(); n > 0; n--) files.push(r.text(r.u32()));
        for (let n = r.u32(); n > 0; n--) { const i = r.u32(), f = r.u32(), line = r.u32(); sources.set(i, `${files[f]}:${line}`); }
      }
    };
    try { readSources(module); } catch (e) { /* no section */ }
    if (!found) {
      const m = debugModule();
      if (m) {
        try { readSources(m); } catch (e) { /* none */ }
        try {
          for (const section of WebAssembly.Module.customSections(m, 'name')) {
            const b = new Uint8Array(section), r = u32reader(b, 0);
            while (r.at() < b.length) {
              const id = b[r.at()]; r.skip(1); const size = r.u32(), end = r.at() + size;
              if (id === 1) for (let n = r.u32(); n > 0; n--) { const i = r.u32(); names.set(i, r.text(r.u32())); }
              r.skip(end - r.at());
            }
          }
        } catch (e) { /* none */ }
      }
    }
  };
  state.sourceOf = (index) => { if (!sources) read(); return sources.get(index); };
  state.nameFrames = (stack) => {
    if (!sources) read();
    if (!names.size) return String(stack);
    return String(stack).replace(
      /at (wasm:\/\/[^\s()]*?:wasm-function\[(\d+)\]:0x[0-9a-f]+)$/gm,
      (all, where, index) => { const name = names.get(Number(index)); return name ? `at ${name} (${where})` : all; });
  };
  state.mapStack = (stack) => state.nameFrames(stack).replace(
    /at ([^\n]*?) \(wasm:\/\/[^\n]*?:wasm-function\[(\d+)\]:0x[0-9a-f]+\)/g,
    (all, name, index) => {
      const where = state.sourceOf(Number(index));
      const clean = name.replace(/#\d+((?:\/[a-z,]+)*)$/, '$1');
      return where ? `at ${clean} (${where})` : `at ${clean} ${all.slice(all.indexOf('('))}`;
    });
}

// The number of functions that a module compiled in the image places in the
// image's table of functions (its custom section clysm.codes).
export function moduleCodeCount(mod) {
  const sections = WebAssembly.Module.customSections(mod, 'clysm.codes');
  if (sections.length === 0) return 0;
  const b = new Uint8Array(sections[0]);
  let n = 0, shift = 0, i = 0;
  for (;;) { const c = b[i++]; n |= (c & 0x7f) << shift; if (c < 0x80) break; shift += 7; }
  return n;
}

// Where the octets of modules read from fasls come from: Uint8Array ->
// { path, offset } (set by the hosts' __clysm_read_bytes), so that a heap
// image can refer to them instead of holding them.
export const byteSources = new WeakMap();

// Instantiate a module compiled inside the image (a Uint8Array or a
// WebAssembly.Module) and link it with it.  While state.modules is an array
// (an image being prepared to be saved, runtime/image.lisp), the modules are
// recorded there and their functions go to the image's table of functions
// from its end; so do those of a module of a heap image being loaded (at
// CODEBASE).  Otherwise the table is only read by SAVE-IMAGE, which then
// refuses to run, so a module gets a table of its own: in the image's, every
// module compiled (each toplevel form of COMPILE-FILE and of the REPL) would
// be kept for good, which made compiling ASDF run out of memory on Node.js 22.
export function instantiateLinked(state, bytes, codeBase) {
  const mod = bytes instanceof WebAssembly.Module ? bytes : new WebAssembly.Module(bytes);
  const ex = state.instance.exports;
  const recording = Array.isArray(state.modules);
  const count = moduleCodeCount(mod);
  let codes = ex.codes;
  if (codeBase === undefined) {
    if (recording) {
      codeBase = codes.length;
      codes.grow(count);
    } else {
      codes = new WebAssembly.Table({ element: 'anyfunc', initial: count });
      codeBase = 0;
    }
  }
  // the runtime helpers the image exports for its modules (src/live.lisp)
  if (!state.helperImports) {
    state.helperImports = {};
    for (const k of Object.keys(ex)) if (k.startsWith('h:')) state.helperImports[k] = ex[k];
  }
  const inst = new WebAssembly.Instance(mod, {
    ...state.imports,
    clysm: { ...state.helperImports, mv_count: ex.mv_count, mv_buf: ex.mv_buf, empty_slots: ex.empty_slots, dyn: ex.dyn, nil_symbol: ex.nil_symbol,
             poll_count: ex.poll_count,
             lisp_throw: ex.lisp_throw, memory: ex.memory,
             codes, code_base: codeBase },
  });
  if (recording) state.modules.push({ bytes, codeBase, count, instance: inst, source: byteSources.get(bytes) });
  return inst;
}

// ---------------------------------------------------------------------------
// Compilation hints (V8 --wasm-compilation-hints): the functions that a
// program runs when it starts are compiled while the module is compiled, in
// background threads, instead of when they are first called (where each
// costs tens of microseconds).  Which they are is learnt from a run of the
// program with every function noting that it runs (instrumentWasm: before
// its body, it sets the octet of its number in a table in the linear
// memory), then given to the modules with withCompilationHints.

function readLeb(b, p) {
  let n = 0, shift = 0, c;
  do { c = b[p++]; n += (c & 0x7f) * 2 ** shift; shift += 7; } while (c & 0x80);
  return [n, p];
}
function leb(n) {
  const out = [];
  do { let c = n % 128; n = Math.floor(n / 128); if (n) c |= 0x80; out.push(c); } while (n);
  return out;
}
function sleb(n) {
  const out = [];
  for (;;) {
    const c = n & 0x7f; n >>= 7;
    if ((n === 0 && !(c & 0x40)) || (n === -1 && (c & 0x40))) { out.push(c); return out; }
    out.push(c | 0x80);
  }
}
function wasmSections(bytes) {
  const list = [];
  let p = 8;
  while (p < bytes.length) {
    const start = p, id = bytes[p++];
    let size; [size, p] = readLeb(bytes, p);
    let name = null;
    if (id === 0) { const [l, q] = readLeb(bytes, p); name = new TextDecoder().decode(bytes.subarray(q, q + l)); }
    list.push({ id, name, start, payload: p, end: p + size });
    p += size;
  }
  return list;
}
// the numbers of imported functions and globals
function wasmImports(bytes, sections) {
  const s = sections.find((x) => x.id === 2);
  let funcs = 0, globals = 0;
  if (!s) return { funcs, globals };
  let p = s.payload, n; [n, p] = readLeb(bytes, p);
  const text = (q) => { const [l, r] = readLeb(bytes, q); return [new TextDecoder().decode(bytes.subarray(r, r + l)), r + l]; };
  const heapType = (q) => readLeb(bytes, q)[1];   // (an s33: the same length)
  for (let i = 0; i < n; i++) {
    let module, field; [module, p] = text(p); [field, p] = text(p);
    const kind = bytes[p++];
    if (kind === 0) {
      funcs++; [, p] = readLeb(bytes, p);
    } else if (kind === 1) {
      const t = bytes[p++]; if (t === 0x63 || t === 0x64) p = heapType(p);
      const f = bytes[p++]; [, p] = readLeb(bytes, p); if (f & 1) [, p] = readLeb(bytes, p);
    } else if (kind === 2) {
      const f = bytes[p++]; [, p] = readLeb(bytes, p); if (f & 1) [, p] = readLeb(bytes, p);
    } else if (kind === 3) {
      globals++;
      const t = bytes[p++]; if (t === 0x63 || t === 0x64) p = heapType(p); p++;
    } else if (kind === 4) {
      p++; [, p] = readLeb(bytes, p);
    }
  }
  return { funcs, globals };
}
function concat(parts) {
  const out = new Uint8Array(parts.reduce((n, x) => n + x.length, 0));
  let p = 0;
  for (const x of parts) { out.set(x, p); p += x.length; }
  return out;
}

// The image BYTES as it starts from a heap image (or null if it cannot be
// made): the globals of its clysm.image-defaults section, conses, simple
// vectors and symbols whose fields the heap image writes (runtime/image.lisp),
// are made empty (the initializers that the section gives) instead of with
// their contents.  V8 decodes and evaluates every global's initializer when it
// compiles and instantiates a module, about 10 ns an octet and 20 ns an
// instruction each time; those contents were half of the REPL image's
// global section (compiling and instantiating it 43 -> 25 ms), and of the
// objects they made all but the first cons of each list were dropped by
// the heap image.
export function imageVariant(bytes) {
  const sections = wasmSections(bytes);
  const d = sections.find((x) => x.name === 'clysm.image-defaults');
  const g = sections.find((x) => x.id === 6);
  if (!d || !g) return null;
  const { globals: imported } = wasmImports(bytes, sections);
  let p = d.payload, l, n; [l, p] = readLeb(bytes, p); p += l;
  const inits = [];
  for ([n, p] = readLeb(bytes, p); n > 0; n--) { [l, p] = readLeb(bytes, p); inits.push(bytes.subarray(p, p + l)); p += l; }
  const empty = new Map();
  for ([n, p] = readLeb(bytes, p); n > 0; n--) {
    let index, k; [index, p] = readLeb(bytes, p); [k, p] = readLeb(bytes, p);
    empty.set(index - imported, inits[k]);
  }
  // the end of the constant expression from P (null: an instruction not known here)
  const exprEnd = (p) => {
    const skip = (q) => { while (bytes[q] & 0x80) q++; return q + 1; };
    for (;;) {
      const op = bytes[p++];
      if (op === 0x0b) return p;
      if (op === 0x41 || op === 0x42 || op === 0x23 || op === 0xd2 || op === 0xd0) p = skip(p);
      else if (op === 0x43) p += 4;
      else if (op === 0x44) p += 8;
      else if ((op >= 0x6a && op <= 0x6c) || (op >= 0x7c && op <= 0x7e)) continue;
      else if (op === 0xfb) {
        let sub; [sub, p] = readLeb(bytes, p);
        if (sub === 0 || sub === 1 || sub === 6 || sub === 7) p = skip(p);
        else if (sub === 8) p = skip(skip(p));
        else if (sub !== 0x1a && sub !== 0x1b && sub !== 0x1c) return null;
      } else return null;
    }
  };
  p = g.payload;
  let count; [count, p] = readLeb(bytes, p);
  const parts = [new Uint8Array(leb(count))];
  let from = p;   // (the globals from FROM to P are copied as they are)
  for (let i = 0; i < count; i++) {
    const t = bytes[p++];
    if (t === 0x63 || t === 0x64) while (bytes[p++] & 0x80);
    p++;
    const end = exprEnd(p);
    if (end === null) return null;
    const e = empty.get(i);
    if (e) { parts.push(bytes.subarray(from, p), e); from = end; }
    p = end;
  }
  if (p !== g.end) return null;
  parts.push(bytes.subarray(from, p));
  const payload = concat(parts);
  return concat([bytes.subarray(0, 8), ...sections.filter((s) => s !== d).map((s) =>
    s === g ? concat([new Uint8Array([6, ...leb(payload.length)]), payload]) : bytes.subarray(s.start, s.end))]);
}

// BYTES with each function first setting the octet at TABLE + BASE + its
// index among the defined functions to 1; { bytes, count }
export function instrumentWasm(bytes, base, table) {
  const sections = wasmSections(bytes);
  const code = sections.find((x) => x.id === 10);
  if (!code) return { bytes, count: 0 };
  let p = code.payload, n; [n, p] = readLeb(bytes, p);
  const bodies = [new Uint8Array(leb(n))];
  for (let i = 0; i < n; i++) {
    let size; [size, p] = readLeb(bytes, p);
    const end = p + size;
    let q = p, groups; [groups, q] = readLeb(bytes, q);
    for (let g = 0; g < groups; g++) {
      [, q] = readLeb(bytes, q);
      const t = bytes[q++]; if (t === 0x63 || t === 0x64) [, q] = readLeb(bytes, q);
    }
    // i32.const address; i32.const 1; i32.store8
    const call = new Uint8Array([0x41, ...sleb(table + base + i), 0x41, 1, 0x3A, 0, 0]);
    const body = concat([bytes.subarray(p, q), call, bytes.subarray(q, end)]);
    bodies.push(new Uint8Array(leb(body.length)), body);
    p = end;
  }
  const payload = concat(bodies);
  // (the branch hints are at offsets in the bodies, which have moved)
  const parts = [bytes.subarray(0, 8)];
  for (const s of sections) {
    if (s.name === 'metadata.code.branch_hint') continue;
    parts.push(s === code ? concat([new Uint8Array([10, ...leb(payload.length)]), payload])
                          : bytes.subarray(s.start, s.end));
  }
  return { bytes: concat(parts), count: n };
}

// The indexes among the defined functions of those of BYTES named NAMES
// (in its name section)
export function wasmFunctionsNamed(bytes, names) {
  const sections = wasmSections(bytes);
  const { funcs } = wasmImports(bytes, sections);
  const s = sections.find((x) => x.name === 'name');
  const found = [];
  if (!s) return found;
  let [l, p] = readLeb(bytes, s.payload); p += l;
  while (p < s.end) {
    const id = bytes[p++]; let size; [size, p] = readLeb(bytes, p);
    if (id === 1) {
      let n, q = p; [n, q] = readLeb(bytes, q);
      for (let i = 0; i < n; i++) {
        let index, len; [index, q] = readLeb(bytes, q); [len, q] = readLeb(bytes, q);
        if (names.includes(new TextDecoder().decode(bytes.subarray(q, q + len)))) found.push(index - funcs);
        q += len;
      }
    }
    p += size;
  }
  return found;
}

// BYTES with the bodies of the functions it exports as NAMES replaced by
// `unreachable` (the modules of a heap image: their RUN and SNAP are never called)
export function withoutBodies(bytes, names) {
  const sections = wasmSections(bytes);
  const { funcs } = wasmImports(bytes, sections);
  const exp = sections.find((x) => x.id === 7);
  const code = sections.find((x) => x.id === 10);
  if (!exp || !code) return bytes;
  const drop = new Set();
  let p = exp.payload, n; [n, p] = readLeb(bytes, p);
  for (let i = 0; i < n; i++) {
    let l; [l, p] = readLeb(bytes, p);
    const name = new TextDecoder().decode(bytes.subarray(p, p + l)); p += l;
    const kind = bytes[p++]; let index; [index, p] = readLeb(bytes, p);
    if (kind === 0 && names.includes(name)) drop.add(index - funcs);
  }
  if (drop.size === 0) return bytes;
  p = code.payload; [n, p] = readLeb(bytes, p);
  const parts = [new Uint8Array(leb(n))];
  for (let i = 0; i < n; i++) {
    const start = p;
    let size; [size, p] = readLeb(bytes, p);
    // (no locals, unreachable, end)
    parts.push(drop.has(i) ? new Uint8Array([3, 0, 0x00, 0x0B]) : bytes.subarray(start, p + size));
    p += size;
  }
  const payload = concat(parts);
  // (the branch hints of the bodies dropped would be wrong: all are left out)
  return concat([bytes.subarray(0, 8), ...sections.filter((s) => s.name !== 'metadata.code.branch_hint').map((s) =>
    s === code ? concat([new Uint8Array([10, ...leb(payload.length)]), payload]) : bytes.subarray(s.start, s.end))]);
}

// BYTES with V8's own compilationHints section (the first proposal's
// format, which V8 13 reads with --experimental-wasm-compilation-hints; it
// does not read metadata.code.compilation_priority, below): FUNCTIONS
// (indexes among the defined functions) are compiled by Liftoff in
// background threads while the module is compiled, instead of on the main
// thread when first called.  (The hint of each: eager, the default first
// tier, Liftoff as the "top" tier, which dynamic tiering does not stop from
// optimizing a function that runs long; an eager hint with TurboFan's tier
// compiles them all with it, and one with the default top tier makes an
// asynchronous compilation never end.)
export function withEagerHints(bytes, functions) {
  let sections = wasmSections(bytes);
  const old = sections.find((x) => x.name === 'compilationHints');
  if (old) {
    bytes = concat([bytes.subarray(0, old.start), bytes.subarray(old.end)]);
    sections = wasmSections(bytes);
  }
  const fs = sections.find((x) => x.id === 3);
  const code = sections.find((x) => x.id === 10);
  if (!fs || !code || functions.length === 0) return bytes;
  const [count] = readLeb(bytes, fs.payload);
  const hints = new Uint8Array(count);
  for (const f of functions) if (f >= 0 && f < count) hints[f] = 0x12;
  const name = new TextEncoder().encode('compilationHints');
  const payload = concat([new Uint8Array(leb(name.length)), name, new Uint8Array(leb(count)), hints]);
  const section = concat([new Uint8Array([0, ...leb(payload.length)]), payload]);
  return concat([bytes.subarray(0, code.start), section, bytes.subarray(code.start)]);
}

// BYTES with a metadata.code.compilation_priority section that names
// FUNCTIONS (indexes among the defined functions) to be compiled first, and
// OPTIMIZED (among them) to be compiled at once by the optimizing compiler
export function withCompilationHints(bytes, functions, optimized = []) {
  // (without the hints it may have)
  let sections = wasmSections(bytes);
  const old = sections.find((x) => x.name === 'metadata.code.compilation_priority');
  if (old) {
    bytes = concat([bytes.subarray(0, old.start), bytes.subarray(old.end)]);
    sections = wasmSections(bytes);
  }
  if (functions.length === 0) return bytes;
  const { funcs } = wasmImports(bytes, sections);
  const code = sections.find((x) => x.id === 10);
  const name = new TextEncoder().encode('metadata.code.compilation_priority');
  const body = [...leb(functions.length)];
  for (const f of [...new Set(functions)].sort((a, b) => a - b)) {
    // (compilation priority 0, and optimization priority 0 for OPTIMIZED)
    if (optimized.includes(f)) body.push(...leb(funcs + f), 0, 2, 0, 0);
    else body.push(...leb(funcs + f), 0, 1, 0);
  }
  const payload = concat([new Uint8Array(leb(name.length)), name, new Uint8Array(body)]);
  const section = concat([new Uint8Array([0, ...leb(payload.length)]), payload]);
  return concat([bytes.subarray(0, code.start), section, bytes.subarray(code.start)]);
}

// ---------------------------------------------------------------------------
// Heap images (runtime/image.lisp).  A file of a heap image:
//   "CLYSMIMG", the length of the header (u32), the header (JSON), then from
//   an offset that is a multiple of 8 the octets of the modules compiled in
//   the image, then the words of the heap.
// The header: { build, tableLength, modules: [{ offset, length, codeBase }],
//   modulesOffset, heapOffset, heapLength }; BUILD names the build of the
//   image (its custom section clysm.build), for which the file is valid.
//   With state.imageFasls (CLYSM_SAVE_IMAGE=fasls), a module read from a
//   fasl is { file, offset, length, codeBase }: its octets are read there.

// (MODULE: a compiled module, or its octets)
export function imageBuild(module) {
  if (module instanceof WebAssembly.Module) {
    const sections = WebAssembly.Module.customSections(module, 'clysm.build');
    return sections.length ? new TextDecoder().decode(sections[0]) : null;
  }
  const bytes = new Uint8Array(module);
  const s = wasmSections(bytes).find((x) => x.name === 'clysm.build');
  if (!s) return null;
  const [l, p] = readLeb(bytes, s.payload);
  return new TextDecoder().decode(bytes.subarray(p + l, s.end));
}

function saveImage(state, path, nbytes) {
  const fs = globalThis.__clysm_fs;
  const ex = state.instance.exports;
  const heap = new Uint8Array(ex.memory.buffer, 0, nbytes).slice();
  const external = (m) => state.imageFasls && m.source;
  // (without the bodies of their RUN and SNAP, which an image started from
  // the file does not call: it cannot be saved again)
  const blobs = state.modules.map((m) => external(m) ? null
                                  : withoutBodies(m.bytes instanceof Uint8Array ? m.bytes : new Uint8Array(m.bytes),
                                                  ['run', 'snap']));
  const align8 = (n) => (n + 7) & ~7;
  let offset = 0;
  const modules = state.modules.map((m, i) => {
    if (external(m)) return { file: m.source.path, offset: m.source.offset, length: m.bytes.length, codeBase: m.codeBase };
    const e = { offset, length: blobs[i].length, codeBase: m.codeBase };
    offset = align8(offset + blobs[i].length);
    return e;
  });
  // the image as it starts from the file (imageVariant), after the modules
  const variant = state.imageBytes ? imageVariant(state.imageBytes) : null;
  let image;
  if (variant) { image = { offset, length: variant.length }; offset = align8(offset + variant.length); }
  const header = { build: state.build, tableLength: ex.codes.length, modules, image, modulesLength: offset,
                   heapLength: nbytes };
  let json = new TextEncoder().encode(JSON.stringify(header));
  const modulesOffset = align8(12 + json.length);
  const heapOffset = modulesOffset + offset;
  const out = new Uint8Array(heapOffset + nbytes);
  out.set(new TextEncoder().encode('CLYSMIMG'), 0);
  new DataView(out.buffer).setUint32(8, json.length, true);
  out.set(json, 12);
  state.modules.forEach((m, i) => { if (blobs[i]) out.set(blobs[i], modulesOffset + modules[i].offset); });
  if (variant) out.set(variant, modulesOffset + image.offset);
  out.set(heap, heapOffset);
  fs.writeFileSync(path, out);
  state.codeIndex = null;
}

// The header of the heap image FILE (octets), or null if it is not one for
// the image MODULE (a compiled module, its octets, or the name of its build).
export function imageHeader(module, file) {
  if (!file || file.length < 12) return null;
  if (new TextDecoder().decode(file.subarray(0, 8)) !== 'CLYSMIMG') return null;
  const length = new DataView(file.buffer, file.byteOffset).getUint32(8, true);
  const header = JSON.parse(new TextDecoder().decode(file.subarray(12, 12 + length)));
  if (!header.build || header.build !== (typeof module === 'string' ? module : imageBuild(module))) return null;
  header.modulesOffset = (12 + length + 7) & ~7;
  header.heapOffset = header.modulesOffset + header.modulesLength;
  // (a file cut short)
  if (header.heapOffset + header.heapLength > file.length) return null;
  return header;
}

// The octets of the image as it starts from the heap image FILE (imageVariant),
// if the file has them
export function imageOctets(file, header) {
  if (!header.image) return null;
  const start = header.modulesOffset + header.image.offset;
  if (start + header.image.length > header.heapOffset) return null;
  const octets = file.subarray(start, start + header.image.length);
  // (they end with the image's clysm.build section: a tool that rewrote the
  // file without knowing them may have left their place stale)
  const tail = new TextDecoder('latin1').decode(octets.subarray(Math.max(0, octets.length - 64)));
  return tail.endsWith('\x0bclysm.build' + header.build) ? octets : null;
}

// The heap image FILE (with its HEADER) with OCTETS as the image's octets
// as it starts from it
export function imageWithOctets(file, header, octets) {
  const align8 = (n) => (n + 7) & ~7;
  const { modulesOffset, heapOffset, ...h } = header;
  const before = file.subarray(modulesOffset, modulesOffset + header.image.offset);
  h.image = { offset: header.image.offset, length: octets.length };
  h.modulesLength = align8(header.image.offset + octets.length);
  const json = new TextEncoder().encode(JSON.stringify(h));
  const mo = align8(12 + json.length);
  const out = new Uint8Array(mo + h.modulesLength + h.heapLength);
  out.set(file.subarray(0, 8), 0);
  new DataView(out.buffer).setUint32(8, json.length, true);
  out.set(json, 12);
  out.set(before, mo);
  out.set(octets, mo + h.image.offset);
  out.set(file.subarray(heapOffset, heapOffset + h.heapLength), mo + h.modulesLength);
  return out;
}

// Compile the modules of a heap image (the host can do this while it
// instantiates the image); READ(path) reads the fasls it refers to.
export function compileImageModules(file, header, transform = (bytes) => bytes, read = null) {
  // (linked into one module: host/link.mjs)
  if (header.linked) {
    const start = header.modulesOffset + header.linked.offset;
    return Promise.all([WebAssembly.compile(transform(file.subarray(start, start + header.linked.length), 0))]);
  }
  const files = new Map();
  const octets = (e) => {
    if (!e.file) return file.subarray(header.modulesOffset + e.offset, header.modulesOffset + e.offset + e.length);
    if (!files.has(e.file)) files.set(e.file, read(e.file));
    return files.get(e.file).subarray(e.offset, e.offset + e.length);
  };
  return Promise.all(header.modules.map((e, i) => WebAssembly.compile(transform(octets(e), i))));
}

// Start the image (state.instance, whose _start has not run) from the heap
// image FILE: the modules, the objects, then the function given to
// SAVE-IMAGE.  If the objects cannot be made, an exception is thrown before
// any Lisp code has run (the host can then instantiate the image again and
// run _start instead).
export async function loadImage(state, file, header, modules) {
  const ex = state.instance.exports;
  const codes = ex.codes;
  if (codes.length > header.tableLength) throw new Error('heap image: the table of functions is longer');
  codes.grow(header.tableLength - codes.length);
  let instances, restores;
  if (header.linked) {
    // (asynchronously: a page may not instantiate a large module at once)
    const inst = await WebAssembly.instantiate(modules[0], {
      ...state.imports,
      clysm: { mv_count: ex.mv_count, mv_buf: ex.mv_buf, empty_slots: ex.empty_slots, dyn: ex.dyn, nil_symbol: ex.nil_symbol,
               poll_count: ex.poll_count, lisp_throw: ex.lisp_throw, memory: ex.memory, codes },
    });
    restores = header.modules.map((e, i) => inst.exports['restore' + i]);
  } else {
    instances = header.modules.map((e, i) => instantiateLinked(state, modules[i], e.codeBase));
    restores = instances.map((inst) => inst.exports.restore);
  }
  const pages = Math.ceil(header.heapLength / 65536);
  const memory = ex.memory;
  if (memory.buffer.byteLength < pages * 65536) memory.grow(pages - memory.buffer.byteLength / 65536);
  new Uint8Array(memory.buffer).set(file.subarray(header.heapOffset, header.heapOffset + header.heapLength), 0);
  ex.image_load(0);
  restores.forEach((restore, i) => restore(ex.image_module_snap(i)));
}

export function makeCommonEnv(state) {
  const ids = new WeakMap();
  let nextId = 1;
  let charBuf = '';
  state.setCharBuf = (s) => { charBuf = s; };
  const math1 = [Math.sin, Math.cos, Math.tan, Math.asin, Math.acos, Math.atan, Math.exp, Math.log,
                 Math.sqrt, Math.sinh, Math.cosh, Math.tanh, Math.asinh, Math.acosh, Math.atanh,
                 Math.floor, Math.ceil, Math.trunc, Math.round, Math.log10, Math.log2];
  const math2 = [Math.pow, Math.atan2, (a, b) => a % b];
  return {
    identity_hash(x) {
      if (x === null || (typeof x !== 'object' && typeof x !== 'function')) return 0;
      let id = ids.get(x);
      if (id === undefined) { id = nextId++ & 0x1FFFFFFF; ids.set(x, id); }
      return id;
    },
    get_char(i) { return charBuf.charCodeAt(i); },
    math1(x, op) { return math1[op](x); },
    math2(x, y, op) { return math2[op](x, y); },
    time(kind) {
      if (kind === 0) return Date.now();
      if (kind === 1) return performance.now();
      return 0;
    },
    backtrace(fd) {
      // Lisp frames appear in the JS stack with the names from the wasm name section
      const old = Error.stackTraceLimit; Error.stackTraceLimit = 60;
      const stack = new Error().stack || '';
      const frames = (state.nameFrames ? state.nameFrames(stack) : stack).split('\n').slice(1);
      Error.stackTraceLimit = old;
      const names = [];
      for (const f of frames) {
        const m = f.match(/at (.+?) \(wasm:.*?wasm-function\[(\d+)\]/);
        if (!m) continue;
        let name = m[1].replace(/#\d+(\/generic)?$/, '');
        const where = state.sourceOf && state.sourceOf(Number(m[2]));
        if (name.startsWith(':') || name.startsWith('(') || /^(BACKTRACE|%BACKTRACE|ERROR|%ERROR\*|%REPORT-AND-EXIT|INVOKE-DEBUGGER|SIGNAL|%SIGNAL\*|%TYPE-ERROR|%COERCE-TO-CONDITION)$/.test(name)) continue;
        if (name === 'toplevel') { names.push('<toplevel>'); break; }
        names.push(where ? `${name} (${where})` : name);
      }
      const text = 'Backtrace:\n' + names.map((n, i) => `  ${i}: ${n}`).join('\n') + '\n';
      if (state.writeText) state.writeText(fd, text);
    },
    // the same, returning the module's RUN function, which the image calls
    instantiate_module(bytes) {
      let u8 = bytes;
      if (!(bytes instanceof Uint8Array)) {
        u8 = new Uint8Array(bytes.length);
        for (let i = 0; i < bytes.length; i++) u8[i] = bytes.charCodeAt(i);
      }
      return instantiateLinked(state, u8).exports.run;
    },
    // heap images (runtime/image.lisp)
    code_index(f) {
      if (!state.codeIndex) {
        const codes = state.instance.exports.codes;
        state.codeIndex = new Map();
        for (let i = 0; i < codes.length; i++) {
          const g = codes.get(i);
          if (g && !state.codeIndex.has(g)) state.codeIndex.set(g, i);
        }
      }
      const i = state.codeIndex.get(f);
      return i === undefined ? -1 : i;
    },
    module_count() { return Array.isArray(state.modules) ? state.modules.length : -1; },
    module_snap(i) { return state.modules[i].instance.exports.snap(); },
    compile_module(bytes, consts) {
      // instantiate a module compiled inside the image and link it with it
      // BYTES is a string of octets, or a Uint8Array (e.g. read from a fasl)
      let u8 = bytes;
      if (!(bytes instanceof Uint8Array)) {
        u8 = new Uint8Array(bytes.length);
        for (let i = 0; i < bytes.length; i++) u8[i] = bytes.charCodeAt(i);
      }
      return instantiateLinked(state, u8).exports.run(consts);
    },
    put_char_buf(c) {
      if (c < 0) { state.writeBuf = state.nameBuf; state.nameBuf = ''; return; }
      state.nameBuf = (state.nameBuf || '') + String.fromCodePoint(c);
    },
  };
}

// The Math functions themselves (not wrappers): the engine calls its own
// implementation directly from Wasm.
const mathImports = {
  sin: Math.sin, cos: Math.cos, tan: Math.tan, asin: Math.asin, acos: Math.acos, atan: Math.atan,
  exp: Math.exp, log: Math.log, sinh: Math.sinh, cosh: Math.cosh, tanh: Math.tanh,
  asinh: Math.asinh, acosh: Math.acosh, atanh: Math.atanh, pow: Math.pow, atan2: Math.atan2,
};

export function makeImports(state, platform) {
  state.sourceOf ??= () => undefined;
  state.mapStack ??= (stack) => stack;
  state.nameFrames ??= (stack) => String(stack);
  const imports = {
    js: makeJsImports(state),
    env: { ...makeCommonEnv(state), ...platform },
    math: mathImports,
  };
  state.imports = imports;
  globalThis.__clysm_save_image = (path, nbytes) => saveImage(state, path, nbytes);
  return imports;
}
