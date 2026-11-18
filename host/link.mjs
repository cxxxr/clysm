// host/link.mjs — link the modules compiled in an image into one module, for
// a heap image (runtime/image.lisp): V8 spends most of the time of compiling
// thousands of small modules on each module (their types, their imports),
// and instantiating one module is one call.
//
// The modules are those of src/live.lisp: they share the image's types (the
// first recursion group of every module), import functions of the host, the
// image's globals, tag, memory and table of functions, and place their
// functions in the table from their code base (an imported global).  The
// linked module has the types of the image, then those of each module; the
// imported functions of all; the functions, globals and data of each module
// in turn; an element segment for each module at its code base; and exports
// RESTORE<i> for the RESTORE of the Ith module (the others' exports are
// left out: an image started from a heap image does not call them).

function readU(b, p) {
  let n = 0, shift = 0, c;
  do { c = b[p++]; n += (c & 0x7f) * 2 ** shift; shift += 7; } while (c & 0x80);
  return [n, p];
}
function readS(b, p) {
  let n = 0, shift = 0, c;
  do { c = b[p++]; n += (c & 0x7f) * 2 ** shift; shift += 7; } while (c & 0x80);
  if (c & 0x40) n -= 2 ** shift;
  return [n, p];
}
function uleb(n, out) {
  do { let c = n % 128; n = Math.floor(n / 128); if (n) c |= 0x80; out.push(c); } while (n);
}
function sleb(n, out) {
  for (;;) {
    const c = n & 0x7f; n = Math.floor(n / 128);
    if ((n === 0 && !(c & 0x40)) || (n === -1 && (c & 0x40))) { out.push(c); return; }
    out.push(c | 0x80);
  }
}

class Writer {
  constructor() { this.chunks = []; this.cur = []; this.length = 0; }
  byte(x) { this.cur.push(x); if (this.cur.length > 65536) this.flush(); }
  u(n) { uleb(n, this.cur); }
  s(n) { sleb(n, this.cur); }
  bytes(b) { this.flush(); this.chunks.push(b); }
  flush() { if (this.cur.length) { this.chunks.push(Uint8Array.from(this.cur)); this.cur = []; } }
  done() {
    this.flush();
    const n = this.chunks.reduce((a, c) => a + c.length, 0);
    const out = new Uint8Array(n);
    let p = 0;
    for (const c of this.chunks) { out.set(c, p); p += c.length; }
    return out;
  }
}

function sections(b) {
  const list = [];
  let p = 8;
  while (p < b.length) {
    const id = b[p++];
    let size; [size, p] = readU(b, p);
    let name = null, body = p;
    if (id === 0) { let l; [l, body] = readU(b, p); name = new TextDecoder().decode(b.subarray(body, body + l)); body += l; }
    list.push({ id, name, start: p, body, end: p + size });
    p += size;
  }
  return list;
}

const NUMERIC = new Set([0x7F, 0x7E, 0x7D, 0x7C, 0x7B, 0x78, 0x77]);

// A reader of one module that copies what it reads into a writer, with the
// type, function, global and data indexes mapped.
class Mapper {
  constructor(b, maps) { this.b = b; Object.assign(this, maps); }
  heapType(p, w) {
    const [h, q] = readS(this.b, p);
    if (h >= 0) w.s(this.type(h)); else w.bytes(this.b.subarray(p, q));
    return q;
  }
  valType(p, w) {
    const t = this.b[p];
    if (t === 0x63 || t === 0x64) { w.byte(t); return this.heapType(p + 1, w); }
    w.byte(t); return p + 1;
  }
  blockType(p, w) {
    const t = this.b[p];
    if (t === 0x40 || NUMERIC.has(t) || (t >= 0x69 && t <= 0x74)) { w.byte(t); return p + 1; }
    if (t === 0x63 || t === 0x64) return this.valType(p, w);
    const [x, q] = readS(this.b, p);
    w.s(this.type(x)); return q;
  }
  copyU(p, w) { const [x, q] = readU(this.b, p); w.u(x); return q; }
  // the instructions from P to the END that closes the expression (included)
  expr(p, w) {
    const b = this.b;
    let depth = 0;
    for (;;) {
      const op = b[p++];
      w.byte(op);
      switch (op) {
        case 0x02: case 0x03: case 0x04: p = this.blockType(p, w); depth++; break;
        case 0x1F: {   // try_table
          p = this.blockType(p, w); depth++;
          let n; [n, p] = readU(b, p); w.u(n);
          for (let i = 0; i < n; i++) {
            const k = b[p++]; w.byte(k);
            if (k === 0 || k === 1) p = this.copyU(p, w);
            p = this.copyU(p, w);
          }
          break;
        }
        case 0x0B: if (depth === 0) return p; depth--; break;
        case 0x08: case 0x0C: case 0x0D: case 0xD5: case 0xD6: case 0x20: case 0x21: case 0x22: case 0x25:
          p = this.copyU(p, w); break;
        case 0x0E: {
          let n; [n, p] = readU(b, p); w.u(n);
          for (let i = 0; i <= n; i++) p = this.copyU(p, w);
          break;
        }
        case 0x10: case 0x12: case 0xD2: { let f; [f, p] = readU(b, p); w.u(this.func(f)); break; }
        case 0x14: case 0x15: { let t; [t, p] = readU(b, p); w.u(this.type(t)); break; }
        case 0x23: case 0x24: { let g; [g, p] = readU(b, p); w.u(this.global(g)); break; }
        case 0x28: case 0x29: case 0x2A: case 0x2B: case 0x2C: case 0x2D: case 0x2E: case 0x2F:
        case 0x36: case 0x37: case 0x38: case 0x39: case 0x3A: case 0x3B: case 0x3C: case 0x3D: case 0x3E:
          p = this.copyU(p, w); p = this.copyU(p, w); break;
        case 0x3F: case 0x40: w.byte(b[p++]); break;
        case 0x41: case 0x42: { const q = readS(b, p)[1]; w.bytes(b.subarray(p, q)); p = q; break; }
        case 0x43: w.bytes(b.subarray(p, p + 4)); p += 4; break;
        case 0x44: w.bytes(b.subarray(p, p + 8)); p += 8; break;
        case 0xD0: p = this.heapType(p, w); break;
        case 0xFB: {
          let sub; [sub, p] = readU(b, p); w.u(sub);
          switch (sub) {
            case 0: case 1: case 6: case 7: case 11: case 12: case 13: case 14: case 16: {
              let t; [t, p] = readU(b, p); w.u(this.type(t)); break;
            }
            case 2: case 3: case 4: case 5: case 8: {
              let t; [t, p] = readU(b, p); w.u(this.type(t)); p = this.copyU(p, w); break;
            }
            case 9: case 18: {
              let t, d; [t, p] = readU(b, p); [d, p] = readU(b, p); w.u(this.type(t)); w.u(this.data(d)); break;
            }
            case 17: {
              let t1, t2; [t1, p] = readU(b, p); [t2, p] = readU(b, p); w.u(this.type(t1)); w.u(this.type(t2)); break;
            }
            case 20: case 21: case 22: case 23: p = this.heapType(p, w); break;
            case 24: case 25: w.byte(b[p++]); p = this.copyU(p, w); p = this.heapType(p, w); p = this.heapType(p, w); break;
            case 15: case 26: case 27: case 28: case 29: case 30: break;
            default: throw new Error(`link: GC instruction ${sub}`);
          }
          break;
        }
        case 0xFC: {
          let sub; [sub, p] = readU(b, p); w.u(sub);
          if (sub > 7) throw new Error(`link: instruction 0xFC ${sub}`);
          break;
        }
        default:
          if (!(op <= 0x01 || op === 0x05 || op === 0x0A || op === 0x0F || op === 0x1A || op === 0x1B ||
                (op >= 0x45 && op <= 0xC4) || op === 0xD1 || op === 0xD3 || op === 0xD4))
            throw new Error(`link: instruction ${op.toString(16)}`);
      }
    }
  }
  // a subtype of the type section (supertypes and fields mapped)
  subType(p, w) {
    const b = this.b;
    let t = b[p];
    if (t === 0x50 || t === 0x4F) {
      w.byte(t); p++;
      let n; [n, p] = readU(b, p); w.u(n);
      for (let i = 0; i < n; i++) { let x; [x, p] = readU(b, p); w.u(this.type(x)); }
      t = b[p];
    }
    w.byte(t); p++;
    if (t === 0x60) {
      for (let k = 0; k < 2; k++) {
        let n; [n, p] = readU(b, p); w.u(n);
        for (let i = 0; i < n; i++) p = this.valType(p, w);
      }
    } else if (t === 0x5F) {
      let n; [n, p] = readU(b, p); w.u(n);
      for (let i = 0; i < n; i++) { p = this.valType(p, w); w.byte(b[p++]); }
    } else if (t === 0x5E) {
      p = this.valType(p, w); w.byte(b[p++]);
    } else throw new Error(`link: type ${t.toString(16)}`);
    return p;
  }
}

// Skip a subtype without writing it (the shared recursion group).
function skipSubType(b, p) {
  const sink = { byte() {}, u() {}, s() {}, bytes() {} };
  const m = new Mapper(b, { type: (x) => x });
  return m.subType(p, sink);
}

// MODULES: [{ bytes, codeBase }] in their order; the linked module's octets
export function linkModules(modules) {
  let baseTypes = null, baseTypeBytes = null;
  const funcImports = new Map();   // "module\0field" -> { index, type (linked), module, field }
  const funcImportList = [];
  let globalImports = null;        // [{ module, field, bytes }] without code_base
  let otherImports = null;         // table, memory, tag: bytes as in the first module
  const types = new Writer(); let typeCount = 0;
  const parsed = [];
  // first pass: types and imports
  for (const [k, m] of modules.entries()) {
    const b = m.bytes;
    const secs = sections(b);
    const info = { b, secs, codeBase: m.codeBase };
    // types: the shared recursion group, then the module's own
    const ts = secs.find((s) => s.id === 1);
    let p = ts.start, n; [n, p] = readU(b, p);
    if (b[p] !== 0x4E) throw new Error('link: no recursion group');
    const groupStart = p;
    p++;
    let nb; [nb, p] = readU(b, p);
    for (let i = 0; i < nb; i++) p = skipSubType(b, p);
    if (baseTypes === null) { baseTypes = nb; baseTypeBytes = b.slice(groupStart, p); }
    else if (nb !== baseTypes) throw new Error('link: modules of different images');
    info.typeBase = typeCount;            // the module's own types at baseTypes + typeBase...
    info.ownTypes = n - 1;
    info.typesStart = p;
    parsed.push(info);
    // imports
    const is = secs.find((s) => s.id === 2);
    const fimps = [], gimps = [], others = [];
    if (is) {
      let q = is.start, count; [count, q] = readU(b, q);
      const text = (r) => { const [l, s] = readU(b, r); return [new TextDecoder().decode(b.subarray(s, s + l)), s + l]; };
      for (let i = 0; i < count; i++) {
        const entryStart = q;
        let mod, field; [mod, q] = text(q); [field, q] = text(q);
        const kind = b[q++];
        if (kind === 0) { let t; [t, q] = readU(b, q); fimps.push({ mod, field, type: t }); }
        else if (kind === 3) {
          const t = b[q++]; if (t === 0x63 || t === 0x64) [, q] = readS(b, q); q++;
          gimps.push({ mod, field, bytes: b.subarray(entryStart, q) });
        } else {
          if (kind === 1) { const t = b[q++]; if (t === 0x63 || t === 0x64) [, q] = readS(b, q); const f = b[q++]; [, q] = readU(b, q); if (f & 1) [, q] = readU(b, q); }
          else if (kind === 2) { const f = b[q++]; [, q] = readU(b, q); if (f & 1) [, q] = readU(b, q); }
          else if (kind === 4) { q++; [, q] = readU(b, q); }
          others.push(b.subarray(entryStart, q));
        }
      }
    }
    info.fimps = fimps; info.gimps = gimps;
    const kept = gimps.filter((g) => g.field !== 'code_base');
    if (globalImports === null) { globalImports = kept; otherImports = others; }
    else if (kept.length !== globalImports.length || kept.some((g, i) => g.field !== globalImports[i].field))
      throw new Error('link: modules importing other globals');
    typeCount += info.ownTypes;
  }
  const typeOf = (info) => (x) => (x < baseTypes ? x : baseTypes + info.typeBase + (x - baseTypes));
  // the types of each module (mapped)
  for (const info of parsed) {
    const m = new Mapper(info.b, { type: typeOf(info) });
    let p = info.typesStart;
    for (let i = 0; i < info.ownTypes; i++) {
      const t = info.b[p];
      if (t === 0x4E) {            // (a recursion group of its own)
        types.byte(0x4E); p++;
        let n; [n, p] = readU(info.b, p); types.u(n);
        for (let j = 0; j < n; j++) p = m.subType(p, types);
      } else p = m.subType(p, types);
    }
  }
  // the imported functions of all (their types are the image's)
  for (const info of parsed) {
    for (const f of info.fimps) {
      const key = f.mod + '\0' + f.field;
      if (!funcImports.has(key)) {
        const e = { index: funcImportList.length, type: typeOf(info)(f.type), mod: f.mod, field: f.field };
        funcImports.set(key, e); funcImportList.push(e);
      }
    }
  }
  // the functions, globals and data of each module in turn
  let funcCount = funcImportList.length, globalCount = globalImports.length, dataCount = 0;
  for (const info of parsed) {
    const fs = info.secs.find((s) => s.id === 3);
    let n = 0;
    if (fs) [n] = readU(info.b, fs.start);
    const gs = info.secs.find((s) => s.id === 6);
    let ng = 0;
    if (gs) [ng] = readU(info.b, gs.start);
    const ds = info.secs.find((s) => s.id === 11);
    let nd = 0;
    if (ds) [nd] = readU(info.b, ds.start);
    info.funcBase = funcCount; info.globalBase = globalCount; info.dataBase = dataCount;
    info.nfuncs = n; info.nglobals = ng; info.ndata = nd;
    funcCount += n; globalCount += ng; dataCount += nd;
    const fmap = info.fimps.map((f) => funcImports.get(f.mod + '\0' + f.field).index);
    const gmap = [];
    for (const g of info.gimps) {
      gmap.push(g.field === 'code_base' ? -1 : globalImports.findIndex((x) => x.field === g.field));
    }
    const nfi = info.fimps.length, ngi = info.gimps.length;
    info.maps = {
      type: typeOf(info),
      func: (f) => (f < nfi ? fmap[f] : info.funcBase + (f - nfi)),
      global: (g) => {
        if (g < ngi) { if (gmap[g] < 0) throw new Error('link: code_base used'); return gmap[g]; }
        return info.globalBase + (g - ngi);
      },
      data: (d) => info.dataBase + d,
    };
  }
  // the linked module
  const out = new Writer();
  out.bytes(Uint8Array.from([0, 0x61, 0x73, 0x6d, 1, 0, 0, 0]));
  const section = (id, w) => { const body = w.done(); out.byte(id); out.u(body.length); out.bytes(body); };
  { // types
    const w = new Writer(); w.u(1 + typeCount); w.bytes(baseTypeBytes); w.bytes(types.done());
    section(1, w);
  }
  { // imports
    const w = new Writer();
    w.u(otherImports.length + funcImportList.length + globalImports.length);
    for (const o of otherImports) w.bytes(o);
    const name = (s) => { const t = new TextEncoder().encode(s); w.u(t.length); w.bytes(t); };
    for (const f of funcImportList) { name(f.mod); name(f.field); w.byte(0); w.u(f.type); }
    for (const g of globalImports) w.bytes(g.bytes);
    section(2, w);
  }
  { // functions
    const w = new Writer(); w.u(funcCount - funcImportList.length);
    for (const info of parsed) {
      const fs = info.secs.find((s) => s.id === 3);
      if (!fs) continue;
      let p = fs.start, n; [n, p] = readU(info.b, p);
      for (let i = 0; i < n; i++) { let t; [t, p] = readU(info.b, p); w.u(info.maps.type(t)); }
    }
    section(3, w);
  }
  { // globals
    const w = new Writer(); w.u(globalCount - globalImports.length);
    for (const info of parsed) {
      const gs = info.secs.find((s) => s.id === 6);
      if (!gs) continue;
      const m = new Mapper(info.b, info.maps);
      let p = gs.start, n; [n, p] = readU(info.b, p);
      for (let i = 0; i < n; i++) { p = m.valType(p, w); w.byte(info.b[p++]); p = m.expr(p, w); }
    }
    section(6, w);
  }
  { // exports: RESTORE<i>
    const w = new Writer();
    const entries = [];
    for (const [k, info] of parsed.entries()) {
      const es = info.secs.find((s) => s.id === 7);
      if (!es) continue;
      let p = es.start, n; [n, p] = readU(info.b, p);
      for (let i = 0; i < n; i++) {
        let l; [l, p] = readU(info.b, p);
        const name = new TextDecoder().decode(info.b.subarray(p, p + l)); p += l;
        const kind = info.b[p++]; let x; [x, p] = readU(info.b, p);
        if (kind === 0 && name === 'restore') entries.push(['restore' + k, info.maps.func(x)]);
      }
    }
    w.u(entries.length);
    for (const [name, f] of entries) { const t = new TextEncoder().encode(name); w.u(t.length); w.bytes(t); w.byte(0); w.u(f); }
    section(7, w);
  }
  { // elements: each module's functions at its code base
    const w = new Writer();
    const segs = [];
    for (const info of parsed) {
      const es = info.secs.find((s) => s.id === 9);
      if (!es) continue;
      let p = es.start, n; [n, p] = readU(info.b, p);
      for (let i = 0; i < n; i++) {
        const flags = info.b[p++];
        if (flags !== 0) throw new Error(`link: element segment ${flags}`);
        // (the offset: global.get code_base, end)
        if (info.b[p] !== 0x23) throw new Error('link: element offset');
        p++; [, p] = readU(info.b, p); p++;
        let count; [count, p] = readU(info.b, p);
        const funcs = [];
        for (let j = 0; j < count; j++) { let f; [f, p] = readU(info.b, p); funcs.push(info.maps.func(f)); }
        segs.push({ base: info.codeBase, funcs });
      }
    }
    w.u(segs.length);
    for (const s of segs) {
      w.byte(0); w.byte(0x41); w.s(s.base); w.byte(0x0B);
      w.u(s.funcs.length); for (const f of s.funcs) w.u(f);
    }
    section(9, w);
  }
  if (dataCount) { const w = new Writer(); w.u(dataCount); section(12, w); }
  { // code
    const w = new Writer(); w.u(funcCount - funcImportList.length);
    for (const info of parsed) {
      const cs = info.secs.find((s) => s.id === 10);
      if (!cs) continue;
      const m = new Mapper(info.b, info.maps);
      let p = cs.start, n; [n, p] = readU(info.b, p);
      for (let i = 0; i < n; i++) {
        let size; [size, p] = readU(info.b, p);
        const end = p + size;
        const body = new Writer();
        let q = p, groups; [groups, q] = readU(info.b, q); body.u(groups);
        for (let g = 0; g < groups; g++) { q = m.copyU(q, body); q = m.valType(q, body); }
        q = m.expr(q, body);
        if (q !== end) throw new Error('link: a function body was not read to its end');
        const bytes = body.done();
        w.u(bytes.length); w.bytes(bytes);
        p = end;
      }
    }
    section(10, w);
  }
  if (dataCount) { // data (passive)
    const w = new Writer(); w.u(dataCount);
    for (const info of parsed) {
      const ds = info.secs.find((s) => s.id === 11);
      if (!ds) continue;
      let p = ds.start, n; [n, p] = readU(info.b, p);
      for (let i = 0; i < n; i++) {
        const flags = info.b[p++];
        if (flags !== 1) throw new Error(`link: data segment ${flags}`);
        let l; [l, p] = readU(info.b, p);
        w.byte(1); w.u(l); w.bytes(info.b.subarray(p, p + l)); p += l;
      }
    }
    section(11, w);
  }
  { // names of the functions (stack traces)
    const names = [];
    for (const f of funcImportList) names.push([f.index, f.field]);
    for (const info of parsed) {
      const ns = info.secs.find((s) => s.name === 'name');
      if (!ns) continue;
      let p = ns.body;
      while (p < ns.end) {
        const id = info.b[p++]; let size; [size, p] = readU(info.b, p);
        if (id === 1) {
          let q = p, n; [n, q] = readU(info.b, q);
          const nfi = info.fimps.length;
          for (let i = 0; i < n; i++) {
            let x, l; [x, q] = readU(info.b, q); [l, q] = readU(info.b, q);
            if (x >= nfi) names.push([info.maps.func(x), info.b.subarray(q, q + l)]);
            q += l;
          }
        }
        p += size;
      }
    }
    const sub = new Writer(); sub.u(names.length);
    for (const [x, name] of names) {
      const t = typeof name === 'string' ? new TextEncoder().encode(name) : name;
      sub.u(x); sub.u(t.length); sub.bytes(t);
    }
    const w = new Writer();
    const label = new TextEncoder().encode('name');
    w.u(label.length); w.bytes(label);
    const s = sub.done(); w.byte(1); w.u(s.length); w.bytes(s);
    section(0, w);
  }
  return out.done();
}
