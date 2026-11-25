// Write the JavaScript entry of a Lisp library (bin/clysm library):
//   node tools/gen-library.mjs DIR/NAME.wasm
// loads the module once to learn its exports (JS:EXPORT), then writes
// DIR/NAME.mjs (an ES module whose named exports are the Lisp functions),
// DIR/NAME.d.ts (their TypeScript types) and copies the host files that
// they need into DIR/clysm/; the names and places of the functions, for
// stack traces, are moved from DIR/NAME.wasm to DIR/NAME.wasm.debug.
import fs from 'node:fs';
import path from 'node:path';
import { gzipSync } from 'node:zlib';
import { spawnSync } from 'node:child_process';
import { fileURLToPath } from 'node:url';
import { load } from '../host/lib.mjs';

const wasmPath = path.resolve(process.argv[2]);
const dir = path.dirname(wasmPath);
const name = path.basename(wasmPath, '.wasm');

// The names and places of the functions (the custom sections name and
// clysm.sources, 7 % of a module) go to NAME.wasm.debug, a module of those
// sections only, which the host reads when it first maps a stack trace
// (host/lib.mjs): a page or a package can leave it out.  (A library that
// holds its module in itself holds them in a string of their own, decoded
// when a stack trace is first mapped: 4 % less to decompress and compile
// when it starts.)
const compiled = fs.readFileSync(wasmPath);
let kept = compiled, debugBytes = null;
// (a module whose sections were taken out already, by gen-library run on it
// before: they are in NAME.wasm.debug)
const debugPath = `${wasmPath}.debug`;
{
  const bytes = compiled;
  const leb = (p) => { let x = 0, s = 0, c; do { c = bytes[p++]; x |= (c & 127) << s; s += 7; } while (c & 128); return [x >>> 0, p]; };
  const keep = [bytes.subarray(0, 8)], debug = [bytes.subarray(0, 8)];
  let p = 8;
  while (p < bytes.length) {
    const start = p, id = bytes[p++];
    let size; [size, p] = leb(p);
    let section = null;
    if (id === 0) { const [l, q] = leb(p); section = bytes.subarray(q, q + l).toString(); }
    (section === 'name' || section === 'clysm.sources' ? debug : keep).push(bytes.subarray(start, p + size));
    p += size;
  }
  if (debug.length > 1) {
    kept = Buffer.concat(keep);
    debugBytes = Buffer.concat(debug);
    fs.writeFileSync(wasmPath, kept);
    fs.writeFileSync(debugPath, debugBytes);
  } else if (fs.existsSync(debugPath)) {
    debugBytes = fs.readFileSync(debugPath);
  }
}
const lib = await load(wasmPath, { stdout: () => {}, stderr: (t) => process.stderr.write(t) });

const identifier = /^[A-Za-z_$][A-Za-z0-9_$]*$/;
const entries = Object.entries(lib.exports);
const named = entries.filter(([k]) => identifier.test(k));
// What the library said of its exports (JS:EXPORT), its classes (JS:DEFINE-CLASS),
// its types (JS:DECLARE-TYPES) and how it is loaded (JS:LIBRARY-OPTIONS).
const info = lib.library ?? { exports: {}, declarations: [], classes: {}, options: {}, defined: {} };
const defined = info.defined ?? {};
const reexports = info.reexports ?? [];
const entrypoints = info.entrypoints ?? [];
const entryOf = (k) => info.exports[k]?.entry ?? null;
// (by the class itself: a class may be exported under another name than its own)
const classOf = (k, v) => {
  if (typeof v !== 'function') return null;
  const name = Object.keys(defined).find((n) => defined[n] === v);
  return name === undefined ? null : info.classes[name];
};

// The name a class is declared under in a module of the exports MINE: its own when it is exported
// under it there, else one of its own (see declarationOf; unnamedClasses declares it as it is).
const localClassName = (spec, mine) =>
  mine.some(([n]) => n === spec.name) && !mine.some(([n, v]) => n === spec.name && classOf(n, v) === spec)
    ? `__clysm_${spec.name}` : spec.name;

// The declaration of a class of no TYPE: any arguments, any members.
const fallbackDeclaration = (name) => `declare class ${name} {\n  constructor(...args: any[]);\n  [key: string]: any;\n}`;
const typeOf = (k, f) => {
  const given = info.exports[k]?.type ?? (typeof f === 'function' ? f.clysmType : undefined);
  if (given) return given;
  if (typeof f === 'function') return f.clysmAsync ? '(...args: any[]) => Promise<any>' : '(...args: any[]) => any';
  return ['number', 'string', 'boolean'].includes(typeof f) ? typeof f : 'any';
};
// The declaration of an export in a .d.ts: a class as it was declared
// (JS:DEFINE-CLASS's TYPE), the others as constants of their type.
// The declarations of the exports MINE of a module: a class is declared once there, whatever
// the names it is exported under.
const declarationsOfExports = (mine) => {
  const ownNamed = new Set(mine.filter(([k, f]) => classOf(k, f)?.name === k).map(([k]) => k));
  const declared = new Set();
  const names = new Set(mine.map(([k]) => k));
  return mine.map(([k, f]) => declarationOf(k, f, ownNamed, declared, names)).join('\n');
};
// (OWN-NAMED: the classes exported under their own names in the module, declared so; DECLARED:
// those declared under a name of their own, once)
const declarationOf = (k, f, ownNamed = new Set(), declared = new Set(), names = new Set()) => {
  const spec = classOf(k, f);
  // (a type JS:EXPORT gave a class is its declaration: that of the class itself is not needed)
  if (spec && !info.exports[k]?.type) {
    const text = spec.type ? spec.type.trim() : fallbackDeclaration(spec.name);
    if (spec.name === k) return `export ${text}`;
    if (ownNamed.has(spec.name)) return `export { ${spec.name} as ${k} };`;
    const first = !declared.has(spec.name);
    declared.add(spec.name);
    // (when the class's own name is another export's, it is declared under one of its own, and
    // its declaration names it so throughout)
    const taken = names.has(spec.name);
    const local = taken ? `__clysm_${spec.name}` : spec.name;
    // (not in a string literal, nor as a member's name after a dot)
    const word = new RegExp(`(?<![\\w$.])${spec.name.replace(/\$/g, '\\$')}(?![\\w$])`, 'g');
    const renamed = taken
      ? text.split(/('(?:[^'\\]|\\.)*'|"(?:[^"\\]|\\.)*"|`(?:[^`\\]|\\.)*`)/)
          .map((part, i) => (i % 2 === 1 ? part : part.replace(word, local))).join('')
      : text;
    return `${first ? `${renamed}\n` : ''}export { ${local} as ${k} };`;
  }
  return `export declare const ${k}: ${typeOf(k, f)};`;
};
const declarationsOf = (entry) =>
  info.declarations.filter((d) => (d.entry ?? null) === entry).map((d) => `${d.text.trim()}\n`).join('');

// The declarations of the classes exported only under names that are no identifiers (a property
// of the library's exports, and of its interface Exports), which MINE does not declare.
const unnamedClasses = (mine) => {
  const declared = new Set(mine.map(([k, f]) => classOf(k, f)?.name).filter(Boolean));
  const texts = [];
  for (const [k, f] of entries) {
    const spec = identifier.test(k) || info.exports[k]?.type ? null : classOf(k, f);
    if (spec && !declared.has(spec.name)) {
      declared.add(spec.name);
      texts.push(`${spec.type ? spec.type.trim() : fallbackDeclaration(spec.name)}\n`);
    }
  }
  return texts.join('');
};

// The modules of the entries (JS:EXPORT's ENTRY), each with the exports
// given to it: those of no entry are NAME.mjs's.
// (an entry of declarations alone, a subpath of types, is one too)
const entryNames = [...new Set([...named.map(([k]) => entryOf(k)), ...info.declarations.map((d) => d.entry ?? null), ...reexports.map((r) => r.entry), ...entrypoints.map((e) => e.entry)]
  .filter((e) => e !== null))];
for (const e of entryNames) {
  if (typeof e !== 'string' || !identifier.test(e.replace(/-/g, '_')) || e === name) throw new Error(`the entry ${JSON.stringify(e)} is no name of a module beside ${name}.mjs`);
}
// A small expression grammar, not JavaScript source supplied by the library.
// Optional property access makes a predicate safe when a host global is absent.
const entryCondition = (value) => {
  if (value === null || ['string', 'boolean', 'number'].includes(typeof value)) return JSON.stringify(value);
  if (!Array.isArray(value) || !value.length) throw new Error('invalid JS:ENTRYPOINT condition');
  const [op, ...args] = value;
  if (op === 'global' && args.length && args.every((a) => typeof a === 'string'))
    return `globalThis${args.map((a) => `?.[${JSON.stringify(a)}]`).join('')}`;
  if ((op === 'and' || op === 'or') && args.length)
    return `(${args.map(entryCondition).join(op === 'and' ? ' && ' : ' || ')})`;
  if (op === 'not' && args.length === 1) return `(!${entryCondition(args[0])})`;
  if (op === 'equal' && args.length === 2) return `(${entryCondition(args[0])} === ${entryCondition(args[1])})`;
  if (op === 'present' && args.length === 1) return `(${entryCondition(args[0])} != null)`;
  if (op === 'function' && args.length === 1) return `(typeof ${entryCondition(args[0])} === 'function')`;
  throw new Error(`invalid JS:ENTRYPOINT condition operator ${JSON.stringify(op)}`);
};
const entryMessage = (value) => {
  if (typeof value === 'string') return JSON.stringify(value);
  if (!Array.isArray(value) || !value.length) throw new Error('invalid JS:ENTRYPOINT check message');
  const [op, ...args] = value;
  if (op === 'global' && args.length && args.every((a) => typeof a === 'string'))
    return `String(${entryCondition(value)})`;
  if (op === 'concat' && args.length) return `(${args.map(entryMessage).join(' + ')})`;
  throw new Error('invalid JS:ENTRYPOINT check message');
};
const entryInitializers = new Map();
for (const spec of entrypoints) {
  if (!info.options?.lazy || typeof spec.entry !== 'string' || typeof spec.run !== 'function')
    throw new Error('JS:ENTRYPOINT needs a named entry and a function in a lazy library');
  if (entryInitializers.has(spec.entry)) throw new Error(`the entry ${spec.entry} has more than one initializer`);
  if (!Array.isArray(spec.imports) || spec.imports.some((i) => !Array.isArray(i) || i.length !== 2
      || typeof i[0] !== 'string' || !i[0].length || typeof i[1] !== 'string'
      || (i[1] !== '*' && !identifier.test(i[1])))
      || new Set(spec.imports.map((i) => i[0])).size !== spec.imports.length)
    throw new Error('JS:ENTRYPOINT imports need unique module specifiers and export names (or *)');
  if (!Array.isArray(spec.environment) || spec.environment.some((v) => typeof v !== 'string' || !identifier.test(v))
      || new Set(spec.environment).size !== spec.environment.length)
    throw new Error('JS:ENTRYPOINT environment names must be unique identifiers');
  if (![null, 'stderr'].includes(spec.errors)) throw new Error('invalid JS:ENTRYPOINT error policy');
  spec.checks ??= [];
  spec.loadErrors ??= [];
  if (!Array.isArray(spec.checks) || spec.checks.some((c) => !Array.isArray(c) || c.length !== 2))
    throw new Error('JS:ENTRYPOINT checks need predicate/message pairs');
  for (const [condition, message] of spec.checks) { entryCondition(condition); entryMessage(message); }
  if (!Array.isArray(spec.loadErrors) || spec.loadErrors.some((e) => !Array.isArray(e) || e.length !== 2
      || typeof e[0] !== 'string' || !e[0].length || typeof e[1] !== 'string')
      || new Set(spec.loadErrors.map((e) => e[0])).size !== spec.loadErrors.length)
    throw new Error('JS:ENTRYPOINT load errors need unique code/message string pairs');
  entryCondition(spec.condition);
  entryInitializers.set(spec.entry, spec);
}
const exportsOf = (entry) => named.filter(([k]) => entryOf(k) === entry);
// Explicit named re-exports keep a public entry narrow without handwritten glue.
// Validate before writing entry modules; JSON.stringify quotes module specifiers.
const exportedNames = new Map(entryNames.map((e) => [e, new Set(exportsOf(e).map(([k]) => k))]));
for (const r of reexports) {
  if (typeof r.entry !== 'string' || typeof r.from !== 'string' || !r.from.length
      || !Array.isArray(r.names) || !r.names.length
      || r.names.some((n) => typeof n !== 'string' || (!identifier.test(n) || n.startsWith('__clysm_')))) {
    throw new Error('JS:REEXPORT needs a named entry, a module specifier and one or more export names');
  }
  const names = exportedNames.get(r.entry);
  for (const n of r.names) {
    if (names.has(n)) throw new Error(`the entry ${r.entry} exports ${n} more than once`);
    names.add(n);
  }
}
// (the names of the modules' helpers)
const reserved = entries.filter(([k]) => k.startsWith('__clysm_'));
if (reserved.length) throw new Error(`a library cannot export ${reserved.map(([k]) => k).join(', ')}: names that begin with __clysm_ are its modules' own`);
// (".js" for a package of "type": "module", whose TypeScript pairs NAME.js with NAME.d.ts)
const ext = info.options?.extension ?? '.mjs';
if (!['.mjs', '.js'].includes(ext)) throw new Error(`the extension of the modules is .mjs or .js, not ${ext}`);
const files = [];
const write = (file, text) => { fs.writeFileSync(path.join(dir, file), text); files.push(file); };

// (JS:LIBRARY-OPTIONS :OUTPUT :ERROR: what Lisp writes to its standard output goes to the standard error)
const toStderr = info.options?.output === 'error';
const outputOptions = toStderr
  ? '// (what Lisp writes to its standard output goes to the standard error output)\nimport { writeToStderr as __clysm_stderr } from \'./clysm/jsffi.mjs\';\nconst __clysm_defaults = { stdout: __clysm_stderr };\n'
  : 'const __clysm_defaults = {};\n';

// (JS:LIBRARY-OPTIONS :EMBED: the module's bytes are in its modules, which load it with the host
// of host/web/lib.mjs, of no Node.js: a page, a browser's worker, Electron's renderer, Node.js)
const embed = info.options?.embed === true;
const locations = info.options?.locations ?? [];
if (!Array.isArray(locations) || locations.some((p) => typeof p !== 'string' || !p.length)
    || (embed && locations.length)) throw new Error('library locations must be nonempty paths of a non-embedded library');
const locationHelper = locations.length ? `import { existsSync as __clysm_exists } from 'node:fs';
const __clysm_locations = ${JSON.stringify(locations)}.map((p) => new URL(p, import.meta.url));
const __clysm_module_file = () => __clysm_locations.find((p) => __clysm_exists(p)) ?? __clysm_locations[0];
` : '';
const compression = info.options?.compression ?? null;
if (compression !== null && (!embed || !['gzip', 'xz'].includes(compression))) {
  throw new Error('library compression must be gzip or xz with embed');
}
const pack = (bytes) => {
  if (compression === 'gzip') return gzipSync(bytes, { level: 9 });
  if (compression === 'xz') {
    const result = spawnSync('xz', ['--format=xz', '--compress', '--stdout', '--check=crc32', '-9'],
      { input: bytes, maxBuffer: Math.max(1024 * 1024, bytes.length * 2) });
    if (result.error || result.status !== 0) {
      throw new Error(`XZ library compression requires a working xz command: ${result.error?.message ?? result.stderr.toString()}`);
    }
    return result.stdout;
  }
  return bytes;
};
const embedded = embed ? pack(kept).toString('base64') : null;
const embeddedDebug = embed && debugBytes ? pack(debugBytes).toString('base64') : null;
const compressionLabel = compression === 'gzip' ? 'gzipped then ' : compression === 'xz' ? 'XZ-compressed then ' : '';
const embeddedHost = compression === 'xz' ? './clysm/web/xz.mjs'
  : compression === 'gzip' ? './clysm/web/compressed.mjs' : './clysm/web/lib.mjs';
const loaderImport = embed
  ? `import { bytesOfBase64, loadSync } from '${embeddedHost}';`
  : "import { loadSync } from './clysm/lib.mjs';";
const embeddedOptions = toStderr ? '{ outputToErrors: true }' : '{}';
// (the names and places of the functions, for a stack trace: decoded only
// when one is mapped, host/common.mjs)
const embeddedDebugText = embeddedDebug
  ? `// The names and places of its functions, ${compressionLabel}in base64 (${debugBytes.length} decoded bytes): read when a stack trace is first mapped.\nconst __clysm_debug = '${embeddedDebug}';\nconst __clysm_debug_bytes = () => ${info.options?.lazy ? 'bytesOfBase64' : '__clysm_bytes_of'}(__clysm_debug);\n`
  : '';

if (info.options?.lazy) {
  // A lazy library: importing its modules loads nothing. NAME.library.mjs
  // loads the module at the first call of an export or use of a class
  // (loadSync, in the calling thread), and has made the classes; the
  // constants are written into the modules.
  // (a constant is written as JSON: what JSON would change, such as NaN, a function in an
  // object or an object of a class, is refused)
  const json = (v) => v === null || typeof v === 'string' || typeof v === 'boolean'
    || (typeof v === 'number' && Number.isFinite(v) && !Object.is(v, -0))
    // (an array without holes, which JSON would write as null)
    || (Array.isArray(v) && Object.keys(v).length === v.length && v.every(json))
    || (typeof v === 'object' && Object.getPrototypeOf(v) === Object.prototype && Object.values(v).every(json));
  const unwritable = named.filter(([k, v]) => typeof v !== 'function' && !json(v));
  if (unwritable.length) throw new Error(`a lazy library exports functions, classes and constants of JSON: not ${unwritable.map(([k]) => k).join(', ')}`);
  // (the names the modules import their helpers as)
  const classes = Object.entries(info.classes).filter(([k]) => defined[k]);
  write(`${name}.library${ext}`, `// Generated by Clysm (bin/clysm library) from ${name}.wasm: what loads it,
// once, at the first call of one of its exports or use of one of its classes.
// useLibrary() gives another module (or the same with other options of load()
// in clysm/lib.mjs) before that.
${loaderImport}
${locationHelper}import { makeClass } from './clysm/jsffi.mjs';

${embed ? `// The module, ${compressionLabel}in base64 (${kept.length} decoded bytes).\nconst embedded = '${embedded}';\n${embeddedDebugText}let source = null;` : (locations.length ? 'let source = null;' : `let source = new URL('./${name}.wasm', import.meta.url);`)}
let given = {};
let lib = null;
// (the module loadLibrary() compiled, of the source it was given then)
let compiled = null;
let loading = null;
${embed ? `const __clysm_defaults = ${embeddedOptions};\n` : outputOptions}
export function library() {
  lib ??= loadSync(compiled ?? ${embed ? 'source ?? bytesOfBase64(embedded)' : locations.length ? 'source ?? __clysm_module_file()' : 'source'}, { ...__clysm_defaults, ${embeddedDebug ? '...(source ? {} : { debug: __clysm_debug_bytes }), ' : ''}...given, classes${entrypoints.length ? ', modules: { ...entryModules, ...given.modules }, entryContexts' : ''} });
  return lib;
}

export function useLibrary(module, options = {}) {
  if (lib) throw new Error('useLibrary: the library of ${name}.wasm is loaded already');
  source = module;
  given = options;
  compiled = null;
}

// library(), with the module compiled by WebAssembly.compile, which leaves the
// thread free meanwhile (a page draws): the bytes are still decoded, and the
// module instantiated and its toplevel forms run, in the calling thread. Once
// it is settled library() is that library; one already loaded is given as it is.
export function loadLibrary() {
  loading ??= (async () => {
    const from = source;
    const bytes = lib ? null : ${embed ? 'from ?? bytesOfBase64(embedded)' : 'from'};
    if (bytes instanceof Uint8Array) {
      const module = await WebAssembly.compile(bytes);
      // (not if useLibrary() gave another module meanwhile)
      if (!lib && source === from) compiled = module;
    }
    return library();
  })();
  return loading;
}

export const classes = {};
${entrypoints.length ? `const entryContexts = Object.create(null);
const entryModules = Object.create(null);
export function registerEntry(name, context) {
  if (lib && Object.keys(context.modules).length) throw new Error('entry imports must be registered before the library is loaded');
  Object.assign(entryModules, context.modules);
  entryContexts[name] = context;
  return context;
}
` : ''}
${classes.map(([k, spec]) => `classes[${JSON.stringify(k)}] = makeClass(${JSON.stringify({ ...spec, type: undefined })}, library, classes);\n`).join('')}`);
  write(`${name}.library.d.ts`, `// Generated by Clysm (bin/clysm library) from ${name}.wasm.
export interface Library {
  instance: WebAssembly.Instance;
  exports: Record<string, any>;
}
export declare function library(): Library;
export declare function useLibrary(module: ${embed ? 'Uint8Array | WebAssembly.Module' : 'string | URL | Uint8Array | WebAssembly.Module'}, options?: Record<string, unknown>): void;
export declare function loadLibrary(): Promise<Library>;
export declare const classes: Record<string, any>;
${entrypoints.length ? 'export declare function registerEntry(name: string, context: { url: string; modules: Record<string, unknown>; environment: Record<string, string | undefined> }): unknown;\n' : ''}
`);
  for (const entry of [null, ...entryNames]) {
    const file = entry ?? name;
    const init = entryInitializers.get(entry);
    const lines = exportsOf(entry).map(([k, v]) => {
      if (classOf(k, v)) return `export const ${k} = __clysm_classes[${JSON.stringify(classOf(k, v).name)}];`;
      if (typeof v === 'function') return `export const ${k} = (...args) => __clysm_library().exports.${k}(...args);`;
      // (an object as JSON.parse makes it: a literal would take a key __proto__ for its prototype)
      return typeof v === 'object' && v !== null
        ? `export const ${k} = __clysm_json(${JSON.stringify(JSON.stringify(v))});`
        : `export const ${k} = ${JSON.stringify(v)};`;
    });
    write(`${file}${ext}`, `${init?.shebang ? '#!/usr/bin/env node\n' : ''}// Generated by Clysm (bin/clysm library) from the exports of ${name}.wasm${entry ? ` for its entry ${entry}` : ''}.
// ${init ? 'Its initializer runs on import when its predicate holds.' : `Importing it loads nothing: ./${name}.library${ext} loads the module when it is first used.`}
import { classes as __clysm_classes, library as __clysm_library } from './${name}.library${ext}';
import { jsonConstant as __clysm_json } from './clysm/jsffi.mjs';

${lines.join('\n')}
`);
    if (init) {
      const imported = init.imports.map(([specifier], i) => `import * as __clysm_entry_module${i} from ${JSON.stringify(specifier)};`).join('\n');
      const modules = init.imports.map(([specifier, selected], i) => `[${JSON.stringify(specifier)}]: __clysm_entry_module${i}${selected === '*' ? '' : `[${JSON.stringify(selected)}]`}`).join(', ');
      const environment = init.environment.map((v) => `[${JSON.stringify(v)}]: process.env.${v}`).join(', ');
      const checks = init.checks.map(([condition, message]) => `if (!(${entryCondition(condition)})) throw new Error(${entryMessage(message)});`).join('\n');
      const load = init.loadErrors.length ? `let __clysm_loaded;
try { __clysm_loaded = __clysm_library(); } catch (error) {
  let code;
  try { code = error?.code; } catch { /* Preserve an error whose properties throw. */ }
  const mapped = ${JSON.stringify(init.loadErrors)}.find(([name]) => name === code);
  if (mapped) throw new Error(mapped[1]);
  throw error;
}` : 'const __clysm_loaded = __clysm_library();';
      const start = `const __clysm_context = __clysm_register_entry(${JSON.stringify(entry)}, { url: __clysm_url, modules: { ${modules} }, environment: { ${environment} } });
if (${entryCondition(init.condition)}) {
${checks}
${load}
__clysm_loaded.library.entrypoints.find((e) => e.entry === ${JSON.stringify(entry)}).run(__clysm_context);
}`;
      // Keep host globals in a helper's scope: an entry may export names such
      // as process, Error, or globalThis. process.env.KEY stays literal here
      // so bundlers can substitute a deployment's value at build time.
      write(`${file}.entry${ext}`, `// Generated by Clysm: initialization of ${file}${ext}.
${imported}
export function initialize(__clysm_url, __clysm_library, __clysm_register_entry) {
${init.errors === 'stderr' ? `try {\n${start}\n} catch (error) {\n  process.stderr.write(String(error instanceof Error ? error.message : error) + '\\n');\n  process.exitCode = 1;\n}` : start}
}
`);
      fs.appendFileSync(path.join(dir, `${file}${ext}`), `import { registerEntry as __clysm_register_entry } from './${name}.library${ext}';
import { initialize as __clysm_initialize } from './${file}.entry${ext}';
__clysm_initialize(import.meta.url, __clysm_library, __clysm_register_entry);
`);
      if (init.shebang) fs.chmodSync(path.join(dir, `${file}${ext}`), 0o755);
    }
    write(`${file}.d.ts`, `// Generated by Clysm (bin/clysm library) from the exports of ${name}.wasm${entry ? ` for its entry ${entry}` : ''}.
${declarationsOf(entry)}${declarationsOfExports(exportsOf(entry))}
export {};
`);
  }
} else {
  const own = exportsOf(null);
  // (create is the module's own; the helpers have names of their own)
  const taken = own.filter(([k]) => k === 'create');
  if (taken.length) throw new Error(`a library cannot export ${taken.map(([k]) => k).join(', ')} from ${name}.mjs: the module uses the name (give it an entry)`);
  write(`${name}${ext}`, `// Generated by Clysm (bin/clysm library) from the exports of ${name}.wasm.
// The module's toplevel forms run when this module is imported; create()
// makes another instance, with the options of load() in clysm/lib.mjs.
${embed ? `import { moduleOfBase64 as __clysm_module_of, ${embeddedDebug ? 'bytesOfBase64 as __clysm_bytes_of, ' : ''}loadSync as __clysm_load } from '${embeddedHost}';

const __clysm_defaults = ${embeddedOptions};
// The module, ${compressionLabel}in base64 (${kept.length} decoded bytes).
const __clysm_module = __clysm_module_of('${embedded}');
${embeddedDebug ? `${embeddedDebugText}const __clysm_options = { ...__clysm_defaults, debug: __clysm_debug_bytes };\n` : 'const __clysm_options = __clysm_defaults;\n'}const __clysm_lib = __clysm_load(__clysm_module, __clysm_options);
export const create = async (options) => __clysm_load(__clysm_module, { ...__clysm_options, ...options });` : `import { load as __clysm_load } from './clysm/lib.mjs';

${outputOptions}${locationHelper}const __clysm_url = ${locations.length ? '__clysm_module_file()' : `new URL('./${name}.wasm', import.meta.url)`};
const __clysm_lib = await __clysm_load(__clysm_url, __clysm_defaults);
export const create = (options) => __clysm_load(__clysm_url, { ...__clysm_defaults, ...options });`}
${own.length ? `export const { ${own.map(([k]) => k).join(', ')} } = __clysm_lib.exports;\n` : ''}export default __clysm_lib;
`);
  write(`${name}.d.ts`, `// Generated by Clysm (bin/clysm library) from the exports of ${name}.wasm.
export interface LoadOptions {
  args?: string[];
  stdout?: (text: string) => void;
  stderr?: (text: string) => void;
  stdin?: () => string | null;
  modules?: Record<string, unknown>;
  import?: (specifier: string) => Promise<unknown>;
  require?: (specifier: string) => unknown;
}
${declarationsOf(null)}${unnamedClasses(own)}export interface Exports {
${entries.map(([k, f]) => `  ${identifier.test(k) ? k : JSON.stringify(k)}: ${entryOf(k) !== null && identifier.test(k) ? `typeof import('./${entryOf(k)}.js').${k}` : classOf(k, f) && !info.exports[k]?.type ? `typeof ${localClassName(classOf(k, f), own)}` : typeOf(k, f)};`).join('\n')}
}
export interface Library {
  instance: WebAssembly.Instance;
  exports: Exports;
  eval(source: string): string;
}
${declarationsOfExports(own)}
export declare function create(options?: LoadOptions): Promise<Library>;
declare const lib: Library;
export default lib;
`);
  for (const entry of entryNames) {
    const mine = exportsOf(entry);
    write(`${entry}${ext}`, `// Generated by Clysm (bin/clysm library) from the exports of ${name}.wasm for its entry ${entry}.
import __clysm_lib from './${name}${ext}';

export const { ${mine.map(([k]) => k).join(', ')} } = __clysm_lib.exports;
`);
    write(`${entry}.d.ts`, `// Generated by Clysm (bin/clysm library) from the exports of ${name}.wasm for its entry ${entry}.
${declarationsOf(entry)}${declarationsOfExports(mine)}
`);
  }
}

const hostDir = fileURLToPath(new URL('../host/', import.meta.url));
fs.mkdirSync(path.join(dir, 'clysm'), { recursive: true });
for (const r of reexports) {
  const clause = `{ ${r.names.join(', ')} } from ${JSON.stringify(r.from)};\n`;
  if (!r.typeOnly) fs.appendFileSync(path.join(dir, `${r.entry}${ext}`), `export ${clause}`);
  fs.appendFileSync(path.join(dir, `${r.entry}.d.ts`), `export ${r.typeOnly ? 'type ' : ''}${clause}`);
}

// (an embedded library's host is of the web alone: no module of Node.js is imported)
const hostFiles = embed ? ['common.mjs', 'jsffi.mjs', 'web/lib.mjs'] : ['common.mjs', 'jsffi.mjs', 'lib.mjs', 'worker.mjs', 'process.mjs'];
if (compression === 'gzip') hostFiles.push('web/compressed.mjs', 'web/vendor/fflate.mjs', 'web/vendor/fflate.LICENSE', 'web/vendor/README.md');
if (compression === 'xz') hostFiles.push('web/xz.mjs', 'web/vendor/xzwasm.mjs', 'web/vendor/xzwasm.LICENSE', 'web/vendor/xzwasm.README.md');
for (const f of hostFiles) {
  fs.mkdirSync(path.dirname(path.join(dir, 'clysm', f)), { recursive: true });
  if (fs.existsSync(path.join(hostDir, f))) fs.copyFileSync(path.join(hostDir, f), path.join(dir, 'clysm', f));
}
const others = files.filter((f) => f !== `${name}.mjs` && f !== `${name}.d.ts`);
console.log(`; wrote ${path.join(dir, name)}.{wasm,wasm.debug,mjs,d.ts}${others.length ? ` and ${others.join(', ')}` : ''}: ${entries.length} exports`);
