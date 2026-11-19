// tools/lem-profile.mjs — summarize what LEM_PROFILE=1 tools/lem-scenario.sh
// recorded:
//   node tools/lem-profile.mjs OUT-DIR [TOP]
// For each step's CPU profile (STEP.cpuprofile): its time by kind (Lisp
// code compiled to WebAssembly, the JavaScript of the host's FFI, the
// browser's functions that JavaScript calls (the DOM), GC, the rest of the
// browser (style, layout, paint: "(program)")), and the functions with the
// most time of their own and in all (with what they call).  From the trace
// (trace.json), each step's main-thread time in GC pauses, style, layout,
// paint, and compiling WebAssembly.
import fs from 'node:fs';
import path from 'node:path';

const dir = process.argv[2];
const top = Number(process.argv[3] || 25);
const result = JSON.parse(fs.readFileSync(path.join(dir, 'result.json'), 'utf8'));

function kind(frame) {
  const { functionName: name, url } = frame;
  if (name === '(garbage collector)') return 'gc';
  if (name === '(program)') return 'browser';
  if (name === '(idle)') return 'idle';
  if (name === '(root)') return 'root';
  if (url.startsWith('wasm://')) return 'lisp';
  if (/(jsffi|common|clysm|link)\.mjs/.test(url)) return 'ffi-js';
  if (url === '') return 'dom/builtin';
  return 'js';
}

function profileSummary(profile) {
  const nodes = new Map(profile.nodes.map((n) => [n.id, n]));
  for (const n of profile.nodes) for (const c of n.children || []) nodes.get(c).parent = n.id;
  const self = new Map();
  const total = new Map();
  const kinds = {};
  let sum = 0;
  for (let i = 0; i < profile.samples.length; i++) {
    const dt = (profile.timeDeltas[i + 1] ?? 0) / 1000;
    let n = nodes.get(profile.samples[i]);
    const k = kind(n.callFrame);
    kinds[k] = (kinds[k] || 0) + dt;
    sum += dt;
    const label = (f) => `${f.functionName || '(anonymous)'} [${kind(f)}]`;
    const me = label(n.callFrame);
    self.set(me, (self.get(me) || 0) + dt);
    const seen = new Set();
    for (; n; n = nodes.get(n.parent)) {
      const l = label(n.callFrame);
      if (seen.has(l)) continue;
      seen.add(l);
      total.set(l, (total.get(l) || 0) + dt);
    }
  }
  return { self, total, kinds, sum };
}

const ms = (x) => x.toFixed(1).padStart(8);
const busy = (kinds) => Object.entries(kinds).filter(([k]) => k !== 'idle' && k !== 'root').reduce((a, [, v]) => a + v, 0);

// the trace: events of the page's main thread by name, in each step
let trace = null;
const traceFile = path.join(dir, 'trace.json');
if (fs.existsSync(traceFile)) {
  const events = JSON.parse(fs.readFileSync(traceFile, 'utf8'));
  const list = Array.isArray(events) ? events : events.traceEvents;
  const main = list.find((e) => e.ph === 'M' && e.name === 'thread_name' && e.args.name === 'CrRendererMain'
                         && list.some((m) => m.name === 'step:open-file' && m.pid === e.pid));
  const marks = {};
  for (const e of list)
    if (main && e.pid === main.pid && /^(step|end):/.test(e.name)) marks[e.name] = e.ts;
  trace = { list: main ? list.filter((e) => e.pid === main.pid && e.tid === main.tid) : [], marks };
}
const traceGroups = {
  'gc (main thread)': /^(MinorGC|MajorGC|V8\.GC_(SCAVENGER|MARK_COMPACTOR|MC_|MINOR)|BlinkGC\.AtomicPhase)/,
  'style': /^(UpdateLayoutTree|RecalculateStyles|ParseAuthorStyleSheet)$/,
  'layout': /^(Layout|UpdateLayout)$/,
  'paint': /^(Paint|PrePaint|Layerize|UpdateLayer|CompositeLayers|Commit)$/,
  'wasm compile': /^(v8\.wasm\.(compile|lazyCompile|compileLazy|.*Tier)|wasm\.(Compile|Lazy))/,
  'event dispatch': /^EventDispatch$/,
};
function traceStep(name) {
  const from = trace.marks[`step:${name}`];
  const to = trace.marks[`end:${name}`];
  if (from === undefined || to === undefined) return null;
  const sums = {};
  const names = {};
  const open = [];
  for (const e of trace.list) {
    let dur;
    if (e.ph === 'X') { if (e.ts < from || e.ts > to) continue; dur = (e.dur || 0) / 1000; }
    else if (e.ph === 'B') { open.push(e); continue; }
    else if (e.ph === 'E') {
      const b = open.pop();
      if (!b || b.ts < from || b.ts > to) continue;
      dur = (e.ts - b.ts) / 1000; e = { ...b };
    } else continue;
    names[e.name] = (names[e.name] || 0) + dur;
    for (const [group, re] of Object.entries(traceGroups))
      if (re.test(e.name)) sums[group] = (sums[group] || 0) + dur;
  }
  return { wall: (to - from) / 1000, sums, names };
}

for (const s of result.steps) {
  const file = path.join(dir, `${s.name}.cpuprofile`);
  if (!fs.existsSync(file)) continue;
  const p = profileSummary(JSON.parse(fs.readFileSync(file, 'utf8')));
  const b = busy(p.kinds);
  console.log(`\n== ${s.name}: ${s.count} keys, mean ${s.mean?.toFixed(1)} ms, max ${s.max?.toFixed(1)} ms; `
              + `profile ${p.sum.toFixed(0)} ms, busy ${b.toFixed(0)} ms`);
  console.log('  by kind (ms, % of busy):',
              Object.entries(p.kinds).filter(([k]) => k !== 'idle' && k !== 'root').sort((x, y) => y[1] - x[1])
                .map(([k, v]) => `${k} ${v.toFixed(0)} (${(100 * v / b).toFixed(0)}%)`).join(', '));
  if (trace) {
    const t = traceStep(s.name);
    if (t) console.log('  trace, main thread (ms):', Object.entries(t.sums).map(([k, v]) => `${k} ${v.toFixed(1)}`).join(', '));
  }
  console.log('  self:');
  for (const [name, v] of [...p.self].filter(([n]) => !/^\((idle|root)\)/.test(n)).sort((x, y) => y[1] - x[1]).slice(0, top))
    console.log(`  ${ms(v)} ${(100 * v / b).toFixed(1).padStart(5)}%  ${name}`);
  console.log('  total (with callees):');
  for (const [name, v] of [...p.total].filter(([n]) => !/^\((idle|root|program|garbage)/.test(n)).sort((x, y) => y[1] - x[1]).slice(0, top))
    console.log(`  ${ms(v)} ${(100 * v / b).toFixed(1).padStart(5)}%  ${name}`);
}
if (process.env.LEM_TRACE_NAMES && trace) {
  for (const s of result.steps) {
    const t = traceStep(s.name);
    if (!t) continue;
    console.log(`\n== ${s.name}: trace events by name`);
    for (const [n, v] of Object.entries(t.names).sort((x, y) => y[1] - x[1]).slice(0, 40)) console.log(`  ${ms(v)}  ${n}`);
  }
}

// LEM_CALLERS=NAME,...: in each step, where the functions whose names start
// with NAME are called from (their time with callees, by the chain of the
// 4 callers above)
if (process.env.LEM_CALLERS) {
  for (const s of result.steps) {
    const file = path.join(dir, `${s.name}.cpuprofile`);
    if (!fs.existsSync(file)) continue;
    const profile = JSON.parse(fs.readFileSync(file, 'utf8'));
    const nodes = new Map(profile.nodes.map((n) => [n.id, n]));
    for (const n of profile.nodes) for (const c of n.children || []) nodes.get(c).parent = n.id;
    for (const want of process.env.LEM_CALLERS.split(',')) {
      const chains = new Map();
      let sum = 0;
      for (let i = 0; i < profile.samples.length; i++) {
        const dt = (profile.timeDeltas[i + 1] ?? 0) / 1000;
        let n = nodes.get(profile.samples[i]);
        while (n && !n.callFrame.functionName.startsWith(want)) n = nodes.get(n.parent);
        if (!n) continue;
        const chain = [];
        for (let p = nodes.get(n.parent); p && chain.length < Number(process.env.LEM_DEPTH || 4); p = nodes.get(p.parent)) chain.push(p.callFrame.functionName);
        const k = chain.join(' < ');
        chains.set(k, (chains.get(k) || 0) + dt);
        sum += dt;
      }
      if (!sum) continue;
      console.log(`\n== ${s.name}: ${want} ${sum.toFixed(1)} ms (with callees), called from`);
      for (const [k, v] of [...chains].sort((a, b) => b[1] - a[1]).slice(0, 8)) console.log(`  ${ms(v)}  ${k}`);
    }
  }
}
