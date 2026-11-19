// tests/lem/scenario.js — a scenario of a person using Lem on Clysm (cxxxr/lem's
// frontends/clysm/), run as the Electron shell's test module
// (CLYSM_ELECTRON_TEST) by tools/lem-scenario.sh.  It types into the
// desktop application with sendInputEvent and measures, for each key, how
// long the page takes to show its effect: from the keydown (as the page
// receives it) to the last change of the page's DOM before it is quiet
// again (LEM_QUIET ms, by default 50).
//
// Steps: start-up to the first frame; open a large Lisp file; scroll it
// page by page; type 200 characters of Lisp (Return indents); an
// incremental search; M-x commands typed with their completion.
//
// The results (a JSON object) go to LEM_SCENARIO_OUT/result.json and a
// table to the standard output.  With LEM_PROFILE set, each step also
// records a CPU profile of the page (STEP.cpuprofile, the DevTools
// protocol's Profiler) and the whole run a trace (trace.json: the page's
// main thread, V8's GC and compilation), which tools/lem-profile.mjs
// summarizes.
const fs = require('node:fs');
const path = require('node:path');

const out = process.env.LEM_SCENARIO_OUT;
const file = process.env.LEM_SCENARIO_FILE;     // the Lisp file opened
const quiet = Number(process.env.LEM_QUIET || 50);
const profiling = !!process.env.LEM_PROFILE;
const sleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms));

// what the page runs to time a key: the keydown, and the last mutation of
// the page after it
const probe = `(() => {
  if (window.__lemProbe) return;
  const P = window.__lemProbe = { t0: 0, last: 0, n: 0 };
  addEventListener('keydown', () => { P.t0 = performance.now(); P.last = 0; P.n = 0; }, true);
  new MutationObserver(() => { P.last = performance.now(); P.n++; })
    .observe(document.body, { subtree: true, childList: true, characterData: true, attributes: true });
  // resolves when the page has not changed for QUIET ms after a change
  // (or after MAX ms without one)
  P.settle = (quiet, max) => new Promise((resolve) => {
    const start = performance.now();
    const tick = () => {
      const now = performance.now();
      if ((P.last && now - P.last >= quiet) || now - start > max)
        resolve({ latency: P.last ? P.last - P.t0 : null, mutations: P.n });
      else setTimeout(tick, 4);
    };
    tick();
  });
})()`;

module.exports = (win, app) => {
  const t0 = Number(process.env.CLYSM_STARTUP_T0) || Date.now() - process.uptime() * 1000;
  const wc = win.webContents;
  win.setContentSize(1200, 900);
  const js = (code) => wc.executeJavaScript(code);
  const lisp = async (form) => (await js(`lisp.eval(${JSON.stringify(form)})`)).replace(/\n$/, '');
  const errors = [];
  wc.on('console-message', (event) => {
    if ((event.level === 'error' || event.level === 3) && !/^\[(INFO|WARN)\]/.test(event.message))
      errors.push(event.message);
  });
  const result = { steps: [], startup: {} };
  let dbg = null;

  // one key: sent, then the page settles; its latency in ms (or null if the
  // page did not change)
  let current = null;
  // (MAX: how long to wait for a change, for a key that may make none: a
  // prefix key)
  const key = async (keyCode, modifiers = [], max = 3000) => {
    wc.sendInputEvent({ type: 'keyDown', keyCode, modifiers });
    if (keyCode.length === 1 && modifiers.every((m) => m === 'shift'))
      wc.sendInputEvent({ type: 'char', keyCode, modifiers });
    wc.sendInputEvent({ type: 'keyUp', keyCode, modifiers });
    const r = await js(`__lemProbe.settle(${quiet}, ${max})`);
    if (current) current.keys.push(r.latency);
    return r.latency;
  };
  const type = async (text) => { for (const c of text) await key(c === '\n' ? 'Return' : c); };

  const step = async (name, body) => {
    current = { name, keys: [] };
    if (dbg) await dbg.sendCommand('Profiler.start');
    await js(`performance.mark('step:${name}')`);
    const start = Date.now();
    // (a body that returns a number gives the latency of the step's one
    // action itself)
    const latency = await body();
    if (typeof latency === 'number') current.keys = [latency];
    current.wall = Date.now() - start;
    await js(`performance.mark('end:${name}')`);
    if (dbg) {
      const { profile } = await dbg.sendCommand('Profiler.stop');
      fs.writeFileSync(path.join(out, `${name}.cpuprofile`), JSON.stringify(profile));
    }
    const ks = current.keys.filter((x) => x !== null);
    current.count = current.keys.length;
    current.mean = ks.length ? ks.reduce((a, b) => a + b, 0) / ks.length : null;
    current.max = ks.length ? Math.max(...ks) : null;
    current.median = ks.length ? [...ks].sort((a, b) => a - b)[Math.floor(ks.length / 2)] : null;
    result.steps.push(current);
    current = null;
    await sleep(300);
  };

  const finish = async (status) => {
    if (dbg) {
      // the trace, as JSON, from a stream of the DevTools protocol
      const complete = new Promise((resolve) => dbg.on('message', (event, method, params) => {
        if (method === 'Tracing.tracingComplete') resolve(params.stream);
      }));
      await dbg.sendCommand('Tracing.end');
      const stream = await complete;
      const fd = fs.openSync(path.join(out, 'trace.json'), 'w');
      for (;;) {
        const { data, eof, base64Encoded } = await dbg.sendCommand('IO.read', { handle: stream, size: 1 << 20 });
        fs.writeSync(fd, base64Encoded ? Buffer.from(data, 'base64') : data);
        if (eof) break;
      }
      fs.closeSync(fd);
      await dbg.sendCommand('IO.close', { handle: stream });
    }
    result.errors = errors;
    fs.writeFileSync(path.join(out, 'result.json'), JSON.stringify(result, null, 1));
    const f = (x) => (x === null || x === undefined ? '-' : x.toFixed(1));
    console.log(`startup: editor shown at ${result.startup.shown} ms (page loaded at ${result.startup.loaded} ms)`);
    console.log('step                 keys   mean ms  median     max   wall ms');
    for (const s of result.steps)
      console.log(`${s.name.padEnd(20)} ${String(s.count).padStart(4)} ${f(s.mean).padStart(9)} ${f(s.median).padStart(7)} ${f(s.max).padStart(7)} ${String(s.wall).padStart(9)}`);
    if (errors.length) console.log(`errors:\n${errors.slice(0, 10).join('\n')}`);
    console.log(status);
    app.exit(0);
  };


  wc.once('did-finish-load', async () => {
    try {
      result.startup.loaded = Date.now() - t0;
      let shown = false;
      for (let i = 0; i < 6000 && !shown; i++) {
        shown = await js("document.querySelector('.lem-line') !== null");
        if (!shown) await sleep(5);
      }
      result.startup.shown = Date.now() - t0;
      if (!shown) return finish('lem-scenario: FAIL (the editor was not shown)');
      await js(probe);
      if (profiling) {
        dbg = wc.debugger;
        dbg.attach('1.3');
        await dbg.sendCommand('Profiler.enable');
        await dbg.sendCommand('Profiler.setSamplingInterval', { interval: 100 });
        await dbg.sendCommand('Tracing.start', {
          transferMode: 'ReturnAsStream', streamFormat: 'json',
          traceConfig: { includedCategories: ['devtools.timeline', 'disabled-by-default-devtools.timeline',
                                              'v8', 'disabled-by-default-v8.gc', 'v8.wasm', 'blink.user_timing'] },
        });
      }
      await sleep(1000);

      // open the file: C-x C-f, its name, Return (the first Lisp file also
      // loads the Lisp mode)
      await key('x', ['control'], 300); await key('f', ['control']);
      await key('a', ['control']); await key('k', ['control']);
      await type(file);
      await step('open-file', async () => {
        // (the page draws while the Lisp mode loads: its latency is until the
        // file is shown highlighted)
        const start = Date.now();
        await key('Return');
        // until it is highlighted
        for (let i = 0; i < 300; i++) {
          if (await js("[...document.querySelectorAll('.lem-line span')].some((s) => s.innerText === 'defun' && s.style.color)")) break;
          await sleep(20);
        }
        return Date.now() - start;
      });
      const name = await lisp('(lem:buffer-name (lem:current-buffer))');
      if (name !== `"${path.basename(file)}"`) return finish(`lem-scenario: FAIL (opened ${name})`);

      await step('scroll-down', async () => { for (let i = 0; i < 40; i++) await key('v', ['control']); });
      await step('scroll-up', async () => { for (let i = 0; i < 20; i++) await key('v', ['alt']); });
      await step('next-line', async () => { for (let i = 0; i < 60; i++) await key('n', ['control']); });

      // 200 characters of Lisp, on a new line in the middle of the file
      await key('a', ['control']); await key('Return'); await key('p', ['control']);
      const code = '(defun scenario-sum (list &key (key #\'identity))\n'
            + '(let ((total 0))\n(dolist (x list total)\n(when (numberp (funcall key x))\n'
            + '(incf total (funcall key x))))))\n(scenario-sum (list 1 2 3) :key #\'1+)\n'
            + '(print (scenario-sum \'(4 5 6)))\n';
      const text = code.slice(0, 200);
      result.typedAtLine = Number(await lisp('(lem:line-number-at-point (lem:current-point))'));
      await step('type-lisp', async () => { await type(text); });
      const typed = await lisp('(let ((p (lem:copy-point (lem:current-point) :temporary))) (lem:line-start p) (lem:points-to-string (lem:character-offset (lem:copy-point p :temporary) -200) p))');
      result.typed = typed;
      await step('delete-backward', async () => { for (let i = 0; i < 40; i++) await key('Backspace'); });

      // C-s: an incremental search, then its next matches
      await key('<', ['alt', 'shift']);
      await step('isearch', async () => {
        await key('s', ['control']);
        await type('make-');
        for (let i = 0; i < 15; i++) await key('s', ['control']);
        await key('Return');
      });

      // M-x, a command typed with its completion
      await step('M-x', async () => {
        for (const command of ['list-buffers', 'describe-mode']) {
          await key('x', ['alt']);
          await type(command);
          await key('Return');
          await key('g', ['control'], 300);
          await key('x', ['control'], 300); await key('1', [], 300);    // the other window closed
        }
      });
      await step('buffer-end', async () => { await key('>', ['alt', 'shift']); await key('<', ['alt', 'shift']); });
      await finish('lem-scenario: ok');
    } catch (e) {
      await finish(`lem-scenario: FAIL ${e && e.stack}`);
    }
  });
};
