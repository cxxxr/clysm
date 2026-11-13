// A Lisp REPL in a page: the terminal of host/web/index.html, and the panel
// that the development build of an application docks at the bottom of its
// window (dockRepl).
//
//   const repl = createRepl(container);
//   const lisp = await loadClysm(bytes, repl.write, { readLine: repl.readLine });
//   repl.start(lisp);
//
// The image must contain lib/web-main.lisp (the global clysmRepl).

const CSS = `
.clysm-repl { --r-bg: #ffffff; --r-fg: #1d1d1f; --r-muted: #6b6b70; --r-accent: #2f5d8a;
  --r-line: #e3e1db; --r-value: #1f6b4f; --r-error: #b3261e;
  box-sizing: border-box; overflow: auto; background: var(--r-bg); color: var(--r-fg);
  border: 1px solid var(--r-line); border-radius: 8px; padding: 12px 14px; cursor: text;
  font: 14px/1.45 ui-monospace, SFMono-Regular, Menlo, Consolas, monospace; text-align: left; }
@media (prefers-color-scheme: dark) {
  .clysm-repl { --r-bg: #1f2023; --r-fg: #e8e6e1; --r-muted: #9a9a9f; --r-accent: #7fb0e0;
    --r-line: #33343a; --r-value: #8fd1a8; --r-error: #f2948a; }
}
.clysm-repl .r-log { white-space: pre-wrap; overflow-wrap: anywhere; }
.clysm-repl .in { color: var(--r-accent); }
.clysm-repl .val { color: var(--r-value); }
.clysm-repl .err { color: var(--r-error); }
.clysm-repl .info { color: var(--r-muted); }
.clysm-repl .r-entry { display: flex; }
.clysm-repl .r-prompt { color: var(--r-accent); white-space: pre; user-select: none; }
.clysm-repl textarea { flex: 1; border: 0; outline: 0; padding: 0; margin: 0; resize: none;
  overflow: hidden; background: transparent; color: var(--r-fg); font: inherit; line-height: inherit; }
.clysm-repl textarea:disabled { opacity: .5; }
.clysm-repl-dock { position: fixed; left: 0; right: 0; bottom: 0; height: 38vh; z-index: 1000;
  border-radius: 0; border-width: 1px 0 0; box-shadow: 0 -4px 16px rgba(0,0,0,.12); }
.clysm-repl-toggle { position: fixed; right: 12px; bottom: 12px; z-index: 1001;
  font: 12px ui-monospace, monospace; padding: 3px 10px; border-radius: 6px; cursor: pointer;
  border: 1px solid #8884; background: #8882; color: inherit; }
`;

let styled = false;
function addStyle() {
  if (styled) return;
  styled = true;
  const style = document.createElement('style');
  style.textContent = CSS;
  document.head.append(style);
}

// Make CONTAINER a REPL terminal.  OPTIONS: afterEval, called after every
// evaluation; historyKey, the localStorage key of the input history.
export function createRepl(container, { afterEval, historyKey = 'clysm-repl-history' } = {}) {
  addStyle();
  container.classList.add('clysm-repl');
  const log = document.createElement('div');
  log.className = 'r-log';
  const entry = document.createElement('div');
  entry.className = 'r-entry';
  const promptEl = document.createElement('span');
  promptEl.className = 'r-prompt';
  const src = document.createElement('textarea');
  Object.assign(src, { rows: 1, spellcheck: false, disabled: true, autocomplete: 'off' });
  src.setAttribute('autocapitalize', 'off');
  src.setAttribute('aria-label', 'Lisp input');
  entry.append(promptEl, src);
  container.append(log, entry);

  let lisp = null;
  let lastChar = '\n';   // the last character written to the log
  let pending = '';      // output since the last newline (a prompt for READ-LINE)
  const add = (text, cls) => {
    if (!text) return;
    const s = document.createElement('span');
    if (cls) s.className = cls;
    s.textContent = text;
    log.append(s);
    lastChar = text[text.length - 1];
    const nl = text.lastIndexOf('\n');
    pending = nl < 0 ? pending + text : text.slice(nl + 1);
  };
  const freshLine = () => { if (lastChar !== '\n') add('\n'); };
  const scroll = () => { container.scrollTop = container.scrollHeight; };

  // input history, kept in localStorage
  let history = [];
  try { history = JSON.parse(localStorage.getItem(historyKey)) || []; } catch {}
  let hpos = history.length, draft = '';
  const remember = (code) => {
    if (history[history.length - 1] !== code) history.push(code);
    history = history.slice(-500);
    hpos = history.length;
    try { localStorage.setItem(historyKey, JSON.stringify(history)); } catch {}
  };

  const resize = () => { src.rows = Math.max(1, src.value.split('\n').length); };
  const setInput = (text) => { src.value = text; resize(); src.setSelectionRange(text.length, text.length); };
  const prompt = () => `${globalThis.clysmRepl.packageName()}> `;
  const clear = () => { log.textContent = ''; lastChar = '\n'; pending = ''; src.focus(); };

  const showResult = (result) => {
    // the values, one per line, possibly followed by "; Error: ..."
    const m = /(^|\n); Error: /.exec(result);
    const at = m ? m.index + m[1].length : result.length;
    add(result.slice(0, at), 'val');
    add(result.slice(at), 'err');
  };
  const run = async (code) => {
    const p = promptEl.textContent;
    freshLine();
    add(p + code.replace(/\n/g, '\n' + ' '.repeat(p.length)) + '\n', 'in');
    setInput('');
    if (code.trim()) remember(code);
    src.disabled = true;
    scroll();
    // let the page paint before a long computation
    await new Promise((r) => requestAnimationFrame(() => setTimeout(r, 0)));
    const bsp = lisp.instance.exports.bsp.value;
    try {
      const result = lisp.eval(code);
      freshLine();
      showResult(result);
    } catch (e) {
      freshLine();
      add(e instanceof RangeError ? '; Error: control stack exhausted\n'
                                  : `; Internal error: ${e.message}\n`, 'err');
      // the exception skipped the code that restores special variables
      globalThis.clysmRepl.unbindTo(bsp);
    }
    if (afterEval) afterEval();
    promptEl.textContent = prompt();
    src.disabled = false;
    src.focus();
    scroll();
  };

  src.addEventListener('input', resize);
  src.addEventListener('keydown', (e) => {
    if (e.isComposing) return;
    e.stopPropagation();   // not a key of the application around the REPL
    const before = src.value.slice(0, src.selectionStart);
    const after = src.value.slice(src.selectionEnd);
    if (e.key === 'Enter' && !e.shiftKey) {
      // Enter evaluates a complete input and continues an incomplete one;
      // Ctrl+Enter always evaluates
      if (e.ctrlKey || e.metaKey || !after.trim() && globalThis.clysmRepl.complete(src.value)) {
        e.preventDefault();
        if (src.value.trim() || e.ctrlKey || e.metaKey) run(src.value);
      }
    } else if (e.key === 'ArrowUp' && !before.includes('\n') && hpos > 0) {
      e.preventDefault();
      if (hpos === history.length) draft = src.value;
      setInput(history[--hpos]);
    } else if (e.key === 'ArrowDown' && !after.includes('\n') && hpos < history.length) {
      e.preventDefault();
      hpos++;
      setInput(hpos === history.length ? draft : history[hpos]);
    } else if (e.key === 'l' && e.ctrlKey) {
      e.preventDefault();
      clear();
    } else if (e.key === 'Tab' && !e.shiftKey) {
      e.preventDefault();
      document.execCommand('insertText', false, '  ');
    }
  });
  // clicking the terminal focuses the input, unless text is being selected
  container.addEventListener('click', () => { if (!getSelection().toString()) src.focus(); });

  return {
    // output of the Lisp program (the onOutput argument of loadClysm)
    write: (text, fd) => { add(text, fd === 2 ? 'err' : undefined); scroll(); },
    // standard input (READ-LINE, READ...): ask with a dialog; Cancel is end of file
    readLine: () => window.prompt(pending.trim() || 'Standard input'),
    info: (text) => add(text, 'info'),
    error: (text) => add(text, 'err'),
    clear,
    start(image, { focus = true } = {}) {
      lisp = image;
      promptEl.textContent = prompt();
      src.disabled = false;
      if (focus) src.focus();
    },
  };
}

// Dock a REPL at the bottom of the page (for the development build of an
// application); a button hides and shows it.
export function dockRepl(options) {
  const panel = document.createElement('div');
  const repl = createRepl(panel, options);
  panel.classList.add('clysm-repl-dock');
  const toggle = document.createElement('button');
  toggle.className = 'clysm-repl-toggle';
  const padding = document.body.style.paddingBottom;
  const show = (on) => {
    panel.hidden = !on;
    toggle.textContent = on ? 'REPL ▾' : 'REPL ▴';
    document.body.style.paddingBottom = on ? '38vh' : padding;
  };
  toggle.addEventListener('click', () => show(panel.hidden));
  document.body.append(panel, toggle);
  show(true);
  return repl;
}
