// site/playground.mjs — the page of the playground (site/index.html): the
// editor, the samples, the output and the REPL.  The Lisp image runs in a
// worker (site/worker.mjs); the page compiles its module once and gives it to
// each worker it starts, so Stop (ending the worker) and a new image are quick.

import { samples } from './samples.mjs';

const $ = (id) => document.getElementById(id);
const code = $('code'), highlight = $('highlight'), out = $('out'), repl = $('repl');
const runButton = $('run'), stopButton = $('stop'), status = $('status'), promptLabel = $('prompt');

// ---------------------------------------------------------------- highlighting

const SPECIAL = new Set(`defun defmacro defvar defparameter defconstant defclass defmethod defgeneric
  defstruct define-condition deftype defpackage in-package lambda let let* flet labels macrolet
  symbol-macrolet if when unless cond case ecase typecase etypecase and or not progn prog1 block
  return return-from loop do do* dolist dotimes tagbody go setq setf incf decf push pop
  multiple-value-bind multiple-value-prog1 destructuring-bind handler-case handler-bind restart-case
  unwind-protect catch throw quote function declare the eval-when with-output-to-string
  with-input-from-string with-open-file with-slots print-unreadable-object locally`.split(/\s+/));
const DEFINING = new Set(['defun', 'defmacro', 'defvar', 'defparameter', 'defconstant', 'defclass',
  'defmethod', 'defgeneric', 'defstruct', 'define-condition', 'deftype', 'defpackage']);
const DELIMITER = /[\s()'"`,;]/;

const escapeHtml = (s) => s.replace(/[&<>]/g, (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;' })[c]);

// the source as HTML, with a span of a class for each kind of token
function highlightLisp(src) {
  let html = '', i = 0, previous = '';
  const span = (cls, text) => { html += `<span class="t-${cls}">${escapeHtml(text)}</span>`; };
  while (i < src.length) {
    const c = src[i];
    let j = i + 1;
    if (c === ';') {
      while (j < src.length && src[j] !== '\n') j++;
      span('comment', src.slice(i, j));
    } else if (c === '#' && src[j] === '|') {
      const end = src.indexOf('|#', j + 1);
      j = end < 0 ? src.length : end + 2;
      span('comment', src.slice(i, j));
    } else if (c === '"') {
      while (j < src.length && src[j] !== '"') j += src[j] === '\\' ? 2 : 1;
      j = Math.min(j + 1, src.length);
      span('string', src.slice(i, j));
    } else if (c === '#' && src[j] === '\\') {
      j += 2;
      while (j < src.length && !DELIMITER.test(src[j])) j++;
      span('string', src.slice(i, j));
    } else if (c === '(' || c === ')') {
      span('paren', c);
    } else if (c === "'" || c === '`' || c === ',' || (c === '#' && src[j] === "'")) {
      if ((c === ',' && src[j] === '@') || c === '#') j++;
      span('quote', src.slice(i, j));
    } else if (/\s/.test(c)) {
      while (j < src.length && /\s/.test(src[j])) j++;
      html += src.slice(i, j);
    } else {
      while (j < src.length && !DELIMITER.test(src[j])) j++;
      const word = src.slice(i, j), lower = word.toLowerCase();
      if (DEFINING.has(previous)) span('name', word);
      else if (word[0] === ':' || word.startsWith('&')) span('keyword', word);
      else if (/^[-+]?(\d+\.?\d*([eEdD][-+]?\d+)?|\d*\.\d+([eEdD][-+]?\d+)?|\d+\/\d+)$/.test(word)
               || /^#[cC]$/.test(word)) span('number', word);
      else if (SPECIAL.has(lower) && src[i - 1] === '(') span('special', word);
      else html += escapeHtml(word);
      previous = lower;
      i = j;
      continue;
    }
    if (!/\s/.test(c)) previous = '';
    i = j;
  }
  // (a last newline would not take room in the pre without something after it)
  return html + '\n';
}

const rehighlight = () => {
  highlight.innerHTML = highlightLisp(code.value);
  highlight.scrollTop = code.scrollTop;
  highlight.scrollLeft = code.scrollLeft;
};
code.addEventListener('input', rehighlight);
code.addEventListener('scroll', () => {
  highlight.scrollTop = code.scrollTop;
  highlight.scrollLeft = code.scrollLeft;
});

// ---------------------------------------------------------------- reading

// The scan of SRC up to its end: the positions of the parentheses still
// open, and whether the end is inside a string or a block comment.
function scan(src) {
  const open = [];
  let inString = false, inComment = false;
  for (let i = 0; i < src.length; i++) {
    const c = src[i];
    if (inString) { if (c === '\\') i++; else if (c === '"') inString = false; continue; }
    if (inComment) { if (c === '|' && src[i + 1] === '#') { inComment = false; i++; } continue; }
    if (c === ';') { while (i < src.length && src[i] !== '\n') i++; }
    else if (c === '"') inString = true;
    else if (c === '#' && src[i + 1] === '|') { inComment = true; i++; }
    else if (c === '#' && src[i + 1] === '\\') i += 2;
    else if (c === '(') open.push(i);
    else if (c === ')') open.pop();
  }
  return { open, inString, inComment };
}

const complete = (src) => {
  const { open, inString, inComment } = scan(src);
  return open.length === 0 && !inString && !inComment;
};

// The indentation of a new line after BEFORE: under the first argument of
// the innermost open list, or two columns in for a form whose operator is a
// symbol (as Lisp editors do for bodies), or one column in for data.
function indentation(before) {
  const { open, inString } = scan(before);
  if (inString || open.length === 0) return '';
  const at = open[open.length - 1];
  const column = at - (before.lastIndexOf('\n', at) + 1);
  const rest = before.slice(at + 1);
  const operator = /^[^\s()'"`,;]+/.exec(rest);
  if (!operator) return ' '.repeat(column + 1);
  const op = operator[0].toLowerCase();
  const firstArg = /^[^\s()'"`,;]+[ \t]+(?=\S)/.exec(rest);
  if (SPECIAL.has(op) || op.startsWith('def') || op.startsWith('with-') || !firstArg || rest.includes('\n'))
    return ' '.repeat(column + 2);
  return ' '.repeat(column + 1 + firstArg[0].length);
}

const insert = (textarea, text) => {
  // (execCommand keeps the textarea's undo history)
  textarea.focus();
  if (!document.execCommand('insertText', false, text)) {
    textarea.setRangeText(text, textarea.selectionStart, textarea.selectionEnd, 'end');
    textarea.dispatchEvent(new Event('input'));
  }
};

code.addEventListener('keydown', (e) => {
  if (e.isComposing) return;
  if (e.key === 'Enter' && (e.ctrlKey || e.metaKey)) {
    e.preventDefault();
    runProgram();
  } else if (e.key === 'Enter' && !e.shiftKey) {
    e.preventDefault();
    insert(code, '\n' + indentation(code.value.slice(0, code.selectionStart)));
  } else if (e.key === 'Tab' && !e.shiftKey && !e.ctrlKey && !e.metaKey && !e.altKey) {
    e.preventDefault();
    insert(code, '  ');
  }
});

// ---------------------------------------------------------------- output

const scrollOut = () => { out.scrollTop = out.scrollHeight; };

function newEntry(head, cls) {
  const entry = document.createElement('div');
  entry.className = 'entry';
  if (head) {
    const h = document.createElement('div');
    h.className = `head ${cls || ''}`;
    h.textContent = head;
    entry.append(h);
  }
  out.append(entry);
  scrollOut();
  return entry;
}

function addText(entry, text, cls) {
  if (!text) return;
  const last = entry.lastElementChild;
  if (last && last.className === cls && last.tagName === 'SPAN') last.textContent += text;
  else {
    const s = document.createElement('span');
    s.className = cls;
    s.textContent = text;
    entry.append(s);
  }
  scrollOut();
}

function addLine(entry, text, cls) {
  const d = document.createElement('div');
  d.className = cls;
  d.textContent = text;
  entry.append(d);
  scrollOut();
}

const info = (text) => addLine(newEntry(), text, 'info');

$('clear').addEventListener('click', () => {
  for (const child of [...out.children]) if (child !== canvasHolder) child.remove();
  canvasHolder.hidden = true;
});

// ---------------------------------------------------------------- the image

let module = null;          // the compiled image
let worker = null;
let ready = false;
let running = null;         // { id, entry } of the evaluation going on
let nextId = 1;
let canvasHolder = document.createElement('div');
canvasHolder.hidden = true;
out.append(canvasHolder);

const setStatus = (text) => { status.textContent = text; };

function updateControls() {
  runButton.disabled = !ready || !!running;
  repl.disabled = !ready || !!running;
  stopButton.disabled = !running;
}

async function compileImage() {
  const response = await fetch('clysm.wasm');
  if (!response.ok) throw new Error(`clysm.wasm: ${response.status} ${response.statusText}`);
  try {
    return await WebAssembly.compileStreaming(response.clone());
  } catch (e) {
    // (a server that does not say application/wasm)
    if (!(e instanceof TypeError)) throw e;
    return WebAssembly.compile(await response.arrayBuffer());
  }
}

function startWorker() {
  ready = false;
  updateControls();
  setStatus('Starting…');
  // a canvas can be given to one worker only: a new one for each image
  const canvas = document.createElement('canvas');
  canvas.width = 480;
  canvas.height = 320;
  canvas.setAttribute('aria-label', 'Canvas of the program');
  canvasHolder.replaceChildren(canvas);
  canvasHolder.hidden = true;
  const offscreen = canvas.transferControlToOffscreen();
  worker = new Worker(new URL('./worker.mjs', import.meta.url), { type: 'module' });
  worker.addEventListener('message', onMessage);
  worker.addEventListener('error', (e) => {
    info(`The worker failed: ${e.message || 'see the console'}`);
    setStatus('Failed');
  });
  worker.postMessage({ type: 'boot', module, canvas: offscreen }, [offscreen]);
}

function onMessage({ data }) {
  switch (data.type) {
    case 'ready':
      ready = true;
      promptLabel.textContent = 'CL-USER>';
      setStatus('Ready');
      updateControls();
      break;
    case 'failed':
      info(`Clysm could not start: ${data.message}`);
      setStatus('Failed');
      break;
    case 'output':
      if (running) addText(running.entry, data.text, data.fd === 2 ? 'stderr' : 'stdout');
      break;
    case 'result': {
      if (!running || running.id !== data.id) break;
      const { entry } = running;
      running = null;
      // (output that did not end its line)
      // (the lines below are blocks of their own: the output's last newline
      // would leave an empty line)
      const last = entry.lastElementChild;
      if (last && last.tagName === 'SPAN' && last.textContent.endsWith('\n')) last.textContent = last.textContent.slice(0, -1);
      if (data.error) addLine(entry, `; ${data.error}`, 'error');
      else if (data.values) addLine(entry, data.values.split('\n').map((v, i) => (i ? '  ' : '⇒ ') + v).join('\n'), 'values');
      addLine(entry, `${data.ms < 10 ? data.ms.toFixed(1) : Math.round(data.ms)} ms, compilation included`, 'meta');
      if (data.canvas) {
        canvasHolder.hidden = false;
        out.insertBefore(canvasHolder, entry.nextSibling);
      }
      if (data.package) promptLabel.textContent = `${data.package}>`;
      setStatus('Ready');
      updateControls();
      scrollOut();
      if (running === null && document.activeElement === document.body) repl.focus();
      break;
    }
  }
}

function evaluate(source, head, cls) {
  if (!ready || running) return false;
  const id = nextId++;
  running = { id, entry: newEntry(head, cls) };
  setStatus('Running…');
  updateControls();
  worker.postMessage({ type: 'run', id, code: source });
  return true;
}

stopButton.addEventListener('click', () => {
  if (!running) return;
  worker.terminate();
  addLine(running.entry, '; Stopped. A new image was started: what was defined before is gone.', 'error');
  running = null;
  startWorker();
});

// ---------------------------------------------------------------- program and REPL

let current = null;   // the sample shown, or null

function runProgram() {
  evaluate(code.value, `▸ ${current ? current.title : 'Program'}`);
}
runButton.addEventListener('click', runProgram);

let inputs = [];
try { inputs = JSON.parse(localStorage.getItem('clysm-playground-history')) || []; } catch {}
let historyPos = inputs.length, draft = '';

const resizeRepl = () => { repl.rows = Math.max(1, repl.value.split('\n').length); };
const setRepl = (text) => { repl.value = text; resizeRepl(); repl.setSelectionRange(text.length, text.length); };

repl.addEventListener('input', resizeRepl);
repl.addEventListener('keydown', (e) => {
  if (e.isComposing) return;
  const before = repl.value.slice(0, repl.selectionStart);
  const after = repl.value.slice(repl.selectionEnd);
  if (e.key === 'Enter' && !e.shiftKey) {
    e.preventDefault();
    const src = repl.value;
    if (!src.trim()) return;
    if (!(e.ctrlKey || e.metaKey) && (after.trim() || !complete(src))) {
      insert(repl, '\n' + indentation(before));
      return;
    }
    const lines = src.split('\n');
    const prompt = promptLabel.textContent + ' ';
    const head = prompt + lines.join('\n' + ' '.repeat(prompt.length));
    if (evaluate(src, head, 'input')) {
      if (inputs[inputs.length - 1] !== src) inputs.push(src);
      inputs = inputs.slice(-200);
      historyPos = inputs.length;
      try { localStorage.setItem('clysm-playground-history', JSON.stringify(inputs)); } catch {}
      setRepl('');
    }
  } else if (e.key === 'ArrowUp' && !before.includes('\n') && historyPos > 0) {
    e.preventDefault();
    if (historyPos === inputs.length) draft = repl.value;
    setRepl(inputs[--historyPos]);
  } else if (e.key === 'ArrowDown' && !after.includes('\n') && historyPos < inputs.length) {
    e.preventDefault();
    historyPos++;
    setRepl(historyPos === inputs.length ? draft : inputs[historyPos]);
  }
});

// ---------------------------------------------------------------- samples and links

const sampleButtons = new Map();
for (const sample of samples) {
  const b = document.createElement('button');
  b.textContent = sample.title;
  b.setAttribute('aria-pressed', 'false');
  b.addEventListener('click', () => {
    showSample(sample);
    history.replaceState(null, '', `#${sample.id}`);
  });
  $('samples').append(b);
  sampleButtons.set(sample, b);
}

function setCode(text) {
  code.value = text;
  code.scrollTop = 0;
  rehighlight();
}

function showSample(sample) {
  current = sample;
  for (const [s, b] of sampleButtons) b.setAttribute('aria-pressed', String(s === sample));
  $('sample-title').textContent = sample ? sample.title : 'Program';
  $('sample-note').textContent = sample ? sample.note : 'A program from a link.';
  if (sample) setCode(sample.code);
}

const toBase64 = (text) => {
  const bytes = new TextEncoder().encode(text);
  let s = '';
  for (const b of bytes) s += String.fromCharCode(b);
  return btoa(s).replace(/\+/g, '-').replace(/\//g, '_').replace(/=+$/, '');
};
const fromBase64 = (text) => {
  const s = atob(text.replace(/-/g, '+').replace(/_/g, '/'));
  return new TextDecoder().decode(Uint8Array.from(s, (c) => c.charCodeAt(0)));
};

function showLocation() {
  const hash = decodeURIComponent(location.hash.slice(1));
  if (hash.startsWith('code=')) {
    try {
      showSample(null);
      setCode(fromBase64(hash.slice(5)));
      return;
    } catch { /* not a program: the first sample */ }
  }
  showSample(samples.find((s) => s.id === hash) || samples[0]);
}
window.addEventListener('hashchange', showLocation);

$('share').addEventListener('click', async () => {
  const url = `${location.origin}${location.pathname}#code=${toBase64(code.value)}`;
  history.replaceState(null, '', url);
  current = null;
  for (const b of sampleButtons.values()) b.setAttribute('aria-pressed', 'false');
  try {
    await navigator.clipboard.writeText(url);
    setStatus('Link copied');
  } catch {
    setStatus('Link in the address bar');
  }
});

// ---------------------------------------------------------------- start

showLocation();
setStatus('Loading the image…');
try {
  module = await compileImage();
  startWorker();
} catch (e) {
  setStatus('Failed');
  info(`Clysm could not be loaded: ${e.message}`);
  if (e instanceof WebAssembly.CompileError)
    info('This browser lacks a WebAssembly feature that Clysm uses. Try Chrome or Edge 137 or later.');
}
