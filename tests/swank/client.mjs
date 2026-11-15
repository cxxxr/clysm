// tests/swank/client.mjs — a SWANK client that talks to `clysm swank` as
// SLIME does (M-x slime-connect) and checks the answers.
//   node tests/swank/client.mjs PORT DIR
// DIR is a directory for the files the test compiles and loads.
import net from 'node:net';
import fs from 'node:fs';
import path from 'node:path';

const [port, dir] = [Number(process.argv[2]), process.argv[3]];
let failures = 0;
const check = (what, ok, detail = '') => {
  console.log(`${ok ? 'ok  ' : 'FAIL'} ${what}${ok ? '' : `\n     ${detail.slice(0, 600)}`}`);
  if (!ok) failures++;
};

// one connection: messages are a 6-digit hex length and UTF-8 text
function connect() {
  const socket = net.connect({ host: '127.0.0.1', port });
  let buf = Buffer.alloc(0);
  const waiting = [];
  const queue = [];
  socket.on('data', (d) => {
    buf = Buffer.concat([buf, d]);
    while (buf.length >= 6) {
      const n = parseInt(buf.subarray(0, 6).toString(), 16);
      if (buf.length < 6 + n) break;
      const msg = buf.subarray(6, 6 + n).toString('utf8');
      buf = buf.subarray(6 + n);
      // output to Emacs waits for :write-done
      const w = /^\(:write-string .* (\d+)\)$/s.exec(msg);
      if (w) send(`(:write-done ${w[1]})`);
      if (waiting.length) waiting.shift()(msg); else queue.push(msg);
    }
  });
  const closed = new Promise((resolve) => socket.on('close', resolve));
  const send = (s) => {
    const b = Buffer.from(s, 'utf8');
    socket.write(Buffer.concat([Buffer.from(b.length.toString(16).padStart(6, '0')), b]));
  };
  const next = (ms = 120000) => new Promise((resolve, reject) => {
    if (queue.length) return resolve(queue.shift());
    const t = setTimeout(() => reject(new Error('no answer from the server')), ms);
    waiting.push((m) => { clearTimeout(t); resolve(m); });
  });
  let id = 0;
  // a request: the messages up to its :return (or a :debug-activate)
  const rex = async (form, thread = 't') => {
    const n = ++id;
    send(`(:emacs-rex ${form} "COMMON-LISP-USER" ${thread} ${n})`);
    const msgs = [];
    for (;;) {
      const m = await next();
      if (m.startsWith('(:indentation-update')) continue;
      msgs.push(m);
      if (m.startsWith('(:return') && m.endsWith(` ${n})`)) return { msgs, result: m, text: msgs.join('\n') };
      if (m.startsWith('(:debug-activate')) return { msgs, result: null, text: msgs.join('\n') };
    }
  };
  // the messages up to one that matches RE
  const until = async (re, ms) => {
    const msgs = [];
    for (;;) {
      const m = await next(ms);
      msgs.push(m);
      if (re.test(m)) return { msgs, result: m, text: msgs.join('\n') };
    }
  };
  const sendRex = (form, thread = 't') => {
    const n = ++id;
    send(`(:emacs-rex ${form} "COMMON-LISP-USER" ${thread} ${n})`);
    return n;
  };
  return { rex, send, sendRex, until, socket, closed };
}

const lisp = (s) => '"' + s.replace(/\\/g, '\\\\').replace(/"/g, '\\"') + '"';

fs.mkdirSync(dir, { recursive: true });
const file = path.join(dir, 'defs.lisp');
fs.writeFileSync(file, `;; a file compiled with C-c C-k
(defpackage :swank-test (:use :cl))
(in-package :swank-test)

;;; ünïcödé before the definitions
(defun area (width &optional (height width))
  (* width height))

(defgeneric shape-name (shape))
(defmethod shape-name ((shape integer)) "integer")

(defun outer (x)
  (list (inner x)))
(defun inner (x)
  (let ((doubled (* 2 x)))
    (list (error "inner failed with ~A" x) doubled)))
`);

const c = connect();
let r = await c.rex('(swank:connection-info)');
check('connection-info', /:lisp-implementation \(:type "Clysm" :name "clysm"/.test(r.result) && /:style nil/.test(r.result), r.text);

r = await c.rex("(swank:swank-require '(swank-repl swank-arglists swank-fuzzy swank-fancy-inspector swank-c-p-c swank-package-fu swank-presentations swank-trace-dialog swank-macrostep))");
check('swank-require of the contribs of slime-fancy', /"SWANK-REPL"/.test(r.result) && /"SWANK-FANCY-INSPECTOR"/.test(r.result), r.text);

r = await c.rex('(swank-repl:create-repl nil)');
check('create-repl', /\(:ok \("COMMON-LISP-USER" "CL-USER"\)\)/.test(r.result), r.text);

r = await c.rex(`(swank-repl:listener-eval ${lisp('(defun fact (n) (if (< n 2) 1 (* n (fact (1- n)))))\n')})`);
check('REPL: DEFUN returns the name', /\(:write-string "FACT\n" :repl-result\)/.test(r.text), r.text);

r = await c.rex(`(swank-repl:listener-eval ${lisp('(progn (format t "hello~%") (fact 20))\n')})`);
check('REPL: output, then the value',
      /\(:write-string "hello\n" nil \d+\)[\s\S]*"2432902008176640000\n" :repl-result/.test(r.text), r.text);

r = await c.rex(`(swank:interactive-eval ${lisp('(fact 5)')} 1 80)`);
check('interactive-eval (C-x C-e)', /"=> 120 /.test(r.result), r.text);

r = await c.rex(`(swank:compile-string-for-emacs ${lisp('(defun twice (x) "Double X." (* 2 x))')} "scratch.lisp" '((:position 1) (:line 1 1)) nil nil)`);
check('compile-string-for-emacs (C-c C-c)', /:compilation-result nil t/.test(r.result), r.text);

r = await c.rex(`(swank:autodoc '("twice" "" swank::%cursor-marker%) :print-right-margin 80)`);
check('autodoc of a function defined in the image', /"\(twice ===> x <===\)"/.test(r.result), r.text);
r = await c.rex(`(swank:autodoc '("mapcar" "" swank::%cursor-marker%) :print-right-margin 80)`);
check('autodoc of a function of the runtime', /"\(mapcar ===> f <=== list &rest more\)"/.test(r.result), r.text);
r = await c.rex(`(swank:autodoc '("with-open-file" "" swank::%cursor-marker%) :print-right-margin 80)`);
check('autodoc of a macro of the runtime', /with-open-file \(var filename/.test(r.result), r.text);

r = await c.rex(`(swank:find-definitions-for-emacs "twice")`);
check('M-. of a definition from a buffer', /\(:buffer "scratch.lisp"\) \(:offset 1 0\)/.test(r.result), r.text);
r = await c.rex(`(swank:find-definitions-for-emacs "mapcar")`);
check('M-. of a function of the runtime', /\(DEFUN MAPCAR\)" \(:location \(:file "[^"]*runtime\/core\.lisp"\) \(:position \d+\) \(:snippet "\(defun mapcar/.test(r.result), r.text);

r = await c.rex(`(swank:compile-file-for-emacs ${lisp(file)} t)`);
check('compile-file-for-emacs (C-c C-k)', /:compilation-result nil t [\d.e+-]+ t "/.test(r.result), r.text);
// SLIME loads the compiled file
const fasl = /:compilation-result nil t [\d.e+-]+ t ("[^"]*")/.exec(r.result);
r = await c.rex(`(swank:load-file ${fasl ? fasl[1] : '""'})`);
check('load-file', /\(:ok "T"\)/.test(r.result), r.text);

r = await c.rex(`(swank:find-definitions-for-emacs "swank-test::area")`);
const pos = /\(:position (\d+)\)/.exec(r.result);
const chars = [...fs.readFileSync(file, 'utf8')];
check('M-. of a function of a compiled file (a character position)',
      pos && chars.slice(Number(pos[1]) - 1).join('').startsWith('(defun area'), r.text);
r = await c.rex(`(swank:find-definitions-for-emacs "swank-test::shape-name")`);
check('M-. of a generic function and its methods', /DEFGENERIC/.test(r.result) && /DEFMETHOD SWANK-TEST::SHAPE-NAME \(INTEGER\)/.test(r.result), r.text);

r = await c.rex(`(swank:autodoc '("swank-test::area" "" swank::%cursor-marker%) :print-right-margin 80)`);
check('autodoc of a function of a compiled file', /area ===> width <=== &optional/.test(r.result), r.text);

r = await c.rex(`(swank:simple-completions "mak" "COMMON-LISP-USER")`);
check('completions', /"make-hash-table"/.test(r.result), r.text);
r = await c.rex(`(swank:fuzzy-completions "mvb" "COMMON-LISP-USER" :limit 5 :time-limit-in-msec 1500)`);
check('fuzzy completions', /"multiple-value-bind"/.test(r.result), r.text);

r = await c.rex(`(swank:init-inspector ${lisp('(list 1 2 3)')})`);
check('inspector', /"A proper list:"/.test(r.result), r.text);

r = await c.rex(`(swank:swank-macroexpand-1 ${lisp('(when a b)')})`);
check('macroexpand', /\(IF A/.test(r.result), r.text);

// the debugger
r = await c.rex(`(swank-repl:listener-eval ${lisp('(swank-test::outer 3)\n')})`);
check('an error enters the debugger', /\(:debug \d+ 1 \("inner failed with 3"/.test(r.text), r.text);
r = await c.rex('(swank:backtrace 0 20)', '1');
// frames print as calls, with the values of the arguments
const frame = /\((\d+) "\(OUTER 3\)"\)/.exec(r.result);
const innerFrame = /\((\d+) "\(INNER 3\)"\)/.exec(r.result);
check('backtrace', frame && innerFrame && /"EVAL-FOR-EMACS"/.test(r.result), r.text);
if (frame) {
  r = await c.rex(`(swank:frame-source-location ${frame[1]})`, '1');
  check('source of a frame (v in sldb)', /defs\.lisp"\) \(:position \d+\) \(:snippet "\(defun outer/.test(r.result), r.text);
}
if (innerFrame) {
  r = await c.rex(`(swank:frame-locals-and-catch-tags ${innerFrame[1]})`, '1');
  check('locals of a frame', /\(:name "SWANK-TEST::X" :id 0 :value "3"\) \(:name "SWANK-TEST::DOUBLED" :id 0 :value "6"\)/.test(r.result), r.text);
  r = await c.rex(`(swank:eval-string-in-frame ${lisp('(* x doubled)')} ${innerFrame[1]} "SWANK-TEST" 1 80)`, '1');
  check('evaluation in a frame (e in sldb)', /"=> 18 /.test(r.result), r.text);
}
r = await c.rex('(swank:sldb-abort)', '1');
r = await c.rex(`(swank-repl:listener-eval ${lisp('(fact 10)\n')})`);
check('the REPL after the debugger', /"3628800\n" :repl-result/.test(r.text), r.text);

// C-c C-c: an evaluation that does not end is interrupted
const sleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms));
r = await c.rex(`(swank-repl:listener-eval ${lisp('(defvar *spins* 0)\n(defun spin-forever () (loop (incf *spins*)))\n')})`);
let n = c.sendRex(`(swank-repl:listener-eval ${lisp('(spin-forever)\n')})`);
await sleep(1500);
c.send('(:emacs-interrupt t)');
try {
  r = await c.until(/^\(:debug-activate/, 20000);
  check('C-c C-c interrupts a loop', /\(:debug 0 1 \("Interrupt from Emacs"/.test(r.text) && /\(0 "\(SPIN-FOREVER\)"\)/.test(r.text), r.text);
  // continue, and interrupt again
  c.sendRex('(swank:sldb-continue)', '0');
  await sleep(1000);
  r = await c.rex(`(swank:interactive-eval ${lisp('(plusp *spins*)')} 1 80)`);
  check('a request is answered during an evaluation', /"=> T"/.test(r.result), r.text);
  c.send('(:emacs-interrupt t)');
  r = await c.until(/^\(:debug-activate/, 20000);
  check('the computation continues after the interrupt', /"Interrupt from Emacs"/.test(r.text), r.text);
  c.sendRex('(swank:sldb-abort)', '0');
  r = await c.until(new RegExp(`^\\(:return \\(:abort .*\\) ${n}\\)$`), 20000);
  check('the interrupted evaluation is aborted', true);
  r = await c.rex(`(swank-repl:listener-eval ${lisp('(fact 7)\n')})`);
  check('the REPL after an interrupt', /"5040\n" :repl-result/.test(r.text), r.text);
  // SLEEP of the runtime
  n = c.sendRex(`(swank-repl:listener-eval ${lisp('(sleep 600)\n')})`);
  await sleep(1000);
  c.send('(:emacs-interrupt t)');
  r = await c.until(/^\(:debug-activate/, 20000);
  check('C-c C-c interrupts SLEEP', /"Interrupt from Emacs"/.test(r.text), r.text);
  c.sendRex('(swank:sldb-abort)', '0');
  r = await c.until(new RegExp(`^\\(:return \\(:abort .*\\) ${n}\\)$`), 20000);
} catch (e) {
  check('C-c C-c interrupts a loop', false, String(e));
}

c.socket.end();
await c.closed;

// a server started with --dont-close takes the next connection
const c2 = connect();
r = await c2.rex(`(swank:interactive-eval ${lisp('(fact 6)')} 1 80)`);
check('a second connection', /"=> 720 /.test(r.result), r.text);
c2.send('(:emacs-rex (swank:quit-lisp) "COMMON-LISP-USER" t 99)');
await c2.closed;

console.log(failures ? `swank: ${failures} failed` : 'swank: ok');
process.exit(failures ? 1 : 0);
