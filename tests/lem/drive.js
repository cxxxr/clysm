// tests/lem/drive.js — drives Lem in the Electron shell (CLYSM_ELECTRON_TEST):
// it types into the page with sendInputEvent, and checks what Lem has (with
// lisp.eval in the page) and what the page shows.  Prints "ok NAME" or
// "FAIL NAME: ..." for each check, then quits.
// With LEM_WEB_PAGE, the page is the one for a browser (web-shell.js), whose
// files are in memory: the test writes and reads them through Lisp.
const fs = require('node:fs');
const path = require('node:path');

const web = !!process.env.LEM_WEB_PAGE;
const label = web ? 'lem (browser)' : 'lem';
let work = process.env.LEM_TEST_DIR;
const sleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms));

module.exports = (win, app) => {
  const wc = win.webContents;
  win.setContentSize(960, 640);
  const errors = [];
  wc.on('console-message', (event) => {
    // (Lem's log messages are written to the error output)
    if ((event.level === 'error' || event.level === 3) && !/^\[(INFO|WARN)\]/.test(event.message))
      errors.push(event.message);
  });
  const js = (code) => wc.executeJavaScript(code);
  // the printed values of FORM, evaluated in Lem's image
  const lisp = async (form) => (await js(`lisp.eval(${JSON.stringify(form)})`)).replace(/\n$/, '');
  const screen = () => js("document.querySelector('.lem-screen').innerText");
  const key = async (keyCode, modifiers = []) => {
    wc.sendInputEvent({ type: 'keyDown', keyCode, modifiers });
    if (keyCode.length === 1 && modifiers.every((m) => m === 'shift'))
      wc.sendInputEvent({ type: 'char', keyCode, modifiers });
    wc.sendInputEvent({ type: 'keyUp', keyCode, modifiers });
    await sleep(40);
  };
  const type = async (text) => { for (const c of text) await key(c); };
  const command = async (name) => { await key('x', ['alt']); await sleep(100); await type(name); await key('Return'); await sleep(500); };
  let failed = 0;
  const check = (name, ok, detail = '') => {
    if (ok) console.log(`ok ${name}`);
    else { failed++; console.log(`FAIL ${name}${detail ? ': ' + detail : ''}`); }
  };
  const until = async (predicate, ms = 5000) => {
    for (let t = 0; t < ms; t += 100) { if (await predicate()) return true; await sleep(100); }
    return false;
  };
  const lispString = (string) => '"' + string.replace(/\\/g, '\\\\').replace(/"/g, '\\"') + '"';
  const writeFile = async (file, text) => {
    if (!web) return fs.writeFileSync(file, text);
    await lisp(`(with-open-file (out (ensure-directories-exist ${lispString(file)}) :direction :output :if-exists :supersede) (write-string ${lispString(text)} out))`);
  };
  const readFile = async (file) => {
    if (!web) return fs.readFileSync(file, 'utf8');
    // a string as Lisp prints it: in double quotes, \ before " and \
    const printed = await lisp(`(uiop:read-file-string ${lispString(file)})`);
    return printed.slice(1, -1).replace(/\\(.)/g, '$1');
  };

  wc.once('did-finish-load', async () => {
    try {
      const started = await until(async () => (await js("document.querySelectorAll('.lem-line').length")) > 0, 60000);
      if (web) {
        // the page starts with welcome.lisp; the test types into a new buffer
        check('start', started && (await screen()).includes('welcome.lisp'));
        work = JSON.parse(await lisp('(namestring (user-homedir-pathname))')) + 'lem-test';
        await key('x', ['control']); await key('b'); await sleep(200);
        await type('test'); await key('Return'); await sleep(200); await key('y'); await sleep(300);
      } else {
        check('start', started && (await screen()).includes('*tmp*'));
      }

      await type('(hello world)'); await key('Return');
      wc.insertText('日本語');       // what an input method commits
      await sleep(300);
      const text = await lisp('(lem:buffer-text (lem:current-buffer))');
      check('typing', text === '"(hello world)\n日本語"', text);
      check('typing shown', (await screen()).includes('(hello world)'));

      await command('list-buffers');
      check('list-buffers', (await screen()).includes('Buffer') &&
            (await lisp('(lem:get-buffer "*EDITOR ERROR*")')).startsWith('NIL'));
      await key('g', ['control']); await sleep(200);

      const file = path.join(work, 'sample.lisp');
      await writeFile(file, '(defun fact (n)\n  (if (<= n 1) 1 (* n (fact (1- n)))))\n');
      await key('x', ['control']); await key('f', ['control']); await sleep(300);
      await key('a', ['control']); await key('k', ['control']);
      await type(file); await key('Return'); await sleep(800);
      const name = await lisp('(lem:buffer-name (lem:current-buffer))');
      check('find-file', name === '"sample.lisp"', name);
      const mode = await lisp('(lem:buffer-major-mode (lem:current-buffer))');
      check('lisp mode', mode.includes('LISP-MODE'), mode);
      check('highlighting', await js("[...document.querySelectorAll('.lem-line span')].some((s) => s.innerText === 'defun' && s.style.color)"));

      // (the first compilation in the page takes a while)
      await key('x', ['alt', 'control']);
      check('eval defun', await until(async () => (await lisp('(cl-user::fact 10)')) === '3628800'),
            await lisp('(cl-user::fact 10)'));

      await key('>', ['alt', 'shift']); await type('; saved'); await key('x', ['control']); await key('s', ['control']); await sleep(800);
      check('save', (await readFile(file)).endsWith('; saved'));
      if (web) check('kept by the browser', await js(`localStorage.getItem('lem-clysm:' + ${JSON.stringify(file)}) !== null`));

      // a click moves the cursor: line 2, column 4 (below the line of tabs)
      const [cw, ch] = JSON.parse(await js("JSON.stringify((() => { const s = document.querySelector('.lem-line span'); const l = s.parentElement; return [parseFloat(s.style.width) / s.innerText.length, l.offsetHeight]; })())"));
      wc.sendInputEvent({ type: 'mouseDown', x: Math.round(4.5 * cw), y: Math.round(2.5 * ch), button: 'left', clickCount: 1 });
      wc.sendInputEvent({ type: 'mouseUp', x: Math.round(4.5 * cw), y: Math.round(2.5 * ch), button: 'left', clickCount: 1 });
      await sleep(500);
      const position = await lisp('(list (lem:line-number-at-point (lem:current-point)) (lem:point-charpos (lem:current-point)))');
      check('click', position === '(2 4)', position);

      // a drag selects
      const at = (column, line) => ({ x: Math.round((column + 0.5) * cw), y: Math.round((line + 1.5) * ch) });
      wc.sendInputEvent({ type: 'mouseDown', ...at(2, 0), button: 'left', clickCount: 1 });
      wc.sendInputEvent({ type: 'mouseMove', ...at(6, 0), button: 'left', modifiers: ['leftButtonDown'] });
      wc.sendInputEvent({ type: 'mouseUp', ...at(6, 0), button: 'left', clickCount: 1 });
      await sleep(500);
      const region = await lisp('(and (lem:buffer-mark-p (lem:current-buffer)) (lem:points-to-string (lem:region-beginning (lem:current-buffer)) (lem:region-end (lem:current-buffer))))');
      check('drag', region === '"efun"', region);

      // the wheel scrolls
      const long = path.join(work, 'long.txt');
      await writeFile(long, Array.from({ length: 300 }, (_, i) => `line ${i + 1}`).join('\n'));
      await key('x', ['control']); await key('f', ['control']); await sleep(300);
      await key('a', ['control']); await key('k', ['control']);
      await type(long); await key('Return'); await sleep(800);
      wc.sendInputEvent({ type: 'mouseWheel', ...at(5, 5), deltaX: 0, deltaY: -120, canScroll: true });
      await sleep(800);
      const top = await lisp('(lem:line-number-at-point (lem:window-view-point (lem:current-window)))');
      check('wheel', Number(top) > 1, top);

      await command('lisp-repl');
      await type('(list (fact 5) (lisp-implementation-type))'); await key('Return');
      const repl = () => lisp('(lem:buffer-text (lem:get-buffer "*lisp-repl*"))');
      check('repl', await until(async () => (await repl()).includes('(120 \\"Clysm\\")')), await repl());

      const width = await lisp('(lem:display-width)');
      win.setContentSize(700, 400);
      check('resize', await until(async () => (await lisp('(lem:display-width)')) !== width));

      check('no errors on the console', errors.length === 0, errors.join(' | '));
      const finish = () => { console.log(failed ? `${label}: ${failed} failed` : `${label}: ok`); app.exit(failed ? 1 : 0); };
      win.on('closed', finish);
      await key('x', ['control']); await key('c', ['control']);
      // (a browser may not let the page close: then it says that Lem has exited)
      const exited = await until(async () => win.isDestroyed() || (await js("document.body.innerText")).includes('Lem has exited'), 3000);
      check('exit', exited, 'C-x C-c did not close the window');
      finish();
    } catch (e) {
      console.log('FAIL', e && e.stack || e);
    }
    app.exit(1);
  });
};
