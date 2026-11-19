// tests/ide/desktop.js — drives Clysm IDE (examples/ide/) in the Electron
// shell (CLYSM_ELECTRON_TEST, see ui/electron/main.js) on a project folder:
// it types into the page with sendInputEvent, as a user does, and checks the
// page and the files on the disk.  Run by tests/ide/desktop.sh.  Prints
// "ok NAME" or "FAIL NAME: ..." for each check, then quits.
const fs = require('node:fs');
const path = require('node:path');

const sleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms));
const project = process.env.IDE_TEST_PROJECT;
const shot = process.env.IDE_TEST_SCREENSHOT;

module.exports = (win, app) => {
  const wc = win.webContents;
  win.setContentSize(1200, 800);
  const errors = [];
  wc.on('console-message', (event) => {
    if (event.level === 'error' || event.level === 3) errors.push(event.message);
  });
  const js = (code) => wc.executeJavaScript(code);
  const key = async (keyCode, modifiers = []) => {
    wc.sendInputEvent({ type: 'keyDown', keyCode, modifiers });
    wc.sendInputEvent({ type: 'keyUp', keyCode, modifiers });
    await sleep(40);
  };
  const click = async (selector) => {
    const box = await js(`(() => { const e = document.querySelector(${JSON.stringify(selector)});
                                   if (!e) return null;
                                   const r = e.getBoundingClientRect();
                                   return [r.left + Math.min(20, r.width / 2), r.top + r.height / 2]; })()`);
    if (!box) throw new Error(`nothing to click: ${selector}`);
    const [x, y] = box.map(Math.round);
    wc.sendInputEvent({ type: 'mouseDown', x, y, button: 'left', clickCount: 1 });
    wc.sendInputEvent({ type: 'mouseUp', x, y, button: 'left', clickCount: 1 });
    await sleep(150);
  };
  // type into the REPL's input and evaluate
  const repl = async (text) => {
    await click('.ide-input .ce-scroller');
    wc.insertText(text);
    await sleep(50);
    await key('Return');
  };
  const log = () => js("[...document.querySelectorAll('.ide-log > *')].map((e) => e.textContent).join('\\n')");
  const lastValue = () => js("(() => { const v = [...document.querySelectorAll('.e-values')].pop(); return v ? v.textContent : null; })()");
  const editorText = () => js("[...document.querySelectorAll('.ide-editor .ce-line .ce-text')].map((t) => t.textContent).join('\\n')");
  let failed = 0;
  const check = (name, ok, detail = '') => {
    if (ok) console.log(`ok ${name}`);
    else { failed++; console.log(`FAIL ${name}${detail ? ': ' + JSON.stringify(detail) : ''}`); }
  };
  const until = async (predicate, ms = 5000) => {
    for (let t = 0; t < ms; t += 100) { if (await predicate()) return true; await sleep(100); }
    return false;
  };

  wc.once('did-finish-load', async () => {
    try {
      check('starts', await until(async () => (await js("document.querySelectorAll('.ide-row.file').length")) > 0, 30000));
      const files = await js("[...document.querySelectorAll('.ide-files .ide-row')].map((e) => e.textContent.trim())");
      check('the project of the command line', (await js("document.querySelector('.ide-project').textContent")) === 'project');
      check('the files of the folder (not .git)', JSON.stringify(files) === JSON.stringify(['λhello.lisp', '▾ util', 'λmath.lisp']), files);

      // open a file from the tree, evaluate it
      await click('.ide-row.file[title="util/math.lisp"]');
      check('opens a file', await until(async () => (await editorText()) === fs.readFileSync(path.join(project, 'util/math.lisp'), 'utf8')),
            await editorText());
      await key('Return', ['control', 'shift']);
      check('evaluates the file', await until(async () => /MATH:SQUARE/.test(await lastValue() || '')), await lastValue());
      await click('.ide-row.file[title="hello.lisp"]');
      await sleep(200);
      await key('Return', ['control', 'shift']);
      check('evaluates another', await until(async () => /HELLO:GREET/.test(await lastValue() || '')), await lastValue());

      // the REPL: output and values
      await repl('(hello:greet "Lisp")');
      check('the REPL', await until(async () => (await lastValue()) === '⇒ 16'), await lastValue());
      check('output of the REPL', /Hello, Lisp!/.test(await log()));

      // the files of the computer from Lisp, relative to the project
      await repl('(with-open-file (s "out.txt" :direction :output :if-exists :supersede) (write-string "written by Lisp" s))');
      await until(async () => /written by Lisp/.test(await lastValue() || ''));
      const out = path.join(project, 'out.txt');
      check('Lisp writes a file in the project', fs.existsSync(out) && fs.readFileSync(out, 'utf8') === 'written by Lisp');
      await repl('(mapcar (function file-namestring) (directory "*.lisp"))');
      check('Lisp lists the project', await until(async () => (await lastValue()) === '⇒ ("hello.lisp")'), await lastValue());

      // go to a definition in another file: M-. on math:square
      const tabs = await js("[...document.querySelectorAll('.ide-tab')].map((t) => t.firstChild.textContent)");
      check('a tab for each file', JSON.stringify(tabs) === JSON.stringify(['scratch', 'math.lisp', 'hello.lisp']), tabs);
      await click(`.ide-tab:nth-child(${tabs.indexOf('hello.lisp') + 1})`);
      await sleep(200);
      const line = await js("[...document.querySelectorAll('.ide-editor .ce-line')].find((l) => l.textContent.includes('math:square'))?.dataset.line");
      if (line === undefined) throw new Error('no math:square in ' + await editorText());
      await click(`.ide-editor .ce-line[data-line="${line}"] .ce-text`);
      for (let i = 0; i < 8; i++) await key('Right');
      await key('F12');
      check('goes to the definition in another file',
            await until(async () => /math\.lisp/.test(await js("document.querySelector('.ide-tab.current').textContent"))),
            await js("document.querySelector('.ide-tab.current').textContent"));

      // edit and save
      await key('End', ['control']);
      wc.insertText('\n(defun cube (x) (* x x x))');
      await sleep(100);
      check('modified', await js("!!document.querySelector('.ide-tab.current .dot')"));
      await key('s', ['control']);
      await sleep(300);
      check('Mod-S saves', fs.readFileSync(path.join(project, 'util/math.lisp'), 'utf8').endsWith('(defun cube (x) (* x x x))'));
      check('saved', await js("!document.querySelector('.ide-tab.current .dot')"));
      check('outline', /cube/.test(await js("document.querySelector('.ide-outline').textContent")));

      // an endless loop, interrupted; the IDE works meanwhile
      await repl('(loop)');
      await sleep(500);
      check('busy while it runs', await js("!!document.querySelector('.ide-repl .busy')"));
      await click('.ide-toolbar button.danger');
      check('interrupted into the debugger', await until(async () => /中断しました/.test(await js("document.querySelector('.ide-debugger')?.textContent || ''"))));
      await click('.ide-debugger .restart.abort');
      check('gives up', await until(async () => /評価をやめました/.test(await log())));
      await repl('(+ 1 2)');
      check('the REPL after', await until(async () => (await lastValue()) === '⇒ 3'), await lastValue());

      if (shot) fs.writeFileSync(shot, (await wc.capturePage()).toPNG());
      check('no errors', errors.length === 0, errors.join('\n'));
    } catch (e) {
      failed++;
      console.log(`FAIL exception: ${e.stack}`);
    }
    console.log(failed ? `${failed} failed` : 'all passed');
    app.exit(failed ? 1 : 0);
  });
};
