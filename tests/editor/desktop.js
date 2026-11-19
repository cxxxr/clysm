// tests/editor/desktop.js — drives the editor of examples/editor/ in the
// Electron shell (CLYSM_ELECTRON_TEST, see ui/electron/main.js): it types
// into the page with sendInputEvent, as a user does, and checks the page,
// the file on the disk and the clipboard.  Run by tests/editor/desktop.sh.
// Prints "ok NAME" or "FAIL NAME: ..." for each check, then quits.
const fs = require('node:fs');
const path = require('node:path');
// (this file is outside the application: Electron's module is the main module's)
const { clipboard } = require.main.require('electron');

const sleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms));
const file = process.env.EDITOR_TEST_FILE;
const shot = process.env.EDITOR_TEST_SCREENSHOT;

module.exports = (win, app) => {
  const wc = win.webContents;
  win.setContentSize(1000, 700);
  const errors = [];
  wc.on('console-message', (event) => {
    if (event.level === 'error' || event.level === 3) errors.push(event.message);
  });
  const js = (code) => wc.executeJavaScript(code);
  const key = async (keyCode, modifiers = []) => {
    wc.sendInputEvent({ type: 'keyDown', keyCode, modifiers });
    if (keyCode.length === 1 && modifiers.every((m) => m === 'shift'))
      wc.sendInputEvent({ type: 'char', keyCode, modifiers });
    wc.sendInputEvent({ type: 'keyUp', keyCode, modifiers });
    await sleep(30);
  };
  const type = async (text) => {
    for (const c of text) {
      if (c === '\n') await key('Return');
      else await key(c, /[A-Z~"(){}<>?:+_|!@#$%^&*]/.test(c) ? ['shift'] : []);
    }
  };
  // the text on the page (the test's file is short: every line is shown)
  const text = () => js("[...document.querySelectorAll('.ce-line .ce-text')].map((t) => t.textContent).join('\\n')");
  const status = () => js("document.querySelector('.status').textContent");
  const title = () => js('document.title');
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
      const original = fs.readFileSync(file, 'utf8');
      check('starts', await until(async () => (await js("document.querySelectorAll('.ce-line').length")) > 0, 30000));
      check('opens the file of the command line',
            await until(async () => (await text()) === original), await text());
      check('title names the file', /test\.lisp — Clysm Editor/.test(await title()), await title());
      check('desktop toolbar', /開く.*保存/.test(await js("document.querySelector('.toolbar').textContent")));
      check('editor has the keyboard', (await js('document.activeElement.className')) === 'ce-input');

      // typing: indentation and the closing bracket
      await key('End', ['control']);
      await type('\n(defun added (x)\nx');
      const typed = await text();
      check('typing indents', typed.endsWith('\n(defun added (x)\n  x)'), typed.slice(-40));
      check('modified in the title', (await title()).startsWith('●'), await title());

      // an input method's text
      wc.insertText('日本語');
      await sleep(100);
      check('inserted text', (await text()).endsWith('  x日本語)'), (await text()).slice(-20));

      // undo and redo: the text typed in a row (the composed text too) is one step
      await key('z', ['control']);
      check('undo', (await text()).endsWith('(defun added (x)\n  )'), (await text()).slice(-20));
      await key('z', ['control', 'shift']);
      check('redo', (await text()).endsWith('  x日本語)'));

      // saving: Mod-S writes the file
      await key('s', ['control']);
      await sleep(200);
      const saved = fs.readFileSync(file, 'utf8');
      check('Mod-S saves the file', saved === (await text()), saved.slice(-30));
      check('saved in the title', !(await title()).startsWith('●'), await title());

      // the clipboard of the system: select all, copy, paste at the end
      // (in Electron 45 the clipboard of the main process returns promises)
      await clipboard.writeText('');
      await key('a', ['control']);
      await key('c', ['control']);
      await sleep(100);
      const copied = await clipboard.readText();
      check('copy to the clipboard', copied === saved, copied.slice(0, 30));
      // the shell's clipboard, as ui/desktop.lisp reads it (synchronously)
      await clipboard.writeText('from the system');
      const read = await js("desktop.call('readClipboard')");
      check('the shell reads the clipboard', read === 'from the system', read);
      await clipboard.writeText(';; pasted');
      await key('End', ['control']);
      await key('Return');
      await key('v', ['control']);
      await sleep(100);
      check('paste from the clipboard', (await text()).endsWith('\n;; pasted'), (await text()).slice(-20));

      // Emacs keys: C-a C-k kills the line, C-x C-s saves
      await js(`(() => { const s = document.querySelectorAll('.toolbar select')[1];
                         s.value = 'emacs'; s.dispatchEvent(new Event('change')); })()`);
      await sleep(100);
      check('emacs keymap', /Emacs/.test(await status()));
      await key('a', ['control']);
      await key('k', ['control']);
      check('C-k puts the line on the clipboard', (await clipboard.readText()) === ';; pasted',
            await clipboard.readText());
      await key('x', ['control']);
      check('prefix key shown', /C-x-/.test(await status()), await status());
      await key('s', ['control']);
      await sleep(200);
      check('C-k and C-x C-s', fs.readFileSync(file, 'utf8') === saved + '\n', fs.readFileSync(file, 'utf8').slice(-30));

      // the mouse: a double click selects a word
      const box = await js(`(() => { const r = document.querySelector('.ce-def-name').getBoundingClientRect();
                                     return [r.left + 4, r.top + r.height / 2]; })()`);
      for (const clickCount of [1, 2]) {
        wc.sendInputEvent({ type: 'mouseDown', x: Math.round(box[0]), y: Math.round(box[1]), button: 'left', clickCount });
        wc.sendInputEvent({ type: 'mouseUp', x: Math.round(box[0]), y: Math.round(box[1]), button: 'left', clickCount });
        await sleep(60);
      }
      const word = await js("[...document.querySelectorAll('.ce-selected')].map((e) => e.textContent).join('')");
      check('double click selects a word', word === 'square', word);

      if (shot) {
        await js("document.querySelector('.ce-input').focus()");
        await sleep(200);
        fs.writeFileSync(shot, (await wc.capturePage()).toPNG());
      }
      check('no errors', errors.length === 0, errors.join('\n'));
    } catch (e) {
      failed++;
      console.log(`FAIL exception: ${e.stack}`);
    }
    console.log(failed ? `${failed} failed` : 'all passed');
    app.exit(failed ? 1 : 0);
  });
};
