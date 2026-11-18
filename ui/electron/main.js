// The Electron shell of a Clysm desktop application.
//
// The application is a page (index.html next to this file, which loads a
// Lisp program compiled to WebAssembly) written with the UI framework; this
// process opens its window and does what a page cannot do — the menu bar,
// native dialogs and files — when ui/desktop.lisp asks through the `desktop`
// object of preload.js.  It contains nothing specific to an application: the
// name and the window come from package.json,
//   "window": { "width": 640, "height": 720, "minWidth": 380, "minHeight": 400,
//               "backgroundColor": { "light": "#ffffff", "dark": "#151618" } }
// With "node": true, the page can use Node.js (require, process): a Lisp
// program that has the files of the computer (host/web/clysm.mjs with fs),
// as have its workers.
// "jsFlags" are flags of the JavaScript engine (e.g. a larger stack).
// With "resident": true the application stays: closing its window hides it,
// and a client (bin/client of the application, e.g. contrib/lem/desktop/lem)
// shows it at once through a Unix domain socket in the application's data
// directory (resident.sock): lines "open FILE" (FILE given to the page's
// desktop.onOpen), "show", "hide" and "stop" (which ends it); when the page
// ends the application with desktop.call('quit'), it starts again hidden.
// "--hidden" on the command line starts it hidden.
// The environment variable CLYSM_ELECTRON_TEST names a module whose function
// is called with the window (tests drive the page with sendInputEvent and
// look at it with capturePage); the window is then not shown.
const { app, BrowserWindow, Menu, dialog, ipcMain, nativeTheme, net, protocol, clipboard } = require('electron');
const fs = require('node:fs');
const path = require('node:path');
const netServer = require('node:net');
const { pathToFileURL } = require('node:url');

const options = require('./package.json').window || {};

// the page is served from app://app/ (fetch, which loads the .wasm file,
// does not work on file: URLs)
protocol.registerSchemesAsPrivileged([
  { scheme: 'app', privileges: { standard: true, secure: true, supportFetchAPI: true } },
]);

// CLYSM_USER_DATA replaces the directory of the application's data (for tests)
if (process.env.CLYSM_USER_DATA) app.setPath('userData', process.env.CLYSM_USER_DATA);

if (options.jsFlags) app.commandLine.appendSwitch('js-flags', options.jsFlags);

const testModule = process.env.CLYSM_ELECTRON_TEST;

let win = null;
let quitting = false;
const resident = !!options.resident;
const startHidden = process.argv.includes('--hidden');
// files to open given on the command line (after the application's directory)
const argvFiles = (argv) => argv.slice(2).filter((a) => !a.startsWith('-') && a !== '.')
  .map((a) => path.resolve(a));

// ---- the functions called from Lisp: (ui::desktop-call "name" argument...)

const filters = (list) => (list || []).map(({ name, extensions }) => ({ name, extensions }));

const api = {
  readFile(file) {
    try { return fs.readFileSync(file, 'utf8'); } catch { return null; }
  },
  // null, or the reason why the file could not be written
  writeFile(file, text) {
    try {
      fs.mkdirSync(path.dirname(file), { recursive: true });
      fs.writeFileSync(file, text);
      return null;
    } catch (e) { return e.message; }
  },
  userDataFile(name) { return path.join(app.getPath('userData'), name); },
  addRecentDocument(file) { app.addRecentDocument(file); return null; },
  openDialog(o) {
    const files = dialog.showOpenDialogSync(win, {
      title: o.title || undefined, properties: ['openFile'], filters: filters(o.filters),
    });
    return files ? files[0] : null;
  },
  saveDialog(o) {
    return dialog.showSaveDialogSync(win, {
      title: o.title || undefined, filters: filters(o.filters),
      defaultPath: path.join(app.getPath('documents'), o.defaultName || ''),
    }) || null;
  },
  messageBox(o) {
    return dialog.showMessageBoxSync(win, {
      type: o.type || 'info', message: o.message, detail: o.detail || undefined,
      buttons: o.buttons, cancelId: o.cancelId, defaultId: 0, noLink: true,
    });
  },
  setMenu(spec) { buildMenu(spec); return null; },
  // end the application (a resident one only hides its window when it is
  // closed); a resident one starts again at once, hidden, so that a client
  // shows it without waiting (a client's "stop" ends it)
  quit() {
    quitting = true;
    if (resident && !testModule) {
      app.relaunch({ args: [...process.argv.slice(1).filter((a) => a === '.' || (a.startsWith('-') && a !== '--hidden')), '--hidden'] });
    }
    setImmediate(() => app.quit());
    return null;
  },
  // (the renderer of Electron 45 has no clipboard module of its own)
  readClipboard() { return clipboard.readText(); },
  writeClipboard(text) { clipboard.writeText(String(text)); return null; },
};

ipcMain.on('desktop', (event, name, args) => {
  try {
    event.returnValue = api[name](...args);
  } catch (e) {
    console.error(`desktop.${name}:`, e);
    event.returnValue = null;
  }
});

// ---- the menu bar: the menus of the application's commands, then the
// standard ones

function buildMenu({ menus, labels }) {
  const mac = process.platform === 'darwin';
  // an item is a separator, an item with a menu of its own, or one that is
  // chosen (with a check mark if `checked` is a boolean)
  const itemTemplate = (item) => {
    if (item.separator) return { type: 'separator' };
    if (item.items) return { label: item.label, enabled: item.enabled, submenu: item.items.map(itemTemplate) };
    return {
      label: item.label,
      accelerator: item.key || undefined,
      enabled: item.enabled,
      ...(typeof item.checked === 'boolean' ? { type: 'checkbox', checked: item.checked } : {}),
      click: () => win && win.webContents.send('menu', item.id),
    };
  };
  const template = menus.map((menu) => ({ label: menu.label, submenu: menu.items.map(itemTemplate) }));
  const quit = mac ? { role: 'close' } : { role: 'quit', ...(labels.quit ? { label: labels.quit } : {}) };
  if (template.length > 0) template[0].submenu.push({ type: 'separator' }, quit);
  else template.push({ role: 'fileMenu' });
  Menu.setApplicationMenu(Menu.buildFromTemplate([
    ...(mac ? [{ role: 'appMenu' }] : []),
    ...template,
    { role: 'editMenu', ...(labels.edit ? { label: labels.edit } : {}) },
    { role: 'viewMenu', ...(labels.view ? { label: labels.view } : {}) },
  ]));
}

// ---- the window

function createWindow() {
  const background = options.backgroundColor || {};
  win = new BrowserWindow({
    width: options.width || 800,
    height: options.height || 600,
    minWidth: options.minWidth,
    minHeight: options.minHeight,
    backgroundColor: (nativeTheme.shouldUseDarkColors ? background.dark : background.light) || undefined,
    // (a resident application shows its window when it is ready, or when a
    // client asks)
    show: !testModule && !resident,
    webPreferences: {
      preload: path.join(__dirname, 'preload.js'),
      // (a hidden resident application keeps running its timers)
      backgroundThrottling: !resident,
      ...(options.node ? { nodeIntegration: true, nodeIntegrationInWorker: true, contextIsolation: false, sandbox: false } : {}),
      ...(testModule ? { offscreen: true } : {}),
    },
  });
  win.on('closed', () => { win = null; });
  if (resident) {
    win.on('close', (event) => {
      if (!quitting) { event.preventDefault(); win.hide(); }
    });
  }
  win.loadURL('app://app/index.html');
  if (testModule) require(path.resolve(testModule))(win, app);
  // (CLYSM_STARTUP_LOG=FILE: for measuring how long the application takes to
  // start, the milliseconds from CLYSM_STARTUP_T0 (a time of Date.now()), or
  // from the start of the process, to the page's editor on the screen are
  // appended to FILE)
  if (process.env.CLYSM_STARTUP_LOG) {
    const t0 = Number(process.env.CLYSM_STARTUP_T0) || Date.now() - process.uptime() * 1000;
    // (and why the page did not start from its heap image, if it did not)
    win.webContents.on('console-message', (event) => {
      if (/heap image/.test(event.message)) fs.appendFileSync(process.env.CLYSM_STARTUP_LOG, `  ${event.message}\n`);
    });
    win.webContents.once('did-finish-load', async () => {
      for (let i = 0; i < 6000; i++) {
        if (await win.webContents.executeJavaScript("document.querySelector('.lem-line') !== null")) {
          fs.appendFileSync(process.env.CLYSM_STARTUP_LOG, `editor shown at ${Date.now() - t0}\n`);
          return;
        }
        await new Promise((resolve) => setTimeout(resolve, 10));
      }
    });
  }
}

// ---- a resident application: the socket of its clients

const pendingFiles = [];
let pageReady = false;

function openFiles(files) {
  if (files.length === 0) return;
  if (pageReady) win.webContents.send('open', files);
  else pendingFiles.push(...files);
}

function showWindow() {
  if (!win) createWindow();
  if (win.isMinimized()) win.restore();
  win.show();
  win.focus();
  // (CLYSM_RESIDENT_LOG: when, for measuring how fast a client shows it)
  if (process.env.CLYSM_RESIDENT_LOG) {
    console.log('shown', Date.now());
    // (a hidden page draws no frames: the second frame after showing it)
    win.webContents.executeJavaScript('new Promise((r) => requestAnimationFrame(() => requestAnimationFrame(() => r(Date.now()))))')
      .then((t) => console.log('painted', t));
  }
}

function serveClients() {
  const socket = path.join(app.getPath('userData'), 'resident.sock');
  try { fs.unlinkSync(socket); } catch {}
  const server = netServer.createServer((connection) => {
    let text = '';
    connection.on('data', (data) => { text += data; });
    // the client goes on at once: its lines are done after the connection
    // is closed (showing the window takes a while on some systems)
    connection.on('end', () => {
      connection.end();
      setImmediate(() => {
        for (const line of text.split('\n')) {
          if (line === 'show') showWindow();
          else if (line === 'hide') { if (win) win.hide(); }
          else if (line === 'stop') { quitting = true; app.quit(); }
          else if (line.startsWith('open ')) openFiles([line.slice(5)]);
        }
      });
    });
    connection.on('error', () => {});
  });
  server.listen(socket);
  app.on('will-quit', () => { server.close(); try { fs.unlinkSync(socket); } catch {} });
}

// the page is ready for files when it asks for them
ipcMain.on('open-ready', () => {
  pageReady = true;
  if (pendingFiles.length) win.webContents.send('open', pendingFiles.splice(0));
});

app.whenReady().then(() => {
  protocol.handle('app', (request) => {
    const file = path.join(__dirname, decodeURIComponent(new URL(request.url).pathname));
    if (!file.startsWith(__dirname + path.sep)) return new Response('not found', { status: 404 });
    return net.fetch(pathToFileURL(file).toString());
  });
  if (resident) {
    // one resident application: a second start shows it (and opens its files)
    if (!app.requestSingleInstanceLock()) { app.quit(); return; }
    app.on('second-instance', (event, argv) => {
      openFiles(argvFiles(argv));
      if (!argv.includes('--hidden')) showWindow();
    });
    serveClients();
  }
  openFiles(argvFiles(process.argv));
  createWindow();
  if (resident && !startHidden && !testModule) win.once('ready-to-show', () => win.show());
  app.on('activate', () => showWindow());
});

app.on('before-quit', () => { quitting = true; });
app.on('window-all-closed', () => { if (process.platform !== 'darwin' && !resident) app.quit(); });
