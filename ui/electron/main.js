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
const { app, BrowserWindow, Menu, dialog, ipcMain, nativeTheme, net, protocol } = require('electron');
const fs = require('node:fs');
const path = require('node:path');
const { pathToFileURL } = require('node:url');

const options = require('./package.json').window || {};

// the page is served from app://app/ (fetch, which loads the .wasm file,
// does not work on file: URLs)
protocol.registerSchemesAsPrivileged([
  { scheme: 'app', privileges: { standard: true, secure: true, supportFetchAPI: true } },
]);

// CLYSM_USER_DATA replaces the directory of the application's data (for tests)
if (process.env.CLYSM_USER_DATA) app.setPath('userData', process.env.CLYSM_USER_DATA);

let win = null;

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
  const template = menus.map((menu) => ({
    label: menu.label,
    submenu: menu.items.flatMap((item) => [
      ...(item.separator ? [{ type: 'separator' }] : []),
      {
        label: item.label,
        accelerator: item.key || undefined,
        enabled: item.enabled,
        click: () => win && win.webContents.send('menu', item.id),
      },
    ]),
  }));
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
    webPreferences: { preload: path.join(__dirname, 'preload.js') },
  });
  win.on('closed', () => { win = null; });
  win.loadURL('app://app/index.html');
}

app.whenReady().then(() => {
  protocol.handle('app', (request) => {
    const file = path.join(__dirname, decodeURIComponent(new URL(request.url).pathname));
    if (!file.startsWith(__dirname + path.sep)) return new Response('not found', { status: 404 });
    return net.fetch(pathToFileURL(file).toString());
  });
  createWindow();
  app.on('activate', () => { if (!win) createWindow(); });
});

app.on('window-all-closed', () => { if (process.platform !== 'darwin') app.quit(); });
