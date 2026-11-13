// The Electron main process of the desktop TODO app.  The application itself
// is todo.lisp (compiled to todo.wasm) running in the window; this file opens
// the window, builds the menu and does what a page cannot: files and dialogs.
const { app, BrowserWindow, Menu, dialog, ipcMain, nativeTheme, net, protocol } = require('electron');
const fs = require('node:fs');
const path = require('node:path');
const { pathToFileURL } = require('node:url');

// the page is served from app://todo/ (fetch, which loads todo.wasm, does not
// work on file: URLs)
protocol.registerSchemesAsPrivileged([
  { scheme: 'app', privileges: { standard: true, secure: true, supportFetchAPI: true } },
]);

let win = null;

// CLYSM_TODO_USER_DATA replaces the settings directory (for tests)
if (process.env.CLYSM_TODO_USER_DATA) app.setPath('userData', process.env.CLYSM_TODO_USER_DATA);

// ---- the list file: settings.json remembers the one used last

const settingsFile = () => path.join(app.getPath('userData'), 'settings.json');
const defaultFile = () => path.join(app.getPath('userData'), 'todos.json');

function lastFile() {
  try {
    const file = JSON.parse(fs.readFileSync(settingsFile(), 'utf8')).file;
    if (typeof file === 'string') return file;
  } catch {}
  return defaultFile();
}

function readText(file) {
  try { return fs.readFileSync(file, 'utf8'); } catch { return null; }
}

const filters = [{ name: 'TODO リスト', extensions: ['json'] }];

// the API of preload.js; the calls are synchronous for the Lisp program
ipcMain.on('load', (e) => {
  const file = lastFile();
  e.returnValue = { path: file, text: readText(file) };
});
ipcMain.on('save', (e, file, text) => {
  try {
    fs.mkdirSync(path.dirname(file), { recursive: true });
    fs.writeFileSync(file, text);
    e.returnValue = true;
  } catch (err) {
    dialog.showErrorBox('保存できませんでした', `${file}\n${err.message}`);
    e.returnValue = false;
  }
});
ipcMain.on('open', (e) => {
  const files = dialog.showOpenDialogSync(win, { properties: ['openFile'], filters });
  e.returnValue = files ? { path: files[0], text: readText(files[0]) } : null;
});
ipcMain.on('choose-file', (e, title) => {
  e.returnValue = dialog.showSaveDialogSync(win, {
    title, filters, defaultPath: path.join(app.getPath('documents'), 'todo.json'),
  }) || null;
});
ipcMain.on('set-file', (e, file) => {
  try {
    fs.mkdirSync(app.getPath('userData'), { recursive: true });
    fs.writeFileSync(settingsFile(), JSON.stringify({ file }));
  } catch {}
  app.addRecentDocument(file);
  e.returnValue = true;
});

// ---- window and menu

const command = (name) => () => win && win.webContents.send('menu', name);

function buildMenu() {
  const mac = process.platform === 'darwin';
  Menu.setApplicationMenu(Menu.buildFromTemplate([
    ...(mac ? [{ role: 'appMenu' }] : []),
    {
      label: 'ファイル',
      submenu: [
        { label: '新しいリスト…', accelerator: 'CmdOrCtrl+N', click: command('new') },
        { label: '開く…', accelerator: 'CmdOrCtrl+O', click: command('open') },
        { label: '名前を付けて保存…', accelerator: 'CmdOrCtrl+Shift+S', click: command('save-as') },
        { type: 'separator' },
        mac ? { role: 'close' } : { role: 'quit', label: '終了' },
      ],
    },
    { role: 'editMenu', label: '編集' },
    {
      label: '表示',
      submenu: [
        { role: 'reload', label: '再読み込み' },
        { role: 'toggleDevTools', label: '開発者ツール' },
        { type: 'separator' },
        { role: 'resetZoom', label: '実際のサイズ' },
        { role: 'zoomIn', label: '拡大' },
        { role: 'zoomOut', label: '縮小' },
      ],
    },
  ]));
}

function createWindow() {
  win = new BrowserWindow({
    width: 640,
    height: 720,
    minWidth: 380,
    minHeight: 400,
    title: 'TODO',
    backgroundColor: nativeTheme.shouldUseDarkColors ? '#151618' : '#f6f4ef',
    webPreferences: { preload: path.join(__dirname, 'preload.js') },
  });
  win.on('closed', () => { win = null; });
  win.loadURL('app://todo/index.html');
}

app.whenReady().then(() => {
  protocol.handle('app', (request) => {
    const file = path.join(__dirname, decodeURIComponent(new URL(request.url).pathname));
    if (!file.startsWith(__dirname + path.sep)) return new Response('not found', { status: 404 });
    return net.fetch(pathToFileURL(file).toString());
  });
  buildMenu();
  createWindow();
  app.on('activate', () => { if (!win) createWindow(); });
});

app.on('window-all-closed', () => { if (process.platform !== 'darwin') app.quit(); });
