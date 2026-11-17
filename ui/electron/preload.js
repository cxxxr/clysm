// The global `desktop` of the page: what ui/desktop.lisp calls.  The calls are
// synchronous (they return the value), so Lisp code uses the result directly.
const { contextBridge, ipcRenderer } = require('electron');

const desktop = {
  // call the function NAME of main.js (readFile, saveDialog, setMenu...)
  call: (name, ...args) => ipcRenderer.sendSync('desktop', name, args),
  // call FN with the id of a menu item when it is chosen
  onMenu: (fn) => { ipcRenderer.on('menu', (_event, id) => fn(id)); },
  // call FN with each file that the application is asked to open (on its
  // command line, or by a client of a resident application)
  onOpen: (fn) => {
    ipcRenderer.on('open', (_event, files) => { for (const f of files) fn(f); });
    ipcRenderer.send('open-ready');
  },
};

// (a page with Node.js, "node": true in package.json, shares this context)
if (process.contextIsolated) contextBridge.exposeInMainWorld('desktop', desktop);
else window.desktop = desktop;
