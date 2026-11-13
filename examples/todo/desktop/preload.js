// The API the Lisp program uses in the desktop app: the global `desktop`.
// Every call returns at once (synchronous IPC), so Lisp code can use the result.
const { contextBridge, ipcRenderer } = require('electron');

contextBridge.exposeInMainWorld('desktop', {
  // the list file used last: { path, text } (text is null if it does not exist)
  load: () => ipcRenderer.sendSync('load'),
  // write TEXT to the file PATH
  save: (path, text) => ipcRenderer.sendSync('save', path, text),
  // ask for a file to open: { path, text }, or null
  open: () => ipcRenderer.sendSync('open'),
  // ask for a file name to save to: a path, or null
  chooseFile: (title) => ipcRenderer.sendSync('choose-file', title),
  // remember PATH as the list file
  setFile: (path) => ipcRenderer.sendSync('set-file', path),
  // call FN with the name of a menu command: "new", "open" or "save-as"
  onMenu: (fn) => { ipcRenderer.on('menu', (_event, name) => fn(name)); },
});
