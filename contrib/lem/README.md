# Lem on Clysm

[Lem](https://github.com/lem-project/lem), the Common Lisp editor, running on
Clysm in an Electron window, with a frontend written with Clysm's GUI
framework (`ui/`).

```
$ contrib/lem/build.sh                     # builds build/lem-desktop/
$ (cd build/lem-desktop && npm start)
$ make test-lem                            # types into it (and into the page below) without a display
```

For a browser, without a server:

```
$ contrib/lem/build-web.sh                 # build/lem-web/lem.html, one file of 15 MB
```

Open `build/lem-web/lem.html` in Chrome or Edge 137 or later (it needs
JavaScript Promise Integration, which Firefox and Safari do not have yet),
from the disk or from any web server. See [In a browser](#in-a-browser).

`build.sh` needs SBCL, Node.js, git, npm and Quicklisp (`~/quicklisp/`, or
`QUICKLISP_HOME`). It clones Lem (the commit it was written for, or the
checkout named by `LEM_DIR`) and lem-mailbox into `build/lem-deps/`, builds
the image, compiles Lem and its libraries with it (Quicklisp downloads what
is missing; the first time this takes a few minutes), and installs Electron
in `build/lem-desktop/`. Lem then starts in about five seconds.

## How it works

**The image** (`build/lem-desktop/lem.wasm`) contains Clysm's compiler and
the GUI framework. The page (`desktop/index.html`) runs it with the files of
the computer (`host/web/clysm.mjs` with Node.js's `fs`; the Electron shell
gives the page Node.js when `package.json` says `"node": true`), and
`desktop/boot.lisp` loads Lem and the frontend with ASDF from the fasls that
`build.sh` compiled — `prepare.lisp` ran the same image on Node.js and wrote
where the systems are into `config.lisp`.

**One thread.** Lem runs its editor in a thread that waits for events in a
queue, which the frontend's input thread and timer threads fill. A page has
one thread (`src/single-thread.lisp`):

* the editor is a computation started from the event loop (Clysm's
  Bordeaux Threads, `contrib/bordeaux-threads/`, start a "thread" with
  `bt2:*thread-starter*`, here a `js:async-function` called by `setTimeout`);
* where the editor waits for the queue, it waits with `js:await` for a
  promise that sending an event resolves (or a timeout);
* meanwhile the page runs: the listeners of the keyboard send keys, the GUI
  framework updates the screen;
* timers are JavaScript timers that send their functions to the editor, and
  a background job runs at once.

**The screen** (`src/display.lisp`). A view of Lem (a window) is a model of
the GUI framework; `render-line` turns Lem's drawing objects into segments
`(column width text style)` of the view's lines, and `update-display` gives
the lines of the views that changed to their observed slots. The components
that show them are rendered again; a line equal to the one before is not, so
typing a key changes one line of the page. Floating windows have borders and
lie above the others; windows side by side have a line between them.

**Input** (`src/input.lisp`). Keys are typed into a text field that follows
the cursor, so that an input method shows its composition there: keys with a
modifier or without a character come from `keydown`, characters from
`input`, and a composed text from `compositionend`. The mouse clicks, drags
and scrolls in cells.

**Lisp** (`src/lisp-mode.lisp`). Lem's Lisp mode talks to another Lisp
process through micros; here Lem runs in a Lisp, so the Lisp mode evaluates
in that image, which compiles what it evaluates to WebAssembly. Highlighting
and indentation are Lem's (`lem-lisp-syntax`, the grammar of `lem-lisp-mode`).

| Key | Command |
|---|---|
| `C-M-x`, `C-c C-c` | evaluate the top-level form |
| `C-x C-e`, `C-c C-e` | evaluate the form before the cursor |
| `C-c C-r`, `C-c C-l` | evaluate the region, the buffer |
| `M-:` | evaluate a form read from the prompt |
| `C-c C-z`, `M-x lisp-repl` | a REPL of the image: what it evaluates changes the running editor |

## In a browser

The demo at https://cxxxr.github.io/clysm/ is this page, built by the
workflow `.github/workflows/lem-demo.yml` when `main` changes. It opens
`web/welcome.lisp` (copied into the home directory the first time), which
tells what to try.

`build-web.sh` makes one HTML file (`web/index.html` is its template) that
contains the host (`host/*.mjs`, put into the page: a page opened from a file
cannot import modules), the image and a pack of files, both compressed and in
base64:

* `build-web.sh` loads Lem on Node.js as the page will (`web/boot.lisp`),
  with `CLYSM_TRACE_FILES` (`host/run.mjs`) noting the files it reads and
  looks at; `web/pack.mjs` packs those it reads (the fasls, the `.asd`
  files...) and the names and times of the others (ASDF compares the times
  of sources and fasls). About 67 MB, 9 MB compressed.
* The page makes a file system in memory from the pack (`host/web/vfs.mjs`,
  the part of Node.js's `fs` that Clysm uses), and a `process` with the home
  directory of the build, then runs the image as the desktop application
  does. Files written in the home directory are kept in the browser's
  `localStorage`.
* `M-x open-local-file` copies a file of the computer into the home
  directory (the browser's file chooser) and opens it; `M-x download-file`
  saves the buffer to the computer (a download).
* The browser's clipboard cannot be read without waiting: Command-V,
  Control-Shift-V and Shift-Insert paste with the browser's paste event,
  and `C-y` yanks Lem's own kill ring.

## What Lem on Clysm does without

Lem's core (`lem/core`), Paredit and the Lisp mode above are loaded; the
other extensions are not. Libraries that Clysm cannot provide are replaced by
stubs (`stubs/`): micros (the parts Lem's core uses), log4cl (log messages go
to the console of the page), dexador and usocket (no network client:
the DeepL commands fail), deploy (only named by `lem.asd`).
`setup.lisp` drops lem-extension-manager, which installs packages at run time.

* No subprocesses (`uiop:run-program`): `grep`, shells, language servers,
  `git` and `find` do not work.
* Files are read and written as UTF-8. In a browser, the files are those of
  the pack and what was written in the home directory.
* One thread: a long computation stops the editor until it ends, and `C-g`
  cannot interrupt it.

## Files

| Path | Contents |
|---|---|
| `lem-clysm.asd`, `src/` | the frontend: `single-thread`, `files` (file times), `display`, `input`, `frontend` (the `lem-if` protocol), `browser` (files of the computer), `lisp-mode` |
| `setup.lisp` | the stubs and the fixups of Lem's system definitions for ASDF |
| `prepare.lisp` | compiles Lem with the image (run by `build.sh` on Node.js) |
| `desktop/` | the page, `boot.lisp` and `package.json` of the application |
| `web/` | the page for a browser (`index.html`, `boot.lisp`), `pack.mjs`, `make-html.mjs` |
| `lem.css` | the look of both pages |
| `build.sh`, `build-web.sh` | build `build/lem-desktop/`, `build/lem-web/lem.html` |
| `../../tests/lem/` | `make test-lem` |
