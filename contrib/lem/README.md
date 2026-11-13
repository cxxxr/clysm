# Lem on Clysm

[Lem](https://github.com/lem-project/lem), the Common Lisp editor, running on
Clysm in an Electron window, with a frontend written with Clysm's GUI
framework (`ui/`).

```
$ contrib/lem/build.sh                     # builds build/lem-desktop/
$ (cd build/lem-desktop && npm start)
$ make test-lem                            # types into it without a display
```

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

## What Lem on Clysm does without

Lem's core (`lem/core`), Paredit and the Lisp mode above are loaded; the
other extensions are not. Libraries that Clysm cannot provide are replaced by
stubs (`stubs/`): micros (the parts Lem's core uses), log4cl (log messages go
to the console of the page), dexador and usocket (no network client:
the DeepL commands fail), deploy (only named by `lem.asd`).
`setup.lisp` drops lem-extension-manager, which installs packages at run time.

* No subprocesses (`uiop:run-program`): `grep`, shells, language servers,
  `git` and `find` do not work.
* Files are read and written as UTF-8.
* One thread: a long computation stops the editor until it ends, and `C-g`
  cannot interrupt it.

## Files

| Path | Contents |
|---|---|
| `lem-clysm.asd`, `src/` | the frontend: `single-thread`, `files` (file times), `display`, `input`, `frontend` (the `lem-if` protocol), `lisp-mode` |
| `setup.lisp` | the stubs and the fixups of Lem's system definitions for ASDF |
| `prepare.lisp` | compiles Lem with the image (run by `build.sh` on Node.js) |
| `desktop/` | the page, `boot.lisp` and `package.json` of the application |
| `build.sh` | builds `build/lem-desktop/` |
| `../../tests/lem/` | `make test-lem` |
