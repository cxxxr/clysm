# Lem on Clysm

[Lem](https://github.com/lem-project/lem), the Common Lisp editor, running on
Clysm in an Electron window, with a frontend written with Clysm's GUI
framework (`ui/`).

```
$ contrib/lem/build.sh                     # builds build/lem-desktop/
$ build/lem-desktop/lem [FILE...]          # starts it, or shows it at once
$ make test-lem                            # types into it (and into the page below) without a display
```

For a browser, without a server:

```
$ contrib/lem/build-web.sh                 # build/lem-web/lem.html, one file of 22 MB
```

Open `build/lem-web/lem.html` in Chrome or Edge 137 or later (it needs
JavaScript Promise Integration, which Firefox and Safari do not have yet),
from the disk or from any web server. See [In a browser](#in-a-browser).

`build.sh` needs SBCL, Node.js, git, npm and Quicklisp (`~/quicklisp/`, or
`QUICKLISP_HOME`). It clones Lem (the commit it was written for, or the
checkout named by `LEM_DIR`), lem-mailbox and micros into `build/lem-deps/`, builds
the image, compiles Lem and its libraries with it (Quicklisp downloads what
is missing; the first time this takes a few minutes), and installs Electron
in `build/lem-desktop/`.

**Starting at once.** Lem takes about six seconds to start: the image is
made again (WebAssembly GC objects cannot be saved), and Lem's 445 fasls are
loaded. So the application stays resident, as Emacs does with `emacs
--daemon` and `emacsclient`: closing its window only hides it (`C-x C-c`
ends Lem), and `build/lem-desktop/lem` tells it through a Unix domain socket
to open files and show its window, which takes less than 10 ms (`make
test-lem` measures it: 8 ms until the file is on the screen). Only the first
`lem` waits for Lem to start; `lem --daemon` starts it with its window
hidden (e.g. when you log in), so that every `lem` after it is instant. The
socket is `resident.sock` in the application's data directory
(`~/.config/Lem/`); see "resident" in `ui/electron/main.js`.

## How it works

**The image** (`build/lem-desktop/lem.wasm`) contains Clysm's compiler and
the GUI framework. The page (`desktop/index.html`) runs it with the files of
the computer (`host/web/clysm.mjs` with Node.js's `fs`; the Electron shell
gives the page Node.js when `package.json` says `"node": true`), and
`desktop/boot.lisp` loads Lem and the frontend with ASDF from the fasls that
`build.sh` compiled — `prepare.lisp` ran the same image on Node.js and wrote
where the systems are into `config.lisp`.

**One thread.** Lem runs its editor in a thread that waits for events in a
queue, which the frontend's input thread and timer threads fill, and its
Lisp mode has a thread that waits for micros's messages. A page has one
thread (`src/single-thread.lisp`):

* Lem's threads, and micros's, are Clysm's green threads
  (`runtime/thread.lisp`): computations that take turns when they wait,
  suspended by JavaScript Promise Integration;
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

**Lisp** (`src/micros.lisp`). The Lisp mode is Lem's own: completion,
argument lists as you type, `M-.`, describe, the inspector, the debugger,
macroexpansion and the REPL are answers of micros (Lem's SWANK). Lem starts
a micros server in its own image and connects to it (its self-connect);
here, without sockets, the server listens and the Lisp mode connects with
the sockets of `stubs/usocket/`, which connect within the image: a
connection is two pipes, and a reader waits (a green thread) until the
other end writes. micros's backend for Clysm is `contrib/micros/clysm.lisp`
(from that of SWANK). What micros evaluates is compiled to WebAssembly, in
a thread of its own: the editor goes on meanwhile, and code compiled once
the connection is made checks for interrupts, so that `C-c C-c` in the REPL
interrupts a computation (the debugger) and long ones take turns with the
editor. In a browser the page has the sources of Lem, micros, this frontend
and Clysm's runtime, so that `M-.` opens them.

**Extensions** (`src/extensions.lisp`). Lem's language modes (C, CSS, Go,
HTML, JavaScript, JSON, Makefile, Markdown, shell, Python, Rust, SQL, TOML,
TypeScript, XML, YAML), vi-mode and the base16 color themes (about 190) are
compiled with the rest but loaded when first used, so that they do not make
Lem start later: the file types of a mode open their files with a function
that loads the mode's system with ASDF and then turns the mode on, a command
of the mode's name does the same, and `load-theme` loads the themes first
(also before Lem starts, when the theme saved last time is one of them).
`setup.lisp` leaves out what they cannot have here: their language servers,
tree-sitter (`stubs/lem-tree-sitter/`) and the preview of Markdown.

## In a browser

The demo at https://cxxxr.github.io/clysm/ is this page, built by the
workflow `.github/workflows/lem-demo.yml` (run by hand). It opens
`web/welcome.lisp`, a short tour, copied into the home directory (and
replaced by a newer one as long as it was not changed there).

While Lem loads (about 3 s of the 5), the page shows how far it is:
`web/boot.lisp` loads Lem as a computation that lets the page draw itself
between files (`js:await` of the next frame, at most every 50 ms), and calls
`lemBoot.progress` with the number of files loaded; `build-web.sh` counts
them when it loads Lem on Node.js. A browser without JavaScript Promise
Integration gets a page that says so.

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

Lem's core (`lem/core`), Paredit, the Lisp mode above and the extensions
above are there; the others are not (those that run programs, the
language server client, the terminal, legit...). Libraries that Clysm cannot provide are replaced by
stubs (`stubs/`): log4cl (log messages go to the console of the page),
dexador (no network client: the DeepL commands fail), usocket (sockets
within the image, above), lem-process (no programs: `M-x slime` cannot
start another Lisp), deploy (only named by `lem.asd`).
`setup.lisp` drops lem-extension-manager, which installs packages at run time.

* No subprocesses (`uiop:run-program`): `grep`, shells, language servers,
  `git` and `find` do not work.
* Files are read and written as UTF-8. In a browser, the files are those of
  the pack and what was written in the home directory.
* A long computation of the editor itself (not of what the Lisp mode
  evaluates) stops it until it ends.

## Files

| Path | Contents |
|---|---|
| `lem-clysm.asd`, `src/` | the frontend: `single-thread`, `files` (file times), `display`, `input`, `frontend` (the `lem-if` protocol), `browser` (files of the computer), `micros` (the server of the Lisp mode), `extensions` (loaded when first used) |
| `setup.lisp` | the stubs and the fixups of Lem's system definitions for ASDF (and of the extensions) |
| `prepare.lisp` | compiles Lem with the image (run by `build.sh` on Node.js) |
| `desktop/` | the page, `boot.lisp` and `package.json` of the application, and `lem`, its client |
| `web/` | the page for a browser (`index.html`, `boot.lisp`, `welcome.lisp`), `pack.mjs`, `make-html.mjs` |
| `lem.css` | the look of both pages |
| `build.sh`, `build-web.sh` | build `build/lem-desktop/`, `build/lem-web/lem.html` |
| `../../tests/lem/` | `make test-lem` |
