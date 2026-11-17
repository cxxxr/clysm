# Worlds

A world is a Clysm image of its own in a worker of the page, with a
[micros](https://github.com/lem-project/micros) server (Lem's SWANK). The
editor in the page connects to it as it would to a Lisp on another machine,
so what is evaluated runs outside the editor's image and thread:

* a computation that never ends does not stop the editor, and `C-c C-c`
  interrupts it;
* a world can be killed whatever it does, and another started in about a
  second and a half;
* what is defined in a world is not defined in the editor.

In Lem on Clysm (`contrib/lem/`, the desktop application):

```
M-x world-start     start a world and connect the Lisp mode to it
M-x world-kill      end the world of the current connection
```

After `world-start` the REPL, `C-M-x`, `C-c C-k`, completion, argument
lists, `M-.`, the debugger and the inspector work in the world. After
`world-kill` the Lisp mode is back in the editor's image, or in another
world it is still connected to. The lines that a world writes to its
standard output outside the REPL are in the buffer `*worlds*`.

From Lisp:

```lisp
(clysm-world:start-world :on-ready (lambda (world) ...))   ; => a world, :starting
(clysm-world:world-state world)                            ; :starting, :ready, :failed, :dead
(usocket:socket-connect (clysm-world:world-name world) (clysm-world:world-port world))
(clysm-world:kill-world world)
```

## How it works

**The worker** (`worker.js`) is started by the page with the compiled
WebAssembly module of the page's own image (a module can be sent to a
worker, which then does not compile the 20 MB again) and runs it with
`boot.lisp`. That loads micros and this system with ASDF from the fasls of
the application's build, starts the micros server and tells the page that
the world is ready. In Electron the worker has Node.js
(`nodeIntegrationInWorker`), so the world has the files of the computer.

**Connections** (`world.lisp`). The sockets are those of
`contrib/lem/stubs/usocket/`, which connect within an image through pipes.
A connection to a world is the same socket at each end, but the pipe that
an end writes gives its octets to a function, which posts them to the other
side as a message (`data`), where they are written into the pipe that the
other end reads. `usocket:socket-connect` to the name of a world sends
`connect` with a new id, and the world offers the connection to the server
that listens on the port. A world serves any number of connections.

**Threads.** micros's threads in the world are green threads, and the
messages of the page arrive when the world's thread of the computer is free:
when its Lisp threads wait, or, in code compiled after a connection was made
(which checks for interrupts), every 10 ms. Code compiled without the
checks that loops is not interrupted by `C-c C-c`; `world-kill` ends it.

## Limits

* The desktop application only: in a browser the files are in the page's
  memory, which a worker does not have.
* A world starts from the image and loads micros each time (WebAssembly GC
  objects cannot be saved): what was evaluated in a killed world is gone.
* A recursion deeper than the stack ends the evaluation without an answer,
  as it does in the editor's image.
