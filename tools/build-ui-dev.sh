#!/bin/sh
# Build the development image of an application written with the UI framework:
#   tools/build-ui-dev.sh OUT.wasm APP.lisp...
# The image contains the compiler, the framework (with ui/dev.lisp) and the
# application.  A page that loads it docks a REPL with host/web/repl.mjs; what
# is evaluated there is compiled in the page and changes the running application.
set -e
root="$(cd "$(dirname "$0")/.." && pwd)"
out="$1"; shift
CLYSM_REDEFINABLE=1 exec sbcl --script "$root/tools/build-repl.lisp" "$out" \
  "$root/lib/web-main.lisp" "$root/ui/ui.lisp" "$root/ui/command.lisp" "$root/ui/present.lisp" \
  "$root/ui/pane.lisp" "$root/ui/dialog.lisp" "$root/ui/dom.lisp" \
  "$root/ui/desktop.lisp" "$root/ui/dev.lisp" "$@"
