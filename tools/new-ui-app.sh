#!/bin/sh
# Make a new application with the UI framework, anywhere:
#   tools/new-ui-app.sh DIRECTORY [NAME]
# DIRECTORY gets
#   app.lisp            the program (a window with a button)
#   index.html          the page that loads it
#   build.sh            compiles it into DIRECTORY/build/ (--dev: with a REPL)
#   build-desktop.sh    makes the desktop application in DIRECTORY/build-desktop/
#   desktop/package.json  the name and window of the desktop application
# The scripts find Clysm through the variable CLYSM, by default this checkout.
set -e
clysm="$(cd "$(dirname "$0")/.." && pwd)"
dir="$1"
[ -n "$dir" ] || { echo "usage: $0 DIRECTORY [NAME]" >&2; exit 1; }
name="${2:-$(basename "$dir")}"
package="$(echo "$name" | tr 'a-z' 'A-Z' | tr -c 'A-Z0-9\n' '-')"
[ -e "$dir/app.lisp" ] && { echo "$dir/app.lisp exists" >&2; exit 1; }
mkdir -p "$dir/desktop"

cat > "$dir/app.lisp" <<EOF
;;;; app.lisp — $name

(defpackage "$package"
  (:use "CL" "UI"))

(in-package "$package")

(define-interface main-window ()
  ((count :initform 0 :accessor click-count))
  (:panes
   (message title-pane
            :text (computed (format nil "~D clicks" (click-count interface))))
   (button push-button :text "Click me"
           :callback (lambda (data interface)
                       (declare (ignore data))
                       (incf (click-count interface)))))
  (:layouts
   (main column-layout '(message button)))
  (:default-initargs :title "$name"))

(display (make-instance 'main-window))
(install-commands *backend*)
EOF

cat > "$dir/index.html" <<EOF
<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>$name</title>
<link rel="stylesheet" href="ui.css">
<style>
  html, body, #app { height: 100%; margin: 0; }
  body { background: var(--ui-bg); }
  #status { position: fixed; inset: 0; display: grid; place-items: center; color: var(--ui-muted); font: var(--ui-font); }
</style>
</head>
<body>
<div id="status">Loading…</div>
<div id="app"></div>
<script type="module">
  import { loadClysm } from './clysm.mjs';
  const status = document.getElementById('status');
  try {
    const bytes = await (await fetch('app.wasm')).arrayBuffer();
    // what the program prints goes to the console
    let io = { write: (text) => console.log(text), readLine: () => null };
    const lisp = await loadClysm(bytes, (text, fd) => io.write(text, fd), { readLine: () => io.readLine() });
    status.remove();
    // the development build (build.sh --dev) has a REPL
    if (globalThis.clysmUiRefresh) {
      const { dockRepl } = await import('./repl.mjs');
      io = dockRepl({ afterEval: globalThis.clysmUiRefresh });
      io.info('The application runs in this image: (in-package :$(echo "$package" | tr 'A-Z' 'a-z'))\n');
      io.start(lisp, { focus: false });
    }
  } catch (e) {
    status.textContent = 'Error: ' + e.message;
    throw e;
  }
</script>
</body>
</html>
EOF

cat > "$dir/build.sh" <<'EOF'
#!/bin/sh
# Build the application into build/ (serve that directory over HTTP):
#   ./build.sh && python3 -m http.server -d build
# With --dev, the page has a REPL in which the running program is changed.
# CLYSM is the directory of Clysm.
set -e
cd "$(dirname "$0")"
EOF
cat >> "$dir/build.sh" <<EOF
CLYSM="\${CLYSM:-$clysm}"
EOF
cat >> "$dir/build.sh" <<'EOF'
UI="$CLYSM/ui"
mkdir -p build
rm -f build/repl.mjs
if [ "$1" = "--dev" ]; then
  "$CLYSM/tools/build-ui-dev.sh" build/app.wasm app.lisp
  cp "$CLYSM/host/web/repl.mjs" build/
else
  "$CLYSM/bin/clysm" compile "$UI/ui.lisp" "$UI/command.lisp" "$UI/present.lisp" "$UI/pane.lisp" \
    "$UI/dialog.lisp" "$UI/graphics.lisp" "$UI/stream.lisp" "$UI/dom.lisp" "$UI/desktop.lisp" \
    app.lisp -o build/app.wasm 2>build/compile.log || { cat build/compile.log; exit 1; }
fi
cp index.html "$UI/ui.css" "$CLYSM/host/common.mjs" "$CLYSM/host/jsffi.mjs" build/
sed "s#'../common.mjs'#'./common.mjs'#" "$CLYSM/host/web/clysm.mjs" > build/clysm.mjs
echo "built build/ — serve it with e.g.: python3 -m http.server -d build"
EOF

cat > "$dir/build-desktop.sh" <<'EOF'
#!/bin/sh
# Build the desktop application into build-desktop/ and install Electron there:
#   ./build-desktop.sh && (cd build-desktop && npm start)
set -e
cd "$(dirname "$0")"
EOF
cat >> "$dir/build-desktop.sh" <<EOF
CLYSM="\${CLYSM:-$clysm}"
EOF
cat >> "$dir/build-desktop.sh" <<'EOF'
CLYSM="$CLYSM" ./build.sh "$@" >/dev/null
mkdir -p build-desktop
rm -f build-desktop/repl.mjs
cp build/* "$CLYSM/ui/electron/main.js" "$CLYSM/ui/electron/preload.js" desktop/package.json build-desktop/
[ -d build-desktop/node_modules/electron ] || (cd build-desktop && npm install --no-audit --no-fund)
echo "built build-desktop/ — run it with: (cd build-desktop && npm start)"
EOF

cat > "$dir/desktop/package.json" <<EOF
{
  "name": "$(echo "$name" | tr 'A-Z' 'a-z' | tr -c 'a-z0-9\n' '-')",
  "productName": "$name",
  "version": "0.1.0",
  "main": "main.js",
  "private": true,
  "window": {
    "width": 800,
    "height": 600,
    "backgroundColor": { "light": "#f4f4f2", "dark": "#1b1c1e" }
  },
  "scripts": { "start": "electron ." },
  "devDependencies": { "electron": "45.3.1" }
}
EOF

chmod +x "$dir/build.sh" "$dir/build-desktop.sh"
echo "made $dir: build it with $dir/build.sh"
