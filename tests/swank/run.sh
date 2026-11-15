#!/bin/sh
# `clysm swank` (SWANK from an existing Quicklisp installation) and a client
# that talks to it as SLIME does (tests/swank/client.mjs).
cd "$(dirname "$0")/../.." || exit 1
dir=build/swank-test
rm -rf "$dir" && mkdir -p "$dir"
bin/clysm swank --port 0 > "$dir/server.log" 2>&1 < /dev/null &
server=$!
port=""
for i in $(seq 300); do
  port=$(sed -n 's/^;; Swank started at port: \([0-9]*\)\..*/\1/p' "$dir/server.log")
  [ -n "$port" ] && break
  kill -0 $server 2>/dev/null || break
  sleep 1
done
if [ -z "$port" ]; then
  echo "swank: the server did not start"; cat "$dir/server.log"; kill $server 2>/dev/null; exit 1
fi
timeout 600 node tests/swank/client.mjs "$port" "$PWD/$dir"
status=$?
kill $server 2>/dev/null
[ $status -eq 0 ] || { echo "--- server log"; tail -40 "$dir/server.log"; }
exit $status
