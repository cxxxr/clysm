#!/bin/sh
# Profile a Lisp program on Clysm: compile FILE, run it under the V8 CPU
# profiler and print the functions with the most self time.
#   tools/prof.sh FILE.lisp [N]       (N: number of lines, default 40)
set -e
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT
"$ROOT/bin/clysm" compile "$1" -o "$TMP/p.wasm" >/dev/null 2>&1
node --cpu-prof --cpu-prof-dir="$TMP/prof" "$ROOT/host/run.mjs" "$TMP/p.wasm" > "$TMP/out.txt" || true
tail -3 "$TMP/out.txt"
node -e '
const fs=require("fs"), dir=process.argv[1], top=+process.argv[2];
const self=new Map(); let total=0;
for (const f of fs.readdirSync(dir)) {
  const p=JSON.parse(fs.readFileSync(dir+"/"+f)); const byId=new Map(p.nodes.map(n=>[n.id,n]));
  const dt=p.timeDeltas; p.samples.forEach((id,i)=>{ const n=byId.get(id); const k=n.callFrame.functionName||"(anon)";
    self.set(k,(self.get(k)||0)+dt[i]); total+=dt[i]; });
}
const rows=[...self].sort((a,b)=>b[1]-a[1]).slice(0,top);
for (const [k,v] of rows) console.log((v/1000).toFixed(0).padStart(8)+" ms "+(100*v/total).toFixed(1).padStart(5)+"%  "+k);
' "$TMP/prof" "${2:-40}"
