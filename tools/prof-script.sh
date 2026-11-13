#!/bin/sh
# Like tools/prof.sh for a file loaded into the REPL image (bin/clysm script):
# every toplevel form is compiled at run time.
#   tools/prof-script.sh FILE.lisp [N]
set -e
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT
IMG="$("$ROOT/bin/clysm" image-native)"
node --cpu-prof --cpu-prof-dir="$TMP/prof" "$ROOT/host/run.mjs" "$IMG" "$1" > "$TMP/out.txt" || true
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
