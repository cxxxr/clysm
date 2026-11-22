// TypeScript ports of benchmarks of bench/suite.lisp and bench/declared.lisp, for
// tools/bench-ts.sh: the same algorithms, sizes, repetitions and warm-up as the
// Lisp versions, written as ordinary TypeScript (classes, Map, typed arrays).
// Node.js 24 runs it as it is (type stripping: no parameter properties, no enums).

function runBench(name: string, reps: number, fn: () => unknown): void {
  fn(); // warm up
  const start = performance.now();
  let r: unknown = null;
  for (let i = 0; i < reps; i++) r = fn();
  const ms = Math.round(performance.now() - start);
  console.log(`${name} ${ms} ${String(r).slice(0, 40)}`);
}

// ---------------------------------------------------------------- suite.lisp
function fib(n: number): number { return n < 2 ? n : fib(n - 1) + fib(n - 2); }
runBench("b-fib", 3, () => fib(32));

function tak(x: number, y: number, z: number): number {
  return !(y < x) ? z : tak(tak(x - 1, y, z), tak(y - 1, z, x), tak(z - 1, x, y));
}
runBench("b-tak", 20, () => tak(24, 16, 8));

function countPrimes(n: number): number {
  const sieve = new Array<boolean>(n + 1).fill(true);
  let count = 0;
  for (let i = 2; i <= n; i++) {
    if (sieve[i]) {
      count++;
      for (let j = i * i; j <= n; j += i) sieve[j] = false;
    }
  }
  return count;
}
runBench("b-sieve", 5, () => countPrimes(2000000));

function matmul(n: number): number {
  const mk = (v: number) => Array.from({ length: n }, () => new Array<number>(n).fill(v));
  const a = mk(1), b = mk(2), c = mk(0);
  for (let i = 0; i < n; i++)
    for (let j = 0; j < n; j++) {
      let s = 0;
      for (let k = 0; k < n; k++) s += a[i][k] * b[k][j];
      c[i][j] = s;
    }
  return c[n - 1][n - 1];
}
runBench("b-matmul", 20, () => matmul(120));

function floatLoop(n: number): number {
  let x = 0;
  for (let i = 0; i < n; i++) x = x * 0.999 + Math.sqrt(i);
  return x;
}
runBench("b-float", 3, () => floatLoop(2000000));

function mandel(size: number): number {
  let count = 0;
  for (let py = 0; py < size; py++)
    for (let px = 0; px < size; px++) {
      const cr = (3.0 * px) / size - 2.0, ci = (2.0 * py) / size - 1.0;
      let zr = 0, zi = 0, i = 0;
      while (i < 50 && zr * zr + zi * zi < 4) {
        const nzr = zr * zr - zi * zi + cr;
        zi = 2 * zr * zi + ci;
        zr = nzr;
        i++;
      }
      if (i === 50) count++;
    }
  return count;
}
runBench("b-mandel", 5, () => mandel(200));

type Expr = string | number | Expr[];
function deriv(a: Expr): Expr {
  if (!Array.isArray(a)) return a === "x" ? 1 : 0;
  const [op, ...args] = a;
  switch (op) {
    case "+": return ["+", ...args.map(deriv)];
    case "-": return ["-", ...args.map(deriv)];
    case "*": return ["*", a, ["+", ...args.map((b) => ["/", deriv(b), b])]];
    case "/": return ["-", ["/", deriv(args[0]), args[1]],
                      ["/", args[0], ["*", args[1], args[1], deriv(args[1])]]];
    default: return "error";
  }
}
const derivInput: Expr = ["+", ["*", 3, "x", "x"], ["*", "a", "x", "x"], ["*", "b", "x"], 5];
runBench("b-deriv", 10, () => {
  let r: Expr = [];
  for (let i = 0; i < 200000; i++) r = deriv(derivInput);
  return (r as Expr[]).length;
});

runBench("b-list", 20, () => {
  let total = 0;
  for (let k = 0; k < 50; k++) {
    let l = Array.from({ length: 10000 }, (_, i) => i);
    l = l.map((x) => x + 1);
    l = l.filter((x) => x % 2 !== 0);
    l = l.slice().reverse();
    total += l.reduce((s, x) => s + x, 0) & 0xffff;
    total += l.concat(l).length;
    total += l.filter((x) => x % 2 !== 0).length;
    if (l.includes(9999)) total++;
  }
  return total;
});

runBench("b-sort", 5, () => {
  const l = Array.from({ length: 100000 }, (_, i) => (i * 7919) % 100003).sort((a, b) => a - b);
  const v = Array.from({ length: 100000 }, (_, i) => (i * 7919) % 100003).sort((a, b) => b - a);
  return l[0] + v[0];
});

runBench("b-hash", 20, () => {
  const h = new Map<number, number>();
  let s = 0;
  for (let i = 0; i < 200000; i++) h.set(i, i * 2);
  for (let i = 0; i < 200000; i++) s = (s + h.get(i)!) & 0xffffff;
  return s;
});

runBench("b-hash-string", 30, () => {
  const h = new Map<string, number>();
  let s = 0;
  const keys = Array.from({ length: 20000 }, (_, i) => `key-${i}`);
  for (const k of keys) h.set(k, k.length);
  for (let j = 0; j < 5; j++) for (const k of keys) s += h.get(k)!;
  return s;
});

runBench("b-string", 25, () => {
  const chars = new Array<string>(100000);
  for (let i = 0; i < chars.length; i++) chars[i] = String.fromCharCode(97 + (i % 26));
  const s = chars.join("");
  let n = 0;
  for (let k = 0; k < 10; k++) {
    let e = 0;
    for (let i = 0; i < s.length; i++) if (s.charCodeAt(i) === 101) e++;
    n += e;
    n += s.toUpperCase().length;
    n += Math.max(s.indexOf("xyz"), 0);
    n += s.indexOf("z");
  }
  return n;
});

function fact(n: bigint): bigint { return n < 2n ? 1n : n * fact(n - 1n); }
runBench("b-bignum", 50, () => fact(2000n).toString(2).length);

class Pt { x: number; y: number; constructor(x: number, y: number) { this.x = x; this.y = y; } }
runBench("b-struct", 20, () => {
  const p = new Pt(0, 0);
  for (let i = 0; i < 3000000; i++) { p.x = p.x + 1; p.y = (p.y + p.x) & 0xffffff; }
  return p.y;
});

abstract class Shape { abstract area(): number; }
class Circle extends Shape { r: number; constructor(r: number) { super(); this.r = r; } area() { return 3 * this.r * this.r; } }
class Rect extends Shape { w: number; h: number; constructor(w: number, h: number) { super(); this.w = w; this.h = h; } area() { return this.w * this.h; } }
runBench("b-clos", 12, () => {
  const shapes: Shape[] = [new Circle(2), new Rect(3, 4)];
  let s = 0;
  for (let i = 0; i < 500000; i++) for (const x of shapes) s += x.area();
  return s;
});

runBench("b-make-instance", 50, () => {
  let s = 0;
  for (let i = 0; i < 100000; i++) s += new Circle(i & 1023).r;
  return s;
});

// ------------------------------------------------------------- declared.lisp
class Body {
  x: number; y: number; z: number; vx: number; vy: number; vz: number; mass: number;
  constructor(x: number, y: number, z: number, vx: number, vy: number, vz: number, mass: number) {
    this.x = x; this.y = y; this.z = z; this.vx = vx; this.vy = vy; this.vz = vz; this.mass = mass;
  }
}
const SOLAR_MASS = 4 * Math.PI * Math.PI, DAYS_PER_YEAR = 365.24;
function makeSystem(): Body[] {
  const planet = (x: number, y: number, z: number, vx: number, vy: number, vz: number, m: number) =>
    new Body(x, y, z, vx * DAYS_PER_YEAR, vy * DAYS_PER_YEAR, vz * DAYS_PER_YEAR, m * SOLAR_MASS);
  const bodies = [
    new Body(0, 0, 0, 0, 0, 0, SOLAR_MASS),
    planet(4.84143144246472090, -1.16032004402742839, -1.03622044471123109e-1,
           1.66007664274403694e-3, 7.69901118419740425e-3, -6.90460016972063023e-5, 9.54791938424326609e-4),
    planet(8.34336671824457987, 4.12479856412430479, -4.03523417114321381e-1,
           -2.76742510726862411e-3, 4.99852801234917238e-3, 2.30417297573763929e-5, 2.85885980666130812e-4),
    planet(1.28943695621391310e1, -1.51111514016986312e1, -2.23307578892655734e-1,
           2.96460137564761618e-3, 2.37847173959480950e-3, -2.96589568540237556e-5, 4.36624404335156298e-5),
    planet(1.53796971148509165e1, -2.59193146099879641e1, 1.79258772950371181e-1,
           2.68067772490389322e-3, 1.62824170038242295e-3, -9.51592254519715870e-5, 5.15138902046611451e-5),
  ];
  let px = 0, py = 0, pz = 0;
  for (const b of bodies) { px += b.vx * b.mass; py += b.vy * b.mass; pz += b.vz * b.mass; }
  const sun = bodies[0];
  sun.vx = -px / SOLAR_MASS; sun.vy = -py / SOLAR_MASS; sun.vz = -pz / SOLAR_MASS;
  return bodies;
}
function advance(bodies: Body[], dt: number): void {
  const n = bodies.length;
  for (let i = 0; i < n; i++) {
    const a = bodies[i];
    for (let j = i + 1; j < n; j++) {
      const b = bodies[j];
      const dx = a.x - b.x, dy = a.y - b.y, dz = a.z - b.z;
      const d2 = dx * dx + dy * dy + dz * dz;
      const mag = dt / (d2 * Math.sqrt(d2));
      const ma = a.mass * mag, mb = b.mass * mag;
      a.vx -= dx * mb; a.vy -= dy * mb; a.vz -= dz * mb;
      b.vx += dx * ma; b.vy += dy * ma; b.vz += dz * ma;
    }
  }
  for (const b of bodies) { b.x += dt * b.vx; b.y += dt * b.vy; b.z += dt * b.vz; }
}
function energy(bodies: Body[]): number {
  let e = 0;
  const n = bodies.length;
  for (let i = 0; i < n; i++) {
    const a = bodies[i];
    e += 0.5 * a.mass * (a.vx * a.vx + a.vy * a.vy + a.vz * a.vz);
    for (let j = i + 1; j < n; j++) {
      const b = bodies[j];
      const dx = a.x - b.x, dy = a.y - b.y, dz = a.z - b.z;
      e -= (a.mass * b.mass) / Math.sqrt(dx * dx + dy * dy + dz * dz);
    }
  }
  return e;
}
runBench("d-nbody", 1, () => {
  const bodies = makeSystem();
  for (let i = 0; i < 5000000; i++) advance(bodies, 0.01);
  return Math.round(1e9 * energy(bodies));
});

const evalA = (i: number, j: number) => 1 / ((((i + j) * (i + j + 1)) >> 1) + i + 1);
function evalATimesU(u: Float64Array, au: Float64Array): void {
  const n = u.length;
  for (let i = 0; i < n; i++) { let s = 0; for (let j = 0; j < n; j++) s += evalA(i, j) * u[j]; au[i] = s; }
}
function evalAtTimesU(u: Float64Array, au: Float64Array): void {
  const n = u.length;
  for (let i = 0; i < n; i++) { let s = 0; for (let j = 0; j < n; j++) s += evalA(j, i) * u[j]; au[i] = s; }
}
function spectralNorm(n: number): number {
  const u = new Float64Array(n).fill(1), v = new Float64Array(n), tmp = new Float64Array(n);
  for (let i = 0; i < 10; i++) {
    evalATimesU(u, tmp); evalAtTimesU(tmp, v);
    evalATimesU(v, tmp); evalAtTimesU(tmp, u);
  }
  let vbv = 0, vv = 0;
  for (let i = 0; i < n; i++) { vbv += u[i] * v[i]; vv += v[i] * v[i]; }
  return Math.sqrt(vbv / vv);
}
runBench("d-spectral-norm", 1, () => Math.round(1e9 * spectralNorm(2000)));

function mandelbrot(size: number): number {
  const bytesPerRow = Math.ceil(size / 8);
  const out = new Uint8Array(size * bytesPerRow);
  const inv = 2 / size;
  for (let y = 0; y < size; y++) {
    const ci = y * inv - 1;
    let bits = 0, pos = y * bytesPerRow;
    for (let x = 0; x < size; x++) {
      const cr = x * inv - 1.5;
      let zr = 0, zi = 0, tr = 0, ti = 0, escaped = false;
      for (let k = 0; k < 50; k++) {
        zi = 2 * zr * zi + ci; zr = tr - ti + cr; tr = zr * zr; ti = zi * zi;
        if (tr + ti > 4) { escaped = true; break; }
      }
      bits = ((bits << 1) & 0xff) | (escaped ? 0 : 1);
      if ((x & 7) === 7) { out[pos++] = bits; bits = 0; }
    }
    const rem = size & 7;
    if (rem !== 0) out[pos] = (bits << (8 - rem)) & 0xff;
  }
  let sum = 0;
  for (let i = 0; i < out.length; i++) sum = (sum * 31 + out[i]) & 0xffffff;
  return sum;
}
runBench("d-mandelbrot", 1, () => mandelbrot(2000));

function fannkuchRedux(n: number): number {
  const perm = new Int32Array(n), perm1 = new Int32Array(n), count = new Int32Array(n);
  let maxFlips = 0, checksum = 0, permCount = 0, r = n;
  for (let i = 0; i < n; i++) perm1[i] = i;
  for (;;) {
    while (r !== 1) { count[r - 1] = r; r--; }
    perm.set(perm1);
    let flips = 0, k = perm[0];
    while (k !== 0) {
      for (let i = 0, j = k; i < j; i++, j--) { const t = perm[i]; perm[i] = perm[j]; perm[j] = t; }
      flips++;
      k = perm[0];
    }
    if (flips > maxFlips) maxFlips = flips;
    checksum += permCount % 2 === 0 ? flips : -flips;
    for (;;) {
      if (r === n) return checksum * 100 + maxFlips;
      const perm0 = perm1[0];
      for (let i = 0; i < r; i++) perm1[i] = perm1[i + 1];
      perm1[r] = perm0;
      count[r]--;
      if (count[r] > 0) break;
      r++;
    }
    permCount++;
  }
}
runBench("d-fannkuch", 1, () => fannkuchRedux(10));

class TreeNode { left: TreeNode | null; right: TreeNode | null; constructor(l: TreeNode | null, r: TreeNode | null) { this.left = l; this.right = r; } }
function bottomUpTree(depth: number): TreeNode {
  return depth === 0 ? new TreeNode(null, null)
                     : new TreeNode(bottomUpTree(depth - 1), bottomUpTree(depth - 1));
}
function itemCheck(tree: TreeNode): number {
  const l = tree.left;
  return l ? 1 + itemCheck(l) + itemCheck(tree.right!) : 1;
}
function binaryTrees(maxDepth: number): number {
  let total = itemCheck(bottomUpTree(maxDepth + 1));
  const longLived = bottomUpTree(maxDepth);
  for (let depth = 4; depth <= maxDepth; depth += 2) {
    const iterations = 1 << (maxDepth - depth + 4);
    let check = 0;
    for (let i = 0; i < iterations; i++) check += itemCheck(bottomUpTree(depth));
    total += check;
  }
  return total + itemCheck(longLived);
}
runBench("d-binary-trees", 1, () => binaryTrees(17));

runBench("d-tak", 1, () => { let s = 0; for (let i = 0; i < 300; i++) s += tak(22, 16, 8); return s; });
