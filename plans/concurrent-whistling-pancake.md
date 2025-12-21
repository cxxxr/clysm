# マイルストーン1: カーネル・ランタイム構築 - 詳細実装プラン

## 概要

**目標**: Wasm GCを利用したLispデータ構造と基本操作の実装
**成果物**: `kernel.wasm` + JSブリッジ + テストスイート
**方針**: TDD（テスト駆動開発）に基づき、テストファーストで進める

### 技術選択
- **テストフレームワーク**: Node.js 組み込み test runner（依存なし）
- **Character表現**: タグビット方式（i31ref上位1ビットで区別）

---

## Phase 1: 開発環境のセットアップ

### 1.1 Nix Flake の作成

**ファイル**: `flake.nix`

必要なツール:
- `wabt` (wat2wasm, wasm2wat)
- `binaryen` (wasm-opt)
- `nodejs_22` (Wasm GCサポート)
- `deno` (代替テスト環境)

```nix
{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs = { self, nixpkgs }: {
    devShells.x86_64-linux.default = nixpkgs.legacyPackages.x86_64-linux.mkShell {
      packages = with nixpkgs.legacyPackages.x86_64-linux; [
        wabt
        binaryen
        nodejs_22
        deno
      ];
    };
  };
}
```

### 1.2 プロジェクト構造

```
clysm_workbench1/
├── flake.nix
├── flake.lock
├── .envrc                 # use flake
├── .gitignore
├── IMPLEMENTATION_PLAN.md
├── src/
│   └── kernel/
│       └── kernel.wat     # カーネル本体（型定義+プリミティブ）
├── js/
│   ├── bridge.js          # JSブリッジ
│   ├── reader.js          # S式パーサー
│   └── printer.js         # オブジェクト表示
├── test/
│   └── kernel.test.js     # カーネルテスト
├── build/
│   └── kernel.wasm        # ビルド成果物
├── package.json
└── Makefile               # ビルドスクリプト
```

### 1.3 package.json

```json
{
  "name": "clysm",
  "version": "0.1.0",
  "type": "module",
  "scripts": {
    "build": "make build",
    "test": "node --experimental-wasm-gc --test test/*.test.js",
    "clean": "make clean"
  }
}
```

### 1.4 Makefile

```makefile
.PHONY: build test clean

WAT_SRC := src/kernel/kernel.wat
WASM_OUT := build/kernel.wasm

build: $(WASM_OUT)

$(WASM_OUT): $(WAT_SRC)
	@mkdir -p build
	wat2wasm --enable-gc --enable-reference-types --enable-bulk-memory \
		$(WAT_SRC) -o $(WASM_OUT)

test: build
	node --experimental-wasm-gc --test test/*.test.js

clean:
	rm -rf build/
```

---

## Phase 2: 型定義 (WAT)

### 2.1 タグビット方式（Fixnum/Character）

i31refは31ビット符号付き整数。上位1ビットをタグとして使用:

```
i31ref (31 bits):
┌─────────────────────────────────────┐
│ T │        VALUE (30 bits)          │
└─────────────────────────────────────┘

T=0: Fixnum  → 値範囲: -2^29 〜 2^29-1 (-536,870,912 〜 536,870,911)
T=1: Character → Unicode コードポイント (0 〜 0x10FFFF)
```

**実装**:
```wat
;; Fixnum作成: 値を1ビット左シフト（タグ=0）
(func $make_fixnum (param $val i32) (result (ref i31))
  (ref.i31 (i32.shl (local.get $val) (i32.const 1))))

;; Character作成: 値を1ビット左シフト + タグ=1
(func $make_char (param $code i32) (result (ref i31))
  (ref.i31 (i32.or
    (i32.shl (local.get $code) (i32.const 1))
    (i32.const 1))))

;; Fixnumか判定: 最下位ビット=0
(func $fixnump (param $x anyref) (result i32)
  (if (result i32) (ref.test (ref i31) (local.get $x))
    (then (i32.eqz (i32.and
      (i31.get_s (ref.cast (ref i31) (local.get $x)))
      (i32.const 1))))
    (else (i32.const 0))))

;; Characterか判定: 最下位ビット=1
(func $characterp (param $x anyref) (result i32)
  (if (result i32) (ref.test (ref i31) (local.get $x))
    (then (i32.and
      (i31.get_s (ref.cast (ref i31) (local.get $x)))
      (i32.const 1)))
    (else (i32.const 0))))
```

### 2.2 基本型定義

**ファイル**: `src/kernel/types.wat`

```wat
(module
  ;; === 文字列型 ===
  (type $string (array (mut i8)))

  ;; === Cons Cell ===
  (type $cons (struct
    (field $car (mut anyref))
    (field $cdr (mut anyref))
  ))

  ;; === Symbol ===
  (type $symbol (struct
    (field $name (ref $string))       ;; 印字名（不変）
    (field $value (mut anyref))       ;; 値セル
    (field $function (mut anyref))    ;; 関数セル
    (field $plist (mut anyref))       ;; プロパティリスト
    (field $package (mut anyref))     ;; パッケージ
  ))

  ;; === Simple Vector ===
  (type $vector (array (mut anyref)))

  ;; === Boxed Float ===
  (type $float (struct
    (field $value f64)
  ))

  ;; === Bignum ===
  (type $bignum (struct
    (field $sign i32)                 ;; 0: 正, 1: 負
    (field $digits (ref (array i64))) ;; リトルエンディアン
  ))

  ;; === Unbound マーカー ===
  (type $unbound (struct))
)
```

### 2.3 特殊オブジェクト

```wat
  ;; === グローバル特殊オブジェクト ===
  (global $NIL (ref $symbol) ...)      ;; NILシンボル
  (global $T (ref $symbol) ...)        ;; Tシンボル
  (global $UNBOUND (ref $unbound) ...) ;; 未束縛マーカー
```

---

## Phase 3: プリミティブ関数

### 3.1 コンストラクタ

| 関数 | シグネチャ | 説明 |
|---|---|---|
| `cons` | `(anyref, anyref) -> ref $cons` | Consセル生成 |
| `make_symbol` | `(ref $string) -> ref $symbol` | シンボル生成 |
| `make_vector` | `(i32, anyref) -> ref $vector` | ベクタ生成 |
| `make_string` | `(i32) -> ref $string` | 文字列生成 |
| `box_float` | `(f64) -> ref $float` | Float ボックス化 |

### 3.2 アクセサ

| 関数 | シグネチャ | 説明 |
|---|---|---|
| `car` | `(anyref) -> anyref` | Cons car取得 |
| `cdr` | `(anyref) -> anyref` | Cons cdr取得 |
| `rplaca` | `(ref $cons, anyref) -> ref $cons` | car破壊的更新 |
| `rplacd` | `(ref $cons, anyref) -> ref $cons` | cdr破壊的更新 |
| `symbol_name` | `(ref $symbol) -> ref $string` | シンボル名取得 |
| `symbol_value` | `(ref $symbol) -> anyref` | シンボル値取得 |
| `symbol_function` | `(ref $symbol) -> anyref` | シンボル関数取得 |
| `set_symbol_value` | `(ref $symbol, anyref) -> anyref` | 値設定 |
| `set_symbol_function` | `(ref $symbol, anyref) -> anyref` | 関数設定 |
| `svref` | `(ref $vector, i32) -> anyref` | ベクタ要素取得 |
| `svset` | `(ref $vector, i32, anyref) -> anyref` | ベクタ要素設定 |

### 3.3 型述語

| 関数 | シグネチャ | 実装 |
|---|---|---|
| `consp` | `(anyref) -> i32` | `ref.test (ref $cons)` |
| `symbolp` | `(anyref) -> i32` | `ref.test (ref $symbol)` |
| `fixnump` | `(anyref) -> i32` | `ref.test (ref i31)` |
| `stringp` | `(anyref) -> i32` | `ref.test (ref $string)` |
| `vectorp` | `(anyref) -> i32` | `ref.test (ref $vector)` |
| `floatp` | `(anyref) -> i32` | `ref.test (ref $float)` |
| `null` | `(anyref) -> i32` | `ref.eq $NIL` |

### 3.4 比較

| 関数 | シグネチャ | 実装 |
|---|---|---|
| `eq` | `(anyref, anyref) -> i32` | `ref.eq` |
| `eql_fixnum` | `(i31ref, i31ref) -> i32` | 値比較 |

### 3.5 Fixnum算術

| 関数 | シグネチャ | 備考 |
|---|---|---|
| `fx_add` | `(i31ref, i31ref) -> anyref` | オーバーフロー時はBignum |
| `fx_sub` | `(i31ref, i31ref) -> anyref` | 同上 |
| `fx_mul` | `(i31ref, i31ref) -> anyref` | 同上 |
| `fx_div` | `(i31ref, i31ref) -> anyref` | ゼロ除算チェック |
| `fx_lt` | `(i31ref, i31ref) -> i32` | 比較 |
| `fx_le` | `(i31ref, i31ref) -> i32` | 比較 |
| `fx_gt` | `(i31ref, i31ref) -> i32` | 比較 |
| `fx_ge` | `(i31ref, i31ref) -> i32` | 比較 |
| `fx_eq` | `(i31ref, i31ref) -> i32` | 数値等価 |

### 3.6 変換

| 関数 | シグネチャ | 説明 |
|---|---|---|
| `i32_to_fixnum` | `(i32) -> i31ref` | i32→Fixnum |
| `fixnum_to_i32` | `(i31ref) -> i32` | Fixnum→i32 |
| `char_to_code` | `(i31ref) -> i32` | Character→コードポイント |
| `code_to_char` | `(i32) -> i31ref` | コードポイント→Character |

---

## Phase 4: JSブリッジ

### 4.1 ローダー

**ファイル**: `js/bridge.js`

```javascript
export async function loadKernel() {
  const wasmBytes = await fetch('./build/kernel.wasm')
    .then(r => r.arrayBuffer());

  const imports = {
    env: {
      // JS側からのコールバック（将来用）
    }
  };

  const { instance } = await WebAssembly.instantiate(wasmBytes, imports);

  return new LispKernel(instance.exports);
}

class LispKernel {
  constructor(exports) {
    this.exports = exports;
    this.NIL = exports.get_nil();
    this.T = exports.get_t();
  }

  // ヘルパーメソッド
  cons(car, cdr) { return this.exports.cons(car, cdr); }
  car(cell) { return this.exports.car(cell); }
  cdr(cell) { return this.exports.cdr(cell); }
  // ...
}
```

### 4.2 Reader（S式パーサー）

**ファイル**: `js/reader.js`

Phase 1では最小限の実装:
- 整数リテラル
- シンボル
- リスト `(a b c)`
- ドットペア `(a . b)`
- 文字列 `"hello"`
- クォート `'x` → `(quote x)`

### 4.3 Printer

**ファイル**: `js/printer.js`

```javascript
export function printLispObject(kernel, obj) {
  if (kernel.exports.fixnump(obj)) {
    return String(kernel.exports.fixnum_to_i32(obj));
  }
  if (kernel.exports.consp(obj)) {
    return printList(kernel, obj);
  }
  if (kernel.exports.symbolp(obj)) {
    return getSymbolName(kernel, obj);
  }
  // ...
}
```

---

## Phase 5: テスト

### 5.1 テスト戦略（TDD）

各機能の実装順序:
1. テストを書く（Red）
2. 最小限の実装（Green）
3. リファクタリング（Refactor）

### 5.2 テストケース

**ファイル**: `test/kernel.test.js`

```javascript
import { describe, it, before } from 'node:test';
import assert from 'node:assert/strict';
import { loadKernel } from '../js/bridge.js';

describe('Kernel Types', () => {
  let kernel;

  before(async () => {
    kernel = await loadKernel();
  });

  describe('Fixnum', () => {
    it('creates fixnum from i32', () => {
      const n = kernel.exports.make_fixnum(42);
      assert.strictEqual(kernel.exports.fixnump(n), 1);
    });

    it('converts back to i32', () => {
      const n = kernel.exports.make_fixnum(42);
      assert.strictEqual(kernel.exports.fixnum_value(n), 42);
    });

    it('handles negative numbers', () => {
      const n = kernel.exports.make_fixnum(-100);
      assert.strictEqual(kernel.exports.fixnum_value(n), -100);
    });
  });

  describe('Character', () => {
    it('creates character from code point', () => {
      const c = kernel.exports.make_char(65); // 'A'
      assert.strictEqual(kernel.exports.characterp(c), 1);
    });

    it('is not a fixnum', () => {
      const c = kernel.exports.make_char(65);
      assert.strictEqual(kernel.exports.fixnump(c), 0);
    });

    it('converts back to code point', () => {
      const c = kernel.exports.make_char(0x3042); // 'あ'
      assert.strictEqual(kernel.exports.char_code(c), 0x3042);
    });
  });

  describe('Cons', () => {
    it('creates cons cell', () => {
      const a = kernel.exports.make_fixnum(1);
      const b = kernel.exports.make_fixnum(2);
      const cell = kernel.exports.cons(a, b);
      assert.strictEqual(kernel.exports.consp(cell), 1);
    });

    it('car returns first element', () => {
      const a = kernel.exports.make_fixnum(1);
      const b = kernel.exports.make_fixnum(2);
      const cell = kernel.exports.cons(a, b);
      const car = kernel.exports.car(cell);
      assert.strictEqual(kernel.exports.fixnum_value(car), 1);
    });

    it('cdr returns second element', () => {
      const a = kernel.exports.make_fixnum(1);
      const b = kernel.exports.make_fixnum(2);
      const cell = kernel.exports.cons(a, b);
      const cdr = kernel.exports.cdr(cell);
      assert.strictEqual(kernel.exports.fixnum_value(cdr), 2);
    });
  });

  describe('Symbol', () => {
    it('NIL is a symbol', () => {
      assert.strictEqual(kernel.exports.symbolp(kernel.NIL), 1);
    });

    it('null returns true for NIL', () => {
      assert.strictEqual(kernel.exports.null_(kernel.NIL), 1);
    });

    it('T is a symbol', () => {
      assert.strictEqual(kernel.exports.symbolp(kernel.T), 1);
    });
  });

  describe('Arithmetic', () => {
    it('adds two fixnums', () => {
      const a = kernel.exports.make_fixnum(10);
      const b = kernel.exports.make_fixnum(20);
      const result = kernel.exports.fx_add(a, b);
      assert.strictEqual(kernel.exports.fixnum_value(result), 30);
    });

    it('subtracts two fixnums', () => {
      const a = kernel.exports.make_fixnum(30);
      const b = kernel.exports.make_fixnum(10);
      const result = kernel.exports.fx_sub(a, b);
      assert.strictEqual(kernel.exports.fixnum_value(result), 20);
    });
  });

  describe('eq', () => {
    it('same fixnum is eq', () => {
      const a = kernel.exports.make_fixnum(42);
      const b = kernel.exports.make_fixnum(42);
      assert.strictEqual(kernel.exports.eq(a, b), 1);
    });

    it('different fixnums are not eq', () => {
      const a = kernel.exports.make_fixnum(1);
      const b = kernel.exports.make_fixnum(2);
      assert.strictEqual(kernel.exports.eq(a, b), 0);
    });

    it('NIL is eq to NIL', () => {
      assert.strictEqual(kernel.exports.eq(kernel.NIL, kernel.NIL), 1);
    });
  });
});
```

**実行コマンド**: `node --experimental-wasm-gc --test test/kernel.test.js`
```

---

## 実装順序（TDDサイクル）

### Step 1: プロジェクト基盤 (Day 1)
```
1. flake.nix 作成
2. direnv allow
3. package.json 作成
4. Makefile 作成
5. ディレクトリ構造作成
6. 空の kernel.wat 作成 → ビルド確認
```

### Step 2: Fixnum/Character (Day 1-2)
```
RED:   test: make_fixnum, fixnum_value, fixnump
GREEN: impl: i31ref タグビット操作
RED:   test: make_char, char_code, characterp
GREEN: impl: タグビット=1 のケース
RED:   test: fx_add, fx_sub, fx_mul
GREEN: impl: 算術演算（タグ除去→計算→タグ付加）
```

### Step 3: Cons (Day 2)
```
RED:   test: cons, car, cdr, consp
GREEN: impl: $cons struct定義、アクセサ
RED:   test: rplaca, rplacd
GREEN: impl: struct.set
RED:   test: car/cdr on NIL → NIL (特殊ケース)
GREEN: impl: NILチェック追加
```

### Step 4: Symbol (Day 2-3)
```
RED:   test: NIL, T の存在・型確認
GREEN: impl: $symbol struct、グローバル初期化
RED:   test: symbolp, null_
GREEN: impl: ref.test, ref.eq
RED:   test: symbol_name, symbol_value, symbol_function
GREEN: impl: アクセサ
RED:   test: set_symbol_value
GREEN: impl: struct.set
```

### Step 5: String/Vector (Day 3)
```
RED:   test: make_string, string_length, stringp
GREEN: impl: $string array定義
RED:   test: make_vector, vector_length, vectorp
GREEN: impl: $vector array定義
RED:   test: svref, svset
GREEN: impl: array.get/set
```

### Step 6: eq/比較 (Day 3)
```
RED:   test: eq 各種ケース（同一Fixnum、異なるFixnum、NIL、Cons）
GREEN: impl: ref.eq
RED:   test: fx_eq, fx_lt, fx_le, fx_gt, fx_ge
GREEN: impl: 数値比較
```

### Step 7: JSブリッジ (Day 4)
```
1. js/bridge.js: loadKernel(), LispKernel クラス
2. js/printer.js: printLispObject()
3. js/reader.js: 最小限パーサー（整数、シンボル、リスト）
```

### Step 8: 統合テスト (Day 4)
```
RED:   test: リスト構築 (list 1 2 3)
GREEN: impl: 必要なヘルパー
RED:   test: 入れ子リスト
GREEN: impl: 再帰処理
```

---

## 検証基準（Definition of Done）

マイルストーン1は以下が達成されたとき完了:

- [ ] `wat2wasm` で `kernel.wasm` がビルドできる
- [ ] Node.js から `kernel.wasm` をロードできる
- [ ] Fixnum の作成・演算・比較が動作する
- [ ] Cons セルの作成・アクセス・更新が動作する
- [ ] NIL と T が正しく初期化される
- [ ] Symbol の作成・アクセスが動作する
- [ ] 全テストが通過する（カバレッジ90%以上）
- [ ] JSからリストを構築し、表示できる

---

## 作成するファイル一覧

| ファイル | 説明 | Step |
|---|---|---|
| `flake.nix` | 開発環境定義（wabt, nodejs等） | 1 |
| `package.json` | npm設定（type: module） | 1 |
| `Makefile` | ビルドスクリプト | 1 |
| `.gitignore` | build/, node_modules/ | 1 |
| `src/kernel/kernel.wat` | カーネル本体（型+プリミティブ） | 2-6 |
| `test/kernel.test.js` | テストスイート | 2-6 |
| `js/bridge.js` | JSブリッジ・ローダー | 7 |
| `js/printer.js` | オブジェクト表示 | 7 |
| `js/reader.js` | S式パーサー | 7 |

**ファイル総数**: 9ファイル

---

## 次のアクション

プラン承認後、以下の順序で実装開始:

1. **環境構築** (Step 1)
   - `flake.nix` 作成
   - `direnv allow` 実行
   - `package.json`, `Makefile` 作成
   - ディレクトリ構造作成

2. **最初のテスト** (Step 2開始)
   - `test/kernel.test.js` に Fixnum テストを記述
   - `npm test` → 失敗確認（Red）

3. **最小実装** (Step 2継続)
   - `src/kernel/kernel.wat` に型定義追加
   - `npm test` → 成功確認（Green）

4. **以降はTDDサイクルで継続**
