# func-section.lisp リファクタリング計画

**作成日**: 2026-01-02
**親ドキュメント**: implementation-plan.md (Phase 13D-R4)
**ステータス**: 計画中
**対象ファイル**: `src/clysm/compiler/codegen/func-section.lisp`

## 概要

本ドキュメントは、`func-section.lisp`のリファクタリング計画を詳細に定義する。
implementation-plan.mdのPhase 13D-R4「func-section.lisp整理」のサブプランとして、
具体的な問題点、改善方針、実装タスクを記述する。

---

## 1. 現状分析

### 1.1 規模メトリクス

| メトリクス | 現在値 | 目標値 | 達成率 |
|-----------|--------|--------|--------|
| 総行数 | 16,483行 | 8,000行以下 | 49% |
| compile-* 関数数 | 318個 | 150個以下 | 47% |
| case/cond/typecase使用数 | 332箇所 | - | - |
| Wasm命令パターン | 1,003箇所 | - | - |
| append呼び出し | 517箇所 | 0 | 0% |

### 1.2 巨大関数リスト（100行以上）

| 順位 | 関数名 | 行数 | カテゴリ | 対応方針 |
|------|--------|------|----------|----------|
| 1 | `compile-equalp` | 374行 | 等価性 | 共通化+ランタイム |
| 2 | `compile-primitive-call` | 363行 | ディスパッチ | テーブル駆動化 |
| 3 | `compile-equal` | 274行 | 等価性 | 共通化+ランタイム |
| 4 | `compile-parse-integer` | 242行 | 数値 | ランタイム移行 |
| 5 | `compile-write-to-string` | 214行 | I/O | ランタイム移行 |
| 6 | `compile-string-char` | 199行 | 文字列 | ランタイム移行 |
| 7 | `compile-rationalize` | 192行 | 数値 | ランタイム移行 |
| 8 | `compile-subseq` | 189行 | シーケンス | ランタイム移行 |
| 9 | `compile-eql` | 178行 | 等価性 | 共通化 |
| 10 | `compile-call` | 172行 | コア | 分割 |
| 11 | `compile-signum` | 152行 | 数値 | ランタイム移行 |
| 12 | `compile-adjust-array` | 143行 | 配列 | ランタイム移行 |
| 13 | `compile-string-trim` | 135行 | 文字列 | ランタイム移行 |
| 14 | `compile-string-capitalize` | 126行 | 文字列 | ランタイム移行 |
| 15 | `compile-string-compare-ci` | 125行 | 文字列 | ランタイム移行 |
| 16 | `compile-nstring-capitalize` | 125行 | 文字列 | ランタイム移行 |
| 17 | `compile-catch-simple` | 119行 | 制御 | 保持（コア） |
| 18 | `compile-phase` | 116行 | 数値 | ランタイム移行 |
| 19 | `compile-handler-case` | 113行 | 制御 | 保持（コア） |
| 20 | `compile-default-values-preamble` | 113行 | コア | 保持 |

---

## 2. 問題点詳細

### 2.1 非効率なリスト構築パターン（517箇所）

**現在のパターン**:
```lisp
;; 毎回O(n)のコピーが発生
(setf result (append result (list (list :local.get local-x))))
(setf result (append result (list (list :local.set local-y))))
(setf result (append result '(:ref.is_null)))
```

**問題点**:
- `append`は毎回リスト全体をコピー（O(n)）
- n回のappendで全体がO(n²)に
- GC負荷増大

**改善案**:
```lisp
;; push + nreverseパターン（全体O(n)）
(let ((result '()))
  (push (list :local.get local-x) result)
  (push (list :local.set local-y) result)
  (push :ref.is_null result)
  ...
  (nreverse result))
```

### 2.2 cXXr関数の重複（12個の個別定義）

**現在の実装**:
```lisp
(defun compile-caar (args env)
  "Compile (caar x) = (car (car x))"
  (compile-cxr-chain '(:car :car) args env))

(defun compile-cadr (args env)
  "Compile (cadr x) = (car (cdr x))"
  (compile-cxr-chain '(:cdr :car) args env))

;; ... 同様のパターンが12個続く
```

**問題点**:
- 同一パターンの関数が12個
- エラーパターンP626は`compile-cxr-chain`が未定義であることを示唆
- 実際には定義されているが、適切にエクスポートされていない可能性

**改善案**:
```lisp
;; マクロで一括生成
(defmacro define-cxr-compiler (name ops)
  `(defun ,(intern (format nil "COMPILE-~A" name)) (args env)
     ,(format nil "Compile (~A x)" (string-downcase name))
     (compile-cxr-chain ',ops args env)))

(define-cxr-compiler caar (:car :car))
(define-cxr-compiler cadr (:cdr :car))
;; ...
```

### 2.3 巨大dispatch table（compile-primitive-call）

**現在の実装**（363行）:
```lisp
(defun compile-primitive-call (op args env)
  (let ((env (env-with-non-tail env))
        (op-name (when (symbolp op) (symbol-name op))))
    (cond
      ((string= op-name "%SETF-AREF") (compile-setf-aref args env))
      ;; ... 20+のstring=チェック
      (t
       (case op
         (+  (compile-arithmetic-op :i32.add args env 0))
         (-  (if (= 1 (length args)) ...))
         (*  (compile-arithmetic-op :i32.mul args env 1))
         ;; ... 200+のcase分岐
         )))))
```

**問題点**:
- 200+の分岐を持つ巨大case文
- 新しいプリミティブ追加時にこの関数を編集必要
- コンパイル時間への影響

**改善案**:
```lisp
;; テーブル駆動ディスパッチ
(defparameter *primitive-compilers* (make-hash-table :test 'eq))
(defparameter *primitive-compilers-by-name* (make-hash-table :test 'equal))

(defun register-primitive-compiler (op compiler &key (by-name nil))
  "Register a compiler function for a primitive operation."
  (if by-name
      (setf (gethash (string op) *primitive-compilers-by-name*) compiler)
      (setf (gethash op *primitive-compilers*) compiler)))

(defun compile-primitive-call (op args env)
  (let ((env (env-with-non-tail env))
        (op-name (when (symbolp op) (symbol-name op))))
    ;; 1. by-name lookup（%SETF-* 等）
    (let ((by-name-compiler (gethash op-name *primitive-compilers-by-name*)))
      (when by-name-compiler
        (return-from compile-primitive-call
          (funcall by-name-compiler args env))))
    ;; 2. symbol lookup
    (let ((compiler (gethash op *primitive-compilers*)))
      (if compiler
          (funcall compiler args env)
          (error "Unknown primitive: ~A" op)))))

;; 登録は別ファイルまたはinitで
(register-primitive-compiler '+
  (lambda (args env) (compile-arithmetic-op :i32.add args env 0)))
```

### 2.4 等価性関数の重複（compile-eq, compile-eql, compile-equal, compile-equalp）

**問題点**:
- 4つの関数で合計800行以上
- 型チェックロジックが重複
- 新しい型追加時に4箇所を修正必要

**改善案**:
```lisp
;; 共通の型チェック基盤
(defun compile-type-dispatch (value-local cases env)
  "Generate type dispatch code for a value.
   CASES: ((type-predicate . code-generator) ...)"
  ...)

;; eqはシンプルに保持（ref.eq）
;; eql, equal, equalpは段階的に拡張
(defun compile-eql (args env)
  (compile-equality-predicate args env :level :eql))

(defun compile-equal (args env)
  (compile-equality-predicate args env :level :equal))

(defun compile-equalp (args env)
  (compile-equality-predicate args env :level :equalp))
```

### 2.5 責務の混在

**現在の状態**:
func-section.lispが以下を全て担当:

| カテゴリ | 例 | あるべき場所 |
|----------|-----|-------------|
| Wasmプリミティブ | cons, car, cdr, + | func-section.lisp（保持） |
| 型判定 | consp, integerp | func-section.lisp（保持） |
| 制御構造 | tagbody, catch | func-section.lisp（保持） |
| 標準ライブラリ | assoc, member | lib/list-runtime.lisp |
| I/O関数 | format, princ | lib/io-runtime.lisp |
| 数値関数 | parse-integer, rationalize | lib/numeric-runtime.lisp |
| 文字列関数 | string-upcase, string-trim | lib/string-runtime.lisp |
| シーケンス関数 | subseq, remove | lib/sequence-runtime.lisp |

---

## 3. リファクタリング計画

### Phase R4-1: コード生成効率化（推定削減: 500行）

**タスク**:
1. [ ] `append`パターンを`push + nreverse`に置換（517箇所）
2. [ ] ヘルパーマクロ`with-instruction-collector`導入
3. [ ] テスト: 既存テスト全パス確認

**実装例**:
```lisp
(defmacro with-instruction-collector ((var) &body body)
  "Collect instructions efficiently using push + nreverse."
  `(let ((,var '()))
     (macrolet ((emit (instr)
                  `(push ,instr ,',var))
                (emit* (&rest instrs)
                  `(progn ,@(mapcar (lambda (i) `(push ,i ,',var)) instrs))))
       ,@body
       (nreverse ,var))))
```

### Phase R4-2: ディスパッチテーブル化（推定削減: 300行）

**タスク**:
1. [ ] `*primitive-compilers*`ハッシュテーブル導入
2. [ ] `register-primitive-compiler`関数実装
3. [ ] `compile-primitive-call`をテーブル駆動に変更
4. [ ] 既存case分岐をregister呼び出しに変換
5. [ ] テスト: プリミティブ呼び出し全パス確認

### Phase R4-3: cXXr関数統合（推定削減: 100行）

**タスク**:
1. [ ] `compile-cxr-chain`のエクスポート確認/修正
2. [ ] `define-cxr-compiler`マクロ導入
3. [ ] 12個の個別関数をマクロ生成に置換
4. [ ] エラーパターンP626解消確認

### Phase R4-4: 等価性関数リファクタリング（推定削減: 400行）

**タスク**:
1. [ ] 共通型チェック基盤`compile-type-dispatch`実装
2. [ ] `compile-equality-predicate`統合関数実装
3. [ ] compile-eq保持（最小実装）
4. [ ] compile-eql, compile-equal, compile-equalpを統合関数使用に変更
5. [ ] テスト: 024-equality-predicates全パス確認

### Phase R4-5: 文字列関数ランタイム移行（推定削減: 1,500行）

**対象関数**:
- compile-string-char (199行)
- compile-string-trim (135行)
- compile-string-capitalize (126行)
- compile-string-compare-ci (125行)
- compile-nstring-capitalize (125行)
- compile-string-upcase, compile-string-downcase
- その他文字列関連

**タスク**:
1. [ ] `lib/string-runtime.lisp`にLisp実装追加
2. [ ] `*runtime-function-table*`に登録
3. [ ] func-section.lispから対応コード削除
4. [ ] テスト: 文字列テスト全パス確認

### Phase R4-6: 数値関数ランタイム移行（推定削減: 800行）

**対象関数**:
- compile-parse-integer (242行)
- compile-write-to-string (214行)
- compile-rationalize (192行)
- compile-signum (152行)
- compile-phase (116行)

**タスク**:
1. [ ] `lib/numeric-runtime.lisp`にLisp実装追加
2. [ ] `*runtime-function-table*`に登録
3. [ ] func-section.lispから対応コード削除
4. [ ] テスト: 数値テスト全パス確認

### Phase R4-7: シーケンス/配列関数ランタイム移行（推定削減: 500行）

**対象関数**:
- compile-subseq (189行)
- compile-adjust-array (143行)
- その他シーケンス関連

**タスク**:
1. [ ] 既存`lib/sequence-runtime.lisp`拡張
2. [ ] `*runtime-function-table*`に登録
3. [ ] func-section.lispから対応コード削除
4. [ ] テスト: シーケンステスト全パス確認

---

## 4. 依存関係グラフ

```
Phase R4-1 (効率化)
    │
    ▼
Phase R4-2 (ディスパッチ)
    │
    ├──→ Phase R4-3 (cXXr統合)
    │
    ▼
Phase R4-4 (等価性) ←── 独立して実行可能
    │
    ▼
Phase R4-5 (文字列移行)
    │
    ▼
Phase R4-6 (数値移行)
    │
    ▼
Phase R4-7 (シーケンス移行)
```

---

## 5. 検証基準

### 5.1 行数目標

| Phase | 開始時 | 終了時 | 削減行数 |
|-------|--------|--------|----------|
| R4-1 | 16,483 | 15,983 | -500 |
| R4-2 | 15,983 | 15,683 | -300 |
| R4-3 | 15,683 | 15,583 | -100 |
| R4-4 | 15,583 | 15,183 | -400 |
| R4-5 | 15,183 | 13,683 | -1,500 |
| R4-6 | 13,683 | 12,883 | -800 |
| R4-7 | 12,883 | 12,383 | -500 |
| **最終** | **16,483** | **7,883** | **-8,600** |

### 5.2 品質基準

- [ ] 既存テスト全パス（`sbcl --eval "(asdf:test-system :clysm)"`）
- [ ] Stage 1生成成功（`sbcl --load build/stage1-complete.lisp`）
- [ ] Wasm検証パス（`wasm-tools validate dist/clysm-stage1.wasm`）
- [ ] コンパイル率維持または向上（現在19.20%）

### 5.3 エラーパターン解消

| パターンID | 説明 | 対応Phase |
|------------|------|-----------|
| P457 | Undefined function: COMPILE-UNARY-MATH-FFI | R4-2 |
| P626 | Undefined function: COMPILE-CXR-CHAIN | R4-3 |
| P334 | Undefined function: REGISTER-RUNTIME-FUNCTION | R4-5〜R4-7 |

---

## 6. リスク分析

| リスク | 影響度 | 対策 |
|--------|--------|------|
| ランタイム関数未実装 | 高 | 各Phase前にlib/*.lispの実装確認 |
| テスト不足 | 中 | 各関数にunit test追加 |
| パフォーマンス低下 | 中 | ベンチマーク比較（コンパイル時間） |
| 依存関係の見落とし | 中 | grep/静的解析で使用箇所確認 |

---

## 7. 関連ドキュメント

- `implementation-plan.md` - 親ドキュメント（Phase 13D-R4）
- `lib/io-runtime.lisp` - I/Oランタイム実装
- `lib/list-runtime.lisp` - リストランタイム実装
- `lib/sequence-runtime.lisp` - シーケンスランタイム実装

---

## 変更履歴

| バージョン | 日付 | 変更内容 |
|------------|------|----------|
| 1.0.0 | 2026-01-02 | 初版作成 |
