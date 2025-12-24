# Implementation Plan: FFI Foundation

**Branch**: `012-ffi-foundation` | **Date**: 2025-12-24 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/012-ffi-foundation/spec.md`

## Summary

FFI基盤（Foreign Function Interface）を実装し、ホスト環境（JavaScript/wasmtime）との双方向相互運用を確立する。主な機能は：
1. Wasm Import/Export宣言の自動生成
2. Lisp型とWasm値の双方向マーシャリング（fixnum, float, string, boolean, anyref）
3. `ffi:define-foreign-function`マクロによるホスト関数宣言
4. `ffi:call-host`による外部関数呼び出し
5. Lisp関数のWasmエクスポートとホストからのコールバック

線形メモリを使用せず、WasmGC-First原則に準拠。

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) for compiler, WasmGC for output
**Primary Dependencies**: alexandria, babel, trivial-gray-streams, rove (testing)
**Storage**: N/A (compiler generates Wasm binaries)
**Testing**: rove (Common Lisp), wasmtime + Node.js for FFI integration tests
**Target Platform**: WasmGC (wasmtime 27.0+, Chrome 119+, Firefox 120+)
**Project Type**: single (compiler codebase)
**Performance Goals**: FFI call overhead < 100μs, marshalling < 10μs per value
**Constraints**: No linear memory usage (WasmGC-First), sync-only calls
**Scale/Scope**: 5 marshal types, 30+ unit tests

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First | ✅ PASS | 文字列はWasmGC arrayで渡す（FR-006）。線形メモリ不使用を明示（SC-007） |
| II. Lispオブジェクト表現規約 | ✅ PASS | anyrefで未変換オブジェクトをパススルー（FR-008） |
| III. 関数・クロージャ実装戦略 | ✅ PASS | エクスポート関数はクロージャラッパー経由で呼び出し |
| IV. Wasm制御フロー活用 | ✅ PASS | ホスト例外は`try_table`で捕捉しLispコンディションに変換 |
| V. シャローバインディング | N/A | FFIは動的スコープに直接依存しない |
| VI. 段階的動的コンパイル | N/A | FFIはAOTコンパイルで完結 |
| VII. TDD（非交渉） | ✅ REQUIRED | すべてのFFI機能にテスト先行実装が必須 |
| VIII. Nix-Firstワークフロー | ✅ PASS | wasmtime、wasm-toolsはflake.nixで管理済み |
| セキュリティ制約 | ✅ PASS | サンドボックス境界を維持。外部アクセスはimport経由のみ |
| 相互運用要件 | ✅ PARTIAL | Core Wasm importに集中（Component Modelは後続フェーズ） |

**Gate Result**: PASS - 全必須原則に準拠

## Project Structure

### Documentation (this feature)

```text
specs/012-ffi-foundation/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/           # Phase 1 output (Lisp API contracts)
└── tasks.md             # Phase 2 output (/speckit.tasks)
```

### Source Code (repository root)

```text
src/
├── clysm/
│   ├── ffi/                    # NEW: FFI module
│   │   ├── package.lisp        # FFI package definition
│   │   ├── types.lisp          # ForeignFunctionDecl, ExportDecl, MarshalType
│   │   ├── marshalling.lisp    # Type conversion logic
│   │   ├── import-gen.lisp     # Wasm import section generation
│   │   ├── export-gen.lisp     # Wasm export section generation
│   │   └── macros.lisp         # ffi:define-foreign-function, ffi:export-function
│   ├── backend/
│   │   └── sections.lisp       # EXTEND: Import section encoding
│   ├── compiler/
│   │   └── codegen/
│   │       └── wasm-ir.lisp    # EXTEND: Import/Export IR structures
│   └── lib/
│       └── ffi-runtime.lisp    # Runtime support for ffi:call-host

tests/
├── unit/
│   ├── ffi-types-test.lisp     # NEW: FFI type definitions
│   ├── ffi-marshalling-test.lisp # NEW: Marshalling logic
│   └── ffi-codegen-test.lisp   # NEW: Import/Export generation
├── contract/
│   └── ffi-section-test.lisp   # NEW: Wasm binary validation
└── integration/
    ├── ffi-import-test.lisp    # NEW: Host function calls
    ├── ffi-export-test.lisp    # NEW: Lisp function exports
    └── ffi-multi-host-test.lisp # NEW: wasmtime + Node.js tests

# Host test harnesses (for integration testing)
test-harness/
├── wasmtime/
│   └── ffi-host.rs             # Rust host for wasmtime tests
└── node/
    └── ffi-host.js             # JavaScript host for Node.js tests
```

**Structure Decision**: Single project structure (compiler codebase). FFI機能は新規`src/clysm/ffi/`モジュールとして追加。既存の`backend/sections.lisp`と`compiler/codegen/wasm-ir.lisp`を拡張してImport/Exportセクションをサポート。

## Complexity Tracking

> No Constitution violations requiring justification.

| Aspect | Decision | Rationale |
|--------|----------|-----------|
| Module location | `src/clysm/ffi/` | 独立したモジュールで依存関係を明確化 |
| Host test harnesses | `test-harness/` | 統合テストにwasmtimeとNode.jsの両方が必要 |
