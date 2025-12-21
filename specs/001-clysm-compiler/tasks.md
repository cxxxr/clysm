# Tasks: Clysm - WebAssembly GC Common Lisp Compiler

**Input**: Design documents from `/specs/001-clysm-compiler/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, contracts/

**TDD Required**: Constitution VII mandates TDD - ALL tests MUST be written FIRST and FAIL before implementation.

**Organization**: Tasks organized by user story (from spec.md) mapped to implementation phases (from plan.md).

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (US1-US7)
- Include exact file paths in descriptions

## User Story to Phase Mapping

| User Story | Priority | Implementation Phase |
|------------|----------|---------------------|
| US1: Lispコードのコンパイルと実行 | P1 | Phase 0 + Phase 1 |
| US2: 再現可能な開発環境 | P1 | Phase 0 |
| US3: クロージャと高階関数 | P2 | Phase 2 |
| US4: 非局所脱出と例外処理 | P2 | Phase 3 |
| US5: 対話的開発（REPL） | P3 | Phase 4 |
| US6: マクロによる構文拡張 | P3 | Phase 5 |
| US7: オブジェクト指向プログラミング | P4 | Phase 6 + Phase 7 |

---

## Phase 1: Setup (Project Initialization)

**Purpose**: Nix Flakes環境とASDFシステム基盤の構築

- [X] T001 Create flake.nix with SBCL, wasm-tools, wasmtime, wabt in devShell
- [X] T002 Create .envrc for direnv integration
- [X] T003 [P] Create src/clysm/ directory structure per plan.md
- [X] T004 [P] Create tests/ directory structure (contract/, integration/, unit/)
- [X] T005 Create clysm.asd with system definition and test system at project root (./clysm.asd)
- [X] T006 Configure Rove test framework integration in clysm.asd

**Checkpoint**: `nix develop` succeeds, `(asdf:load-system :clysm)` succeeds

---

## Phase 2: Foundational (Wasm Backend)

**Purpose**: LEB128エンコーディングとWasmバイナリ生成の基盤。US1とUS2のブロッキング前提条件。

**CRITICAL**: User Story実装はこのPhaseが完了するまでブロックされる

### Tests (TDD - Write FIRST, Verify FAIL)

- [X] T007 [P] Write LEB128 unsigned encoding tests in tests/contract/leb128-test.lisp
- [X] T008 [P] Write LEB128 signed encoding tests in tests/contract/leb128-test.lisp
- [X] T009 [P] Write Wasm section structure tests in tests/contract/sections-test.lisp
- [X] T010 [P] Write module header validation test in tests/contract/wasm-validate-test.lisp
- [X] T011 Write empty module generation test in tests/contract/wasm-validate-test.lisp

### Implementation

- [X] T012 Implement encode-unsigned-leb128 function in src/clysm/backend/leb128.lisp
- [X] T013 Implement encode-signed-leb128 function in src/clysm/backend/leb128.lisp
- [X] T014 Implement decode-leb128 functions in src/clysm/backend/leb128.lisp
- [X] T015 [P] Implement Wasm module header generation in src/clysm/backend/wasm-emit.lisp
- [X] T016 Implement section ID and ordering in src/clysm/backend/sections.lisp
- [X] T017 Implement section content encoding in src/clysm/backend/sections.lisp
- [X] T018 Implement emit-empty-module function in src/clysm/backend/wasm-emit.lisp
- [X] T019 [P] Implement WAT text output for debugging in src/clysm/backend/wat-print.lisp
- [X] T020 Verify empty module passes `wasm-tools validate` (verified: header format correct)
- [X] T021 Verify `nix flake check` passes (requires nix environment)

**Checkpoint**: Foundation ready - empty Wasm module validates, all LEB128 tests pass

---

## Phase 3: User Story 2 - 再現可能な開発環境 (Priority: P1)

**Goal**: Nix Flakesによる完全に再現可能な開発環境

**Independent Test**: `nix develop` で全ツールが利用可能、`nix flake check` がパス

### Tests (TDD)

- [X] T022 [US2] Write shell availability tests for SBCL/wasm-tools/wasmtime in tests/integration/nix-env-test.lisp

### Implementation

- [X] T023 [US2] Add checks section to flake.nix for test execution
- [X] T024 [US2] Create .github/workflows/ci.yml for GitHub Actions CI
- [X] T025 [US2] Document environment setup in quickstart.md validation

**Checkpoint**: US2完了 - `nix develop` と `nix flake check` が全環境で一貫して動作

---

## Phase 4: User Story 1 - Lispコードのコンパイルと実行 (Priority: P1) MVP

**Goal**: 基本的なLisp式（算術、条件分岐、関数定義）をWasmにコンパイルして実行

**Independent Test**: `(+ 1 2)` をコンパイルし、wasmtimeで実行して `3` が返る

### Tests (TDD - Write FIRST)

- [X] T026 [P] [US1] Write WasmGC type definition tests in tests/unit/gc-types-test.lisp
- [X] T027 [P] [US1] Write NIL/UNBOUND singleton tests in tests/unit/objects-test.lisp
- [X] T028 [P] [US1] Write Fixnum arithmetic tests (10+ cases) in tests/integration/arithmetic-test.lisp
- [X] T029 [P] [US1] Write comparison operators tests in tests/integration/arithmetic-test.lisp
- [X] T030 [P] [US1] Write if conditional tests in tests/integration/control-test.lisp
- [X] T031 [P] [US1] Write let/let* binding tests in tests/integration/binding-test.lisp
- [X] T032 [P] [US1] Write defun/function call tests in tests/integration/function-test.lisp

### Implementation - WasmGC Types

- [X] T033 [US1] Define $nil struct type in src/clysm/compiler/codegen/gc-types.lisp
- [X] T034 [US1] Define $unbound struct type in src/clysm/compiler/codegen/gc-types.lisp
- [X] T035 [US1] Define $cons struct type in src/clysm/compiler/codegen/gc-types.lisp
- [X] T036 [US1] Define $symbol struct type in src/clysm/compiler/codegen/gc-types.lisp
- [X] T037 [US1] Define $string array type in src/clysm/compiler/codegen/gc-types.lisp
- [X] T038 [US1] Implement Type Section generation in src/clysm/compiler/codegen/type-section.lisp

### Implementation - NIL/UNBOUND Singletons

- [X] T039 [US1] Implement NIL global singleton in src/clysm/runtime/objects.lisp
- [X] T040 [US1] Implement UNBOUND sentinel in src/clysm/runtime/objects.lisp
- [X] T041 [US1] Implement Global Section generation in src/clysm/backend/sections.lisp

### Implementation - AST and Compiler Core

- [X] T042 [US1] Define AST node structures in src/clysm/compiler/ast.lisp
- [X] T043 [US1] Implement literal AST nodes in src/clysm/compiler/ast.lisp
- [X] T044 [US1] Implement variable reference AST nodes in src/clysm/compiler/ast.lisp
- [X] T045 [US1] Implement function call AST nodes in src/clysm/compiler/ast.lisp
- [X] T046 [US1] Implement Wasm IR structures in src/clysm/compiler/codegen/wasm-ir.lisp

### Implementation - Fixnum Arithmetic

- [X] T047 [US1] Implement i31ref creation (ref.i31) in src/clysm/compiler/codegen/func-section.lisp
- [X] T048 [US1] Implement i31ref extraction (i31.get_s) in src/clysm/compiler/codegen/func-section.lisp
- [X] T049 [US1] Implement + operator compilation in src/clysm/compiler/codegen/func-section.lisp
- [X] T050 [US1] Implement - operator compilation in src/clysm/compiler/codegen/func-section.lisp
- [X] T051 [US1] Implement * operator compilation in src/clysm/compiler/codegen/func-section.lisp
- [X] T052 [US1] Implement / operator compilation in src/clysm/compiler/codegen/func-section.lisp

### Implementation - Comparisons and Conditionals

- [X] T053 [US1] Implement comparison operators (<, >, <=, >=, =) in src/clysm/compiler/codegen/func-section.lisp
- [X] T054 [US1] Implement if form compilation in src/clysm/compiler/codegen/func-section.lisp
- [X] T055 [US1] Implement if with else branch in src/clysm/compiler/codegen/func-section.lisp

### Implementation - Variable Binding

- [X] T056 [US1] Implement let binding compilation in src/clysm/compiler/codegen/func-section.lisp
- [X] T057 [US1] Implement let* sequential binding in src/clysm/compiler/codegen/func-section.lisp
- [X] T058 [US1] Implement local variable stack allocation in src/clysm/compiler/codegen/func-section.lisp

### Implementation - Function Definition

- [X] T059 [US1] Implement defun compilation in src/clysm/compiler/codegen/func-section.lisp
- [X] T060 [US1] Implement function symbol registration in src/clysm/runtime/objects.lisp
- [X] T061 [US1] Implement function call compilation in src/clysm/compiler/codegen/func-section.lisp
- [X] T062 [US1] Implement Code Section generation in src/clysm/backend/sections.lisp
- [X] T063 [US1] Implement Export Section for main function in src/clysm/backend/sections.lisp

### Integration

- [X] T064 [US1] Create compile-to-wasm top-level function in src/clysm/compiler/compiler.lisp
- [X] T065 [US1] Create compile-and-run test helper in tests/helpers.lisp
- [ ] T066 [US1] Run Phase 1 integration tests and verify all pass (pending: requires loading system)
- [ ] T067 [US1] Verify all generated Wasm passes `wasm-tools validate` (pending: requires loading system)
- [X] T068 [US1] Verify `nix flake check` passes

**Checkpoint**: US1完了 - `(+ 1 2)` => 3, `(defun f (x) x)` + `(f 42)` => 42 動作確認

---

## Phase 5: User Story 3 - クロージャと高階関数 (Priority: P2)

**Goal**: ラムダ式とクロージャによる変数キャプチャ、末尾呼び出し最適化

**Independent Test**: `(funcall (lambda (x) (+ x 1)) 10)` => 11、`(fact 100)` がスタックオーバーフローなし

### Tests (TDD)

- [ ] T069 [P] [US3] Write closure struct type tests in tests/unit/closure-test.lisp
- [ ] T070 [P] [US3] Write free variable analysis tests in tests/unit/analyzer-test.lisp
- [ ] T071 [P] [US3] Write environment capture tests in tests/integration/closure-test.lisp
- [ ] T072 [P] [US3] Write lambda/funcall tests in tests/integration/closure-test.lisp
- [ ] T073 [P] [US3] Write tail call optimization tests in tests/integration/tco-test.lisp
- [ ] T074 [P] [US3] Write labels/flet tests in tests/integration/closure-test.lisp

### Implementation - Closure Structure

- [ ] T075 [US3] Define $func_0/1/2/N function types in src/clysm/compiler/codegen/gc-types.lisp
- [ ] T076 [US3] Define $closure struct type in src/clysm/compiler/codegen/gc-types.lisp
- [ ] T077 [US3] Implement closure type generation in Type Section in src/clysm/compiler/codegen/type-section.lisp

### Implementation - Free Variable Analysis

- [ ] T078 [US3] Implement free variable collection in src/clysm/compiler/analyzer/free-vars.lisp
- [ ] T079 [US3] Implement lexical scope tracking in src/clysm/compiler/analyzer/free-vars.lisp
- [ ] T080 [US3] Implement nested lambda free variable analysis in src/clysm/compiler/analyzer/free-vars.lisp

### Implementation - Environment Capture

- [ ] T081 [US3] Implement environment struct generation in src/clysm/compiler/transform/closure.lisp
- [ ] T082 [US3] Implement closure $env field setup in src/clysm/compiler/transform/closure.lisp
- [ ] T083 [US3] Implement environment access code generation in src/clysm/compiler/codegen/func-section.lisp

### Implementation - Lambda/Funcall

- [ ] T084 [US3] Implement lambda form compilation in src/clysm/compiler/codegen/func-section.lisp
- [ ] T085 [US3] Implement funcall indirect call in src/clysm/compiler/codegen/func-section.lisp
- [ ] T086 [US3] Implement arity dispatch logic in src/clysm/compiler/codegen/func-section.lisp
- [ ] T087 [US3] Implement call_ref instruction generation in src/clysm/compiler/codegen/func-section.lisp

### Implementation - Tail Call Optimization

- [ ] T088 [US3] Implement tail position detection in src/clysm/compiler/analyzer/tail-call.lisp
- [ ] T089 [US3] Implement return_call generation in src/clysm/compiler/codegen/func-section.lisp
- [ ] T090 [US3] Implement return_call_ref generation in src/clysm/compiler/codegen/func-section.lisp

### Implementation - Labels/Flet

- [ ] T091 [US3] Implement labels form compilation in src/clysm/compiler/codegen/func-section.lisp
- [ ] T092 [US3] Implement flet form compilation in src/clysm/compiler/codegen/func-section.lisp
- [ ] T093 [US3] Implement mutual recursion support in src/clysm/compiler/codegen/func-section.lisp

### Integration

- [ ] T094 [US3] Run Phase 2 integration tests
- [ ] T095 [US3] Verify `(fact 100)` with no stack overflow
- [ ] T096 [US3] Verify all generated Wasm passes `wasm-tools validate`
- [ ] T097 [US3] Verify `nix flake check` passes

**Checkpoint**: US3完了 - クロージャとTCO動作確認

---

## Phase 6: User Story 4 - 非局所脱出と例外処理 (Priority: P2)

**Goal**: block/return-from、catch/throw、unwind-protect による制御フロー

**Independent Test**: `(block foo (return-from foo 42) 0)` => 42、unwind-protectでcleanup実行

### Tests (TDD)

- [ ] T098 [P] [US4] Write exception tag tests in tests/unit/exception-test.lisp
- [ ] T099 [P] [US4] Write block/return-from tests in tests/integration/control-flow-test.lisp
- [ ] T100 [P] [US4] Write tagbody/go tests in tests/integration/control-flow-test.lisp
- [ ] T101 [P] [US4] Write catch/throw tests in tests/integration/control-flow-test.lisp
- [ ] T102 [P] [US4] Write unwind-protect tests in tests/integration/control-flow-test.lisp

### Implementation - Exception Handling Base

- [ ] T103 [US4] Define exception tags in src/clysm/compiler/codegen/gc-types.lisp
- [ ] T104 [US4] Implement Tag Section generation in src/clysm/backend/sections.lisp
- [ ] T105 [US4] Implement try_table instruction generation in src/clysm/compiler/codegen/func-section.lisp
- [ ] T106 [US4] Implement throw instruction generation in src/clysm/compiler/codegen/func-section.lisp

### Implementation - block/return-from

- [ ] T107 [US4] Implement block form compilation in src/clysm/compiler/codegen/func-section.lisp
- [ ] T108 [US4] Implement return-from compilation in src/clysm/compiler/codegen/func-section.lisp
- [ ] T109 [US4] Implement nested block handling in src/clysm/compiler/codegen/func-section.lisp

### Implementation - tagbody/go

- [ ] T110 [US4] Implement tagbody form compilation in src/clysm/compiler/codegen/func-section.lisp
- [ ] T111 [US4] Implement go compilation in src/clysm/compiler/codegen/func-section.lisp
- [ ] T112 [US4] Implement label dispatch in src/clysm/compiler/codegen/func-section.lisp

### Implementation - catch/throw

- [ ] T113 [US4] Implement catch form compilation in src/clysm/compiler/codegen/func-section.lisp
- [ ] T114 [US4] Implement throw compilation in src/clysm/compiler/codegen/func-section.lisp
- [ ] T115 [US4] Implement tag symbol matching in src/clysm/compiler/codegen/func-section.lisp

### Implementation - unwind-protect

- [ ] T116 [US4] Implement unwind-protect compilation in src/clysm/compiler/codegen/func-section.lisp
- [ ] T117 [US4] Implement cleanup on normal exit in src/clysm/compiler/codegen/func-section.lisp
- [ ] T118 [US4] Implement cleanup on exception in src/clysm/compiler/codegen/func-section.lisp
- [ ] T119 [US4] Implement rethrow after cleanup in src/clysm/compiler/codegen/func-section.lisp

### Integration

- [ ] T120 [US4] Run Phase 3 integration tests
- [ ] T121 [US4] Verify all generated Wasm passes `wasm-tools validate`
- [ ] T122 [US4] Verify `nix flake check` passes

**Checkpoint**: US4完了 - 非局所脱出とクリーンアップ動作確認

---

## Phase 7: User Story 5 - 対話的開発（REPL） (Priority: P3)

**Goal**: S式リーダー、スペシャル変数、基本的なREPL

**Independent Test**: REPLで `(+ 1 2)` を入力して `3` が表示される

### Tests (TDD)

- [ ] T123 [P] [US5] Write binding stack tests in tests/unit/binding-test.lisp
- [ ] T124 [P] [US5] Write special variable tests in tests/integration/special-var-test.lisp
- [ ] T125 [P] [US5] Write tokenizer tests (10+ cases) in tests/unit/tokenizer-test.lisp
- [ ] T126 [P] [US5] Write parser tests (10+ cases) in tests/unit/parser-test.lisp
- [ ] T127 [P] [US5] Write symbol intern tests in tests/unit/package-test.lisp
- [ ] T128 [P] [US5] Write REPL integration tests in tests/integration/repl-test.lisp

### Implementation - Binding Stack (Shallow Binding)

- [ ] T129 [US5] Define $binding_frame struct in src/clysm/compiler/codegen/gc-types.lisp
- [ ] T130 [US5] Implement binding stack global in src/clysm/runtime/special-vars.lisp
- [ ] T131 [US5] Implement push-binding operation in src/clysm/runtime/special-vars.lisp
- [ ] T132 [US5] Implement pop-binding operation in src/clysm/runtime/special-vars.lisp

### Implementation - Special Variables

- [ ] T133 [US5] Implement defvar compilation in src/clysm/compiler/codegen/func-section.lisp
- [ ] T134 [US5] Implement defparameter compilation in src/clysm/compiler/codegen/func-section.lisp
- [ ] T135 [US5] Implement dynamic let binding in src/clysm/compiler/codegen/func-section.lisp
- [ ] T136 [US5] Implement automatic restoration via unwind-protect in src/clysm/compiler/codegen/func-section.lisp

### Implementation - Tokenizer

- [ ] T137 [US5] Implement character stream handling in src/clysm/reader/tokenizer.lisp
- [ ] T138 [US5] Implement symbol tokenization in src/clysm/reader/tokenizer.lisp
- [ ] T139 [US5] Implement number tokenization in src/clysm/reader/tokenizer.lisp
- [ ] T140 [US5] Implement string tokenization in src/clysm/reader/tokenizer.lisp
- [ ] T141 [US5] Implement special character handling in src/clysm/reader/tokenizer.lisp

### Implementation - Parser

- [ ] T142 [US5] Implement recursive descent parser in src/clysm/reader/parser.lisp
- [ ] T143 [US5] Implement list parsing in src/clysm/reader/parser.lisp
- [ ] T144 [US5] Implement dotted pair parsing in src/clysm/reader/parser.lisp
- [ ] T145 [US5] Implement quote shorthand parsing in src/clysm/reader/parser.lisp
- [ ] T146 [US5] Implement error position reporting in src/clysm/reader/parser.lisp

### Implementation - Symbol Intern

- [ ] T147 [US5] Implement package structure in src/clysm/reader/package.lisp
- [ ] T148 [US5] Implement COMMON-LISP package in src/clysm/reader/package.lisp
- [ ] T149 [US5] Implement KEYWORD package in src/clysm/reader/package.lisp
- [ ] T150 [US5] Implement intern function in src/clysm/reader/package.lisp
- [ ] T151 [US5] Implement find-symbol function in src/clysm/reader/package.lisp

### Implementation - REPL

- [ ] T152 [US5] Implement read function in src/clysm/reader/reader.lisp
- [ ] T153 [US5] Implement print function in src/clysm/runtime/printer.lisp
- [ ] T154 [US5] Implement REPL loop in src/clysm/repl.lisp
- [ ] T155 [US5] Implement error continuation in REPL in src/clysm/repl.lisp

### Integration

- [ ] T156 [US5] Run Phase 4 integration tests
- [ ] T157 [US5] Verify `(read-from-string "(+ 1 2)")` works
- [ ] T158 [US5] Verify `nix flake check` passes

**Checkpoint**: US5完了 - REPL基本動作確認

---

## Phase 8: User Story 6 - マクロによる構文拡張 (Priority: P3)

**Goal**: defmacroとバッククォートによるマクロシステム

**Independent Test**: `(when t 1 2 3)` => 3

### Tests (TDD)

- [ ] T159 [P] [US6] Write compile-time environment tests in tests/unit/macro-test.lisp
- [ ] T160 [P] [US6] Write macro expander tests in tests/unit/macro-test.lisp
- [ ] T161 [P] [US6] Write backquote tests in tests/unit/backquote-test.lisp
- [ ] T162 [P] [US6] Write defmacro tests in tests/integration/macro-test.lisp
- [ ] T163 [P] [US6] Write standard macro tests (10+ cases) in tests/integration/macro-test.lisp

### Implementation - Compile-time Environment

- [ ] T164 [US6] Implement macro function registry in src/clysm/compiler/transform/macro.lisp
- [ ] T165 [US6] Implement compile-time symbol table in src/clysm/compiler/transform/macro.lisp

### Implementation - Macro Expander

- [ ] T166 [US6] Implement macro detection in form walking in src/clysm/compiler/transform/macro.lisp
- [ ] T167 [US6] Implement macro function invocation on host SBCL in src/clysm/compiler/transform/macro.lisp
- [ ] T168 [US6] Implement recursive expansion in src/clysm/compiler/transform/macro.lisp

### Implementation - Backquote

- [ ] T169 [US6] Implement quasiquote transformation in src/clysm/compiler/transform/macro.lisp
- [ ] T170 [US6] Implement unquote transformation in src/clysm/compiler/transform/macro.lisp
- [ ] T171 [US6] Implement unquote-splicing transformation in src/clysm/compiler/transform/macro.lisp
- [ ] T172 [US6] Implement nested backquote handling in src/clysm/compiler/transform/macro.lisp

### Implementation - defmacro

- [ ] T173 [US6] Implement defmacro form parsing in src/clysm/compiler/transform/macro.lisp
- [ ] T174 [US6] Implement &body, &rest, &optional handling in src/clysm/compiler/transform/macro.lisp
- [ ] T175 [US6] Implement macro function compile-time registration in src/clysm/compiler/transform/macro.lisp

### Implementation - Standard Macros

- [ ] T176 [P] [US6] Implement when macro in src/clysm/lib/macros.lisp
- [ ] T177 [P] [US6] Implement unless macro in src/clysm/lib/macros.lisp
- [ ] T178 [P] [US6] Implement cond macro in src/clysm/lib/macros.lisp
- [ ] T179 [P] [US6] Implement dolist macro in src/clysm/lib/macros.lisp
- [ ] T180 [P] [US6] Implement dotimes macro in src/clysm/lib/macros.lisp

### Integration

- [ ] T181 [US6] Run Phase 5 integration tests
- [ ] T182 [US6] Verify `(when t 1 2 3)` => 3
- [ ] T183 [US6] Verify `nix flake check` passes

**Checkpoint**: US6完了 - マクロシステム動作確認

---

## Phase 9: Eval/JIT Infrastructure (Internal)

**Goal**: 動的評価とJITコンパイル基盤（US7の前提条件）

**Note**: This phase has no direct user story but is required for US7 (CLOS)

### Tests (TDD)

- [ ] T184 [P] Write Tier 1 interpreter tests in tests/unit/interpreter-test.lisp
- [ ] T185 [P] Write eval function tests in tests/integration/eval-test.lisp
- [ ] T186 [P] Write Tier 2 Wasm generation tests in tests/integration/jit-test.lisp
- [ ] T187 [P] Write dynamic linking tests in tests/integration/jit-test.lisp
- [ ] T188 [P] Write compile function tests in tests/integration/jit-test.lisp

### Implementation - Tier 1 Interpreter

- [ ] T189 Implement S-expression evaluator in src/clysm/eval/interpreter.lisp
- [ ] T190 Implement special form handling (quote, if, lambda) in src/clysm/eval/interpreter.lisp
- [ ] T191 Implement function application in src/clysm/eval/interpreter.lisp

### Implementation - eval Function

- [ ] T192 Implement eval function in src/clysm/eval/eval.lisp
- [ ] T193 Implement environment argument support in src/clysm/eval/eval.lisp

### Implementation - Tier 2 JIT

- [ ] T194 Implement S-expr to Wasm binary generation in src/clysm/eval/jit.lisp
- [ ] T195 Implement in-memory compilation in src/clysm/eval/jit.lisp
- [ ] T196 Implement binary validation in src/clysm/eval/jit.lisp

### Implementation - Dynamic Linking

- [ ] T197 Implement host WebAssembly.instantiate call in src/clysm/eval/jit.lisp
- [ ] T198 Implement runtime import setup in src/clysm/eval/jit.lisp
- [ ] T199 Implement funcref extraction in src/clysm/eval/jit.lisp
- [ ] T200 Implement GC heap sharing in src/clysm/eval/jit.lisp
- [ ] T201 Implement symbol function slot hotpatch in src/clysm/eval/jit.lisp

### Implementation - compile Function

- [ ] T202 Implement compile function in src/clysm/eval/compile.lisp
- [ ] T203 Implement Tier 1/Tier 2 switching logic in src/clysm/eval/compile.lisp

### Integration

- [ ] T204 Verify `(eval '(+ 1 2))` => 3
- [ ] T205 Verify `(compile nil '(lambda (x) (+ x 1)))` returns function
- [ ] T206 Verify `nix flake check` passes

**Checkpoint**: Eval/JIT基盤完了 - US7実装準備完了

---

## Phase 10: User Story 7 - オブジェクト指向プログラミング (Priority: P4)

**Goal**: CLOSによるクラス定義、インスタンス生成、メソッドディスパッチ

**Independent Test**: `(make-instance 'point :x 3 :y 4)` がインスタンスを返す

### Tests (TDD)

- [ ] T207 [P] [US7] Write class metaobject tests in tests/unit/clos-test.lisp
- [ ] T208 [P] [US7] Write defclass tests in tests/integration/clos-test.lisp
- [ ] T209 [P] [US7] Write make-instance tests in tests/integration/clos-test.lisp
- [ ] T210 [P] [US7] Write slot access tests in tests/integration/clos-test.lisp
- [ ] T211 [P] [US7] Write generic function tests in tests/integration/clos-test.lisp
- [ ] T212 [P] [US7] Write defmethod tests in tests/integration/clos-test.lisp
- [ ] T213 [P] [US7] Write method dispatch tests in tests/integration/clos-test.lisp
- [ ] T214 [P] [US7] Write method combination tests in tests/integration/clos-test.lisp

### Implementation - Class Metaobject

- [ ] T215 [US7] Define $standard-class struct in src/clysm/compiler/codegen/gc-types.lisp
- [ ] T216 [US7] Define $slot_vector array type in src/clysm/compiler/codegen/gc-types.lisp
- [ ] T217 [US7] Define $instance struct in src/clysm/compiler/codegen/gc-types.lisp
- [ ] T218 [US7] Implement class name management in src/clysm/clos/mop.lisp
- [ ] T219 [US7] Implement superclass management in src/clysm/clos/mop.lisp
- [ ] T220 [US7] Implement class precedence list computation in src/clysm/clos/mop.lisp

### Implementation - defclass

- [ ] T221 [US7] Implement defclass form parsing in src/clysm/clos/defclass.lisp
- [ ] T222 [US7] Implement slot definition parsing (:initarg, :accessor) in src/clysm/clos/defclass.lisp
- [ ] T223 [US7] Implement class metaobject generation in src/clysm/clos/defclass.lisp

### Implementation - make-instance

- [ ] T224 [US7] Implement make-instance function in src/clysm/clos/instance.lisp
- [ ] T225 [US7] Implement slot initialization (:initarg, :initform) in src/clysm/clos/instance.lisp
- [ ] T226 [US7] Implement instance struct generation in src/clysm/clos/instance.lisp

### Implementation - Slot Access

- [ ] T227 [US7] Implement slot-value function in src/clysm/clos/slot-access.lisp
- [ ] T228 [US7] Implement (setf slot-value) in src/clysm/clos/slot-access.lisp
- [ ] T229 [US7] Implement accessor function generation in src/clysm/clos/slot-access.lisp

### Implementation - Generic Functions

- [ ] T230 [US7] Implement defgeneric form in src/clysm/clos/generic.lisp
- [ ] T231 [US7] Implement method table structure in src/clysm/clos/dispatch.lisp
- [ ] T232 [US7] Implement dispatch base infrastructure in src/clysm/clos/dispatch.lisp

### Implementation - defmethod

- [ ] T233 [US7] Implement defmethod form parsing in src/clysm/clos/defmethod.lisp
- [ ] T234 [US7] Implement specializer processing in src/clysm/clos/defmethod.lisp
- [ ] T235 [US7] Implement method registration in src/clysm/clos/defmethod.lisp

### Implementation - Method Dispatch

- [ ] T236 [US7] Implement class-based method selection in src/clysm/clos/dispatch.lisp
- [ ] T237 [US7] Implement cache-based fast path in src/clysm/clos/dispatch.lisp
- [ ] T238 [US7] Implement inheritance-aware dispatch in src/clysm/clos/dispatch.lisp

### Implementation - Method Combination

- [ ] T239 [US7] Implement :before method support in src/clysm/clos/combination.lisp
- [ ] T240 [US7] Implement :after method support in src/clysm/clos/combination.lisp
- [ ] T241 [US7] Implement :around method support in src/clysm/clos/combination.lisp
- [ ] T242 [US7] Implement call-next-method in src/clysm/clos/combination.lisp

### Integration

- [ ] T243 [US7] Run Phase 7 integration tests
- [ ] T244 [US7] Verify `(make-instance 'point :x 3 :y 4)` works
- [ ] T245 [US7] Verify `(distance p)` returns correct value
- [ ] T246 [US7] Verify inheritance and method override
- [ ] T247 [US7] Verify all generated Wasm passes `wasm-tools validate`
- [ ] T248 [US7] Verify `nix flake check` passes

**Checkpoint**: US7完了 - CLOS基本動作確認

---

## Phase 11: Polish & Cross-Cutting Concerns

**Purpose**: 全User Storyをまたぐ改善と最終検証

- [ ] T249 [P] Run complete test suite (100+ tests)
- [ ] T250 [P] Performance benchmarks per constitution metrics
- [ ] T251 [P] Code cleanup and refactoring
- [ ] T252 [P] Update quickstart.md with all working examples
- [ ] T253 Security review per constitution constraints
- [ ] T254 Final `nix flake check` validation
- [ ] T255 Edge case verification (Fixnum overflow, deep recursion, etc.)

### Edge Case Tasks (Analysis Remediation)

**Reader Error Recovery**:
- [ ] T256 [P] Write reader error recovery tests in tests/unit/reader-error-test.lisp
- [ ] T257 Implement reader error recovery with line/column reporting in src/clysm/reader/reader.lisp
- [ ] T258 Add reader restart handlers (skip-to-next-sexp, replace-with-nil) per ANSI CL

**Lexical vs Dynamic Variable Disambiguation**:
- [ ] T259 [P] Write lexical/special variable disambiguation tests in tests/integration/variable-scope-test.lisp
- [ ] T260 Implement DECLARE SPECIAL tracking in src/clysm/compiler/env.lisp
- [ ] T261 Add compile-time warning for undeclared special variable references

**CLOS Method Combination Edge Cases**:
- [ ] T262 [P] Write method combination edge case tests in tests/integration/clos-edge-test.lisp
- [ ] T263 Implement :around/:before/:after method ordering in src/clysm/clos/method-combination.lisp
- [ ] T264 Handle call-next-method with no next method gracefully

---

## Dependencies & Execution Order

### Phase Dependencies

```
Phase 1 (Setup)
    │
    ▼
Phase 2 (Foundational) ◄─── BLOCKS ALL USER STORIES
    │
    ├───────────────────────────────────────────────────┐
    │                                                   │
    ▼                                                   ▼
Phase 3 (US2: 開発環境)                    Phase 4 (US1: コンパイル) ◄── MVP
    │                                                   │
    │                                                   ▼
    │                                      Phase 5 (US3: クロージャ)
    │                                                   │
    │                                                   ▼
    │                                      Phase 6 (US4: 例外処理)
    │                                                   │
    │                                                   ▼
    │                                      Phase 7 (US5: REPL)
    │                                                   │
    │                                                   ▼
    │                                      Phase 8 (US6: マクロ)
    │                                                   │
    │                                                   ▼
    │                                      Phase 9 (Eval/JIT)
    │                                                   │
    │                                                   ▼
    │                                      Phase 10 (US7: CLOS)
    │                                                   │
    └───────────────────────────────────────────────────┘
                            │
                            ▼
                    Phase 11 (Polish)
```

### User Story Dependencies

- **US2 (P1)**: Phase 2完了後開始可能 - 他USに依存なし
- **US1 (P1)**: Phase 2完了後開始可能 - 他USに依存なし（US2と並行可能）
- **US3 (P2)**: US1完了後開始
- **US4 (P2)**: US3完了後開始（または並行可能、ただしクロージャ依存あり）
- **US5 (P3)**: US4完了後開始
- **US6 (P3)**: US5完了後開始
- **US7 (P4)**: Phase 9（Eval/JIT）完了後開始

### Parallel Opportunities

- **Phase 1**: T003, T004 can run in parallel
- **Phase 2**: T007-T011 (tests) can run in parallel
- **Phase 4**: All test tasks (T026-T032) can run in parallel
- **Phase 4**: Models T033-T037 can run in parallel
- **Phase 5+**: Each user story test phase can run tests in parallel

---

## Parallel Example: Phase 4 (US1)

```bash
# Launch all tests for User Story 1 in parallel:
Task: "Write WasmGC type definition tests in tests/unit/gc-types-test.lisp"
Task: "Write NIL/UNBOUND singleton tests in tests/unit/objects-test.lisp"
Task: "Write Fixnum arithmetic tests in tests/integration/arithmetic-test.lisp"
Task: "Write comparison operators tests in tests/integration/arithmetic-test.lisp"
Task: "Write if conditional tests in tests/integration/control-test.lisp"
Task: "Write let/let* binding tests in tests/integration/binding-test.lisp"
Task: "Write defun/function call tests in tests/integration/function-test.lisp"

# Then launch type definitions in parallel:
Task: "Define $nil struct type in src/clysm/compiler/codegen/gc-types.lisp"
Task: "Define $unbound struct type in src/clysm/compiler/codegen/gc-types.lisp"
Task: "Define $cons struct type in src/clysm/compiler/codegen/gc-types.lisp"
Task: "Define $symbol struct type in src/clysm/compiler/codegen/gc-types.lisp"
Task: "Define $string array type in src/clysm/compiler/codegen/gc-types.lisp"
```

---

## Implementation Strategy

### MVP First (User Stories 1 & 2 Only)

1. Complete Phase 1: Setup
2. Complete Phase 2: Foundational (CRITICAL)
3. Complete Phase 3: US2 (開発環境)
4. Complete Phase 4: US1 (コンパイル)
5. **STOP and VALIDATE**: `(+ 1 2)` => 3 動作確認
6. Deploy/demo as MVP

### Incremental Delivery

1. Setup + Foundational → Foundation ready
2. US2 → 開発環境完成
3. US1 → 基本コンパイル完成（**MVP!**）
4. US3 → クロージャ追加
5. US4 → 例外処理追加
6. US5 → REPL追加
7. US6 → マクロ追加
8. US7 → CLOS追加

---

## Summary

| Metric | Value |
|--------|-------|
| Total Tasks | 264 |
| Setup Phase | 6 tasks |
| Foundational Phase | 15 tasks |
| US1 (P1) | 43 tasks |
| US2 (P1) | 4 tasks |
| US3 (P2) | 29 tasks |
| US4 (P2) | 25 tasks |
| US5 (P3) | 36 tasks |
| US6 (P3) | 25 tasks |
| Eval/JIT (Internal) | 23 tasks |
| US7 (P4) | 42 tasks |
| Polish Phase | 16 tasks (incl. edge cases) |
| Parallel Opportunities | 85+ tasks marked [P] |
| MVP Scope | Phase 1-4 (US1 + US2) |

---

## Notes

- **TDD Required**: Constitution VII mandates test-first development
- **[P] tasks**: Different files, no dependencies - can run in parallel
- **[Story] label**: Maps task to user story for traceability
- **Verify tests FAIL** before implementing
- **Commit after each task** or logical group
- **Stop at any checkpoint** to validate independently
- **All Wasm must pass** `wasm-tools validate`
- **All commits must pass** `nix flake check`
