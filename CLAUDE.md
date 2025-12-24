# clysm3 Development Guidelines

Auto-generated from all feature plans. Last updated: 2025-12-21

## Active Technologies
- Common Lisp (SBCL 2.4+) + alexandria, babel, trivial-gray-streams, rove (testing) (002-special-vars-compiler)
- N/A (compiler generates Wasm binaries) (002-special-vars-compiler)
- Common Lisp (SBCL 2.4+) - コンパイラ本体 + alexandria, babel, trivial-gray-streams, rove (testing) (007-sequence-functions)
- N/A (コンパイラはWasmバイナリを生成) (007-sequence-functions)
- N/A（コンパイラはWasmバイナリを生成） (008-character-string)
- Common Lisp (SBCL 2.4+) for compiler, WasmGC for output + alexandria, babel, trivial-gray-streams, rove (testing) (010-numeric-tower)
- Common Lisp (SBCL 2.4+) - コンパイラ実装言語 + alexandria, babel, trivial-gray-streams, rove (testing) (013-package-system)
- N/A（インメモリハッシュテーブル） (013-package-system)
- Common Lisp (SBCL 2.4+) - compiler implementation language + alexandria, babel, trivial-gray-streams, rove (testing) (014-condition-system)
- N/A (in-memory condition/handler/restart stacks) (014-condition-system)
- Common Lisp (SBCL 2.4+) - compiler implementation; WasmGC - output target + alexandria, babel (UTF-8), trivial-gray-streams, rove (testing); existing FFI foundation (012), condition system (014), special variables (002), character/string types (008) (015-ffi-stream-io)
- N/A (in-memory streams to host file descriptors) (015-ffi-stream-io)

- Common Lisp (SBCL 2.4+) - コンパイラ本体、WAT/Wasm - 出力 (001-clysm-compiler)

## Project Structure

```text
src/
tests/
```

## Commands

# Add commands for Common Lisp (SBCL 2.4+) - コンパイラ本体、WAT/Wasm - 出力

## Code Style

Common Lisp (SBCL 2.4+) - コンパイラ本体、WAT/Wasm - 出力: Follow standard conventions

## Recent Changes
- 015-ffi-stream-io: Added Common Lisp (SBCL 2.4+) - compiler implementation; WasmGC - output target + alexandria, babel (UTF-8), trivial-gray-streams, rove (testing); existing FFI foundation (012), condition system (014), special variables (002), character/string types (008)
- 014-condition-system: Added Common Lisp (SBCL 2.4+) - compiler implementation language + alexandria, babel, trivial-gray-streams, rove (testing)
- 013-package-system: Added Common Lisp (SBCL 2.4+) - コンパイラ実装言語 + alexandria, babel, trivial-gray-streams, rove (testing)


<!-- MANUAL ADDITIONS START -->
<!-- MANUAL ADDITIONS END -->
