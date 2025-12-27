# Quickstart: FORMAT Function Foundation

**Feature**: 032-format-function
**Date**: 2025-12-27

## Prerequisites

- Nix with flakes enabled
- Repository cloned and on `032-format-function` branch

```bash
git checkout 032-format-function
nix develop
```

## Running Tests

### All FORMAT Tests

```bash
# Run all format-related tests
nix develop -c sbcl --eval '(ql:quickload :clysm/test)' \
                    --eval '(rove:run :clysm/test/format)' \
                    --quit
```

### Specific Test Suites

```bash
# Unit tests only
nix develop -c sbcl --eval '(ql:quickload :clysm/test)' \
                    --eval '(rove:run :clysm/test/format/unit)' \
                    --quit

# Integration tests only
nix develop -c sbcl --eval '(ql:quickload :clysm/test)' \
                    --eval '(rove:run :clysm/test/format/integration)' \
                    --quit
```

### Full CI Check

```bash
nix flake check
```

## Quick Verification

### REPL Session

```bash
nix develop -c sbcl --eval '(ql:quickload :clysm)'
```

```lisp
;; Basic directives (already implemented)
(clysm/streams:format nil "Hello ~A!" "World")
;; => "Hello World!"

(clysm/streams:format nil "Value: ~S" 'symbol)
;; => "Value: SYMBOL"

(clysm/streams:format nil "Count: ~D" 42)
;; => "Count: 42"

;; New directives (after implementation)

;; Fresh-line
(clysm/streams:format nil "Line1~&Line2")
;; => "Line1
;; Line2"  ; newline only if not already at column 0

;; Iteration
(clysm/streams:format nil "Items: ~{~A~^, ~}" '(a b c))
;; => "Items: a, b, c"

;; Conditional
(clysm/streams:format nil "~[zero~;one~;two~:;many~]" 1)
;; => "one"

(clysm/streams:format nil "~:[false~;true~]" t)
;; => "true"

;; Recursive
(clysm/streams:format nil "~?" "Value: ~A" '(42))
;; => "Value: 42"
```

## Key Files

| File | Purpose |
|------|---------|
| `src/clysm/streams/format.lisp` | FORMAT implementation |
| `src/clysm/conditions/types.lisp` | format-error condition |
| `tests/unit/format/*.lisp` | Unit tests by directive |
| `tests/integration/format-ansi-test.lisp` | ANSI compliance |

## Development Workflow

### 1. Write Test First (TDD)

```lisp
;; tests/unit/format/iteration-test.lisp
(deftest iteration-basic
  (testing "~{~} processes list elements"
    (ok (string= (format nil "~{~A~}" '(1 2 3))
                 "123"))))
```

### 2. Run Test (Expect Failure)

```bash
nix develop -c sbcl --eval '(ql:quickload :clysm/test)' \
                    --eval '(rove:run :clysm/test/format/iteration)' \
                    --quit
```

### 3. Implement Feature

Edit `src/clysm/streams/format.lisp`.

### 4. Run Test (Expect Pass)

```bash
nix develop -c sbcl --eval '(ql:quickload :clysm/test)' \
                    --eval '(rove:run :clysm/test/format/iteration)' \
                    --quit
```

### 5. Refactor and Full Check

```bash
nix flake check
```

## Common Tasks

### Adding a New Directive

1. Add directive type to `parse-directive` dispatch
2. Create directive struct (if nested)
3. Add execution handler in `execute-directive`
4. Write unit tests for new directive
5. Update exports in `src/clysm/streams/package.lisp`

### Debugging Format Strings

```lisp
;; Parse and inspect directive structure
(clysm/streams::parse-format-string "~{~A~^, ~}")

;; Trace format execution
(trace clysm/streams::execute-directive)
(format nil "~{~A~}" '(1 2 3))
```

## Self-Hosting Verification

After implementation, verify compiler still works:

```bash
# Compile a test file with Clysm
nix develop -c sbcl --eval '(ql:quickload :clysm)' \
                    --eval '(clysm:compile-file "test.lisp")' \
                    --quit

# Run ANSI test subset
nix develop -c sbcl --eval '(ql:quickload :clysm/ansi-test)' \
                    --eval '(clysm/ansi-test:run-format-tests)' \
                    --quit
```
