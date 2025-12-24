# Quickstart: Fix FFI Streams Module

**Feature**: 018-fix-ffi-streams
**Date**: 2025-12-24

## Prerequisites

```bash
# Enter Nix development shell
nix develop

# Verify SBCL is available
sbcl --version
```

## Step 1: Verify Current State (Before Fix)

```bash
# Run existing tests to establish baseline
cd /home/user/src/clysm-workbench/clysm3
sbcl --eval "(require :asdf)" \
     --eval "(asdf:test-system :clysm)" \
     --quit
```

Expected: All tests pass (streams module is disabled, so no stream tests run).

## Step 2: Enable Streams Module

Edit `clysm.asd` and uncomment:
1. Lines 115-125: Streams module
2. Lines 193-197: Stream unit tests
3. Lines 205-210: Stream integration tests

## Step 3: Attempt Load (Identify Issues)

```bash
sbcl --eval "(require :asdf)" \
     --eval "(asdf:load-system :clysm)" \
     --quit
```

Expected: May fail with package errors. Document exact error.

## Step 4: Apply Fixes

Based on research.md findings:
1. Add stream test package definitions to `tests/package.lisp`
2. Update test file package declarations
3. Verify gc-types exports

## Step 5: Verify Load

```bash
sbcl --eval "(require :asdf)" \
     --eval "(asdf:load-system :clysm)" \
     --eval "(format t \"~%Streams module loaded successfully~%\")" \
     --quit
```

## Step 6: Run Full Test Suite

```bash
sbcl --eval "(require :asdf)" \
     --eval "(asdf:test-system :clysm)" \
     --quit
```

Expected: All tests pass (100% success rate).

## Step 7: ANSI CL Compliance Check

```bash
sbcl --eval "(require :asdf)" \
     --eval "(asdf:load-system :clysm)" \
     --eval "
       (in-package :clysm/streams)
       (format t \"~%Testing format directives:~%\")
       (format t \"~A -> ~A~%\" '|~A| (format nil \"~A\" \"test\"))
       (format t \"~A -> ~A~%\" '|~S| (format nil \"~S\" \"test\"))
       (format t \"~A -> ~A~%\" '|~D| (format nil \"~D\" 42))
       (format t \"~A -> ~A~%\" '|~%| (format nil \"~%\"))
       (format t \"~A -> ~A~%\" '|~~| (format nil \"~~\"))
       (format t \"~%Stream predicates:~%\")
       (format t \"streamp *standard-output* -> ~A~%\" (streamp *standard-output*))
       (format t \"output-stream-p *standard-output* -> ~A~%\" (output-stream-p *standard-output*))
       (format t \"input-stream-p *standard-input* -> ~A~%\" (input-stream-p *standard-input*))
     " \
     --quit
```

## Verification Checklist

- [ ] `(asdf:load-system :clysm)` succeeds without errors
- [ ] `(asdf:test-system :clysm)` shows 100% pass rate
- [ ] Stream unit tests (4 files) all pass
- [ ] Stream integration tests pass
- [ ] No regressions in existing tests
- [ ] Format directives produce correct output
- [ ] Stream predicates return expected values

## Troubleshooting

### Error: "Package CLYSM/STREAMS does not exist"
**Cause**: Module not enabled or load order issue
**Fix**: Verify streams module is uncommented in clysm.asd

### Error: "Symbol X not exported from package Y"
**Cause**: Missing export in source package
**Fix**: Add export to relevant package definition

### Error: "The function X is undefined"
**Cause**: File not compiled or load order wrong
**Fix**: Clear ASDF cache and reload: `(asdf:clear-system :clysm)`

## Next Steps

After successful fix:
1. Commit with conventional commit message
2. Document root cause in commit body
3. Run `nix flake check` to verify CI passes
