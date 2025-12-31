# Quickstart: Phase 13D M4 - DEFUN Blocker Analysis

## Prerequisites

```bash
# Enter development environment
nix develop

# Verify tools are available
sbcl --version      # SBCL 2.4+
wasm-tools --version
```

## Quick Verification

### 1. Run Stage 1 with Error Logging

```bash
# Generate Stage 1 with enhanced error reporting
sbcl --load build/stage1-complete.lisp

# Check the reports
cat dist/stage1-report.json | jq '.summary'
cat dist/stage1-report.json | jq '.error_patterns[:5]'
```

### 2. Check DEFUN Error Log

```bash
# View detailed error log
cat dist/defun-errors.json | jq '.total_entries'

# View top patterns
cat dist/defun-errors.json | jq '[.entries | group_by(.pattern_id) | .[] | {pattern: .[0].pattern_id, count: length}] | sort_by(-.count)[:10]'
```

### 3. Validate Wasm Output

```bash
wasm-tools validate dist/clysm-stage1.wasm
```

### 4. Check Target Modules

```bash
# Check backend/ module compilation
cat dist/stage1-report.json | jq '.modules[] | select(.path | contains("backend/"))'

# Check reader/ module compilation
cat dist/stage1-report.json | jq '.modules[] | select(.path | contains("reader/"))'
```

## Success Criteria Verification

```bash
# SC-001: Compilation rate >= 35%
cat dist/stage1-report.json | jq '.summary.coverage_pct >= 35'

# SC-002: DEFUN failures <= 15,000
cat dist/stage1-report.json | jq '.summary.defun_failures <= 15000'

# SC-003/SC-004: Module 100% compilation
cat dist/stage1-report.json | jq '.modules[] | select(.path | contains("backend/") or contains("reader/")) | select(.failed > 0)'
# Should return empty if modules are fully compiled

# SC-005: Top 10 patterns cover >= 80%
cat dist/stage1-report.json | jq '[.error_patterns[:10] | .[].percentage] | add >= 80'
```

## Run Tests

```bash
# Run all tests
sbcl --eval "(asdf:test-system :clysm)"

# Run specific error analysis tests
sbcl --eval "(asdf:test-system :clysm/tests/error-analysis)"

# Run lambda-list tests
sbcl --eval "(asdf:test-system :clysm/tests/lambda-list)"
```

## Development Workflow

1. **Identify top error pattern**:
   ```bash
   cat dist/stage1-report.json | jq '.error_patterns[0]'
   ```

2. **Find example failures**:
   ```bash
   cat dist/defun-errors.json | jq '.entries[] | select(.pattern_id == "P001") | {fn: .function_name, msg: .error_message}' | head -20
   ```

3. **Implement fix in codegen**:
   - Edit `src/clysm/compiler/codegen/func-section.lisp`
   - Add test case first (TDD)

4. **Verify fix**:
   ```bash
   sbcl --load build/stage1-complete.lisp
   cat dist/stage1-report.json | jq '.summary'
   ```

5. **Repeat until targets met**

## Common Issues

### "Unknown function" errors
- Usually means a function called in DEFUN body isn't yet compiled
- Check if function needs to be added as a primitive

### "&aux init form failed"
- Complex init forms may reference undefined functions
- Simplify init form or ensure dependencies compile first

### "type mismatch" validation errors
- Wasm type system mismatch in generated code
- Check codegen for the specific construct

## Files Reference

| File | Purpose |
|------|---------|
| `build/stage1-complete.lisp` | Stage 1 generation script |
| `src/clysm/stage0/error-analysis.lisp` | Error classification |
| `dist/stage1-report.json` | Compilation report |
| `dist/defun-errors.json` | Detailed error log |
