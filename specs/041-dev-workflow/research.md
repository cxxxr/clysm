# Research: Development Workflow Establishment

**Date**: 2025-12-27
**Feature**: 041-dev-workflow
**Status**: Complete

## R1: CLI Argument Parsing Strategy

### Decision: Option A - Minimal Hand-Written Parser

### Rationale

A minimal hand-written argument parser is the best choice for Clysm because:

1. **Blessed Subset Compatibility**: Complex macro systems or regex-based parsing would require CL features not yet in Clysm's blessed subset
2. **Self-Hosting Requirement**: The parser must work in both SBCL (bootstrap) and Stage 1 (self-hosted)
3. **Limited Scope**: The CLI has only ~10 flags, not justifying a full parsing framework
4. **Predictable Behavior**: Hand-written code is easier to debug in Wasm

### Implementation Approach

```lisp
;; Simple recursive descent for: ./clysm compile <patterns...> -o FILE [--verbose] [--force]
(defun parse-args (args)
  "Parse command-line arguments into option alist."
  (let ((result nil)
        (positional nil))
    (loop while args
          for arg = (pop args)
          do (cond
               ((string= arg "-o") (push (cons :output (pop args)) result))
               ((string= arg "--output") (push (cons :output (pop args)) result))
               ((string= arg "--verbose") (push (cons :verbose t) result))
               ((string= arg "--force") (push (cons :force t) result))
               ((char= (char arg 0) #\-) (error "Unknown option: ~a" arg))
               (t (push arg positional))))
    (values (nreverse result) (nreverse positional))))
```

### Alternatives Rejected

| Alternative | Why Rejected |
|------------|--------------|
| B: Lisp macros | Requires `defmacro` working in Stage 1, adds complexity |
| C: Port clingon | Too large (~2000 LOC), not in blessed subset |
| getopt-style | Requires string processing not in blessed subset |

---

## R2: Glob Pattern Implementation

### Decision: Option C - Host-Side Glob Expansion via FFI

### Rationale

Host-side glob expansion is optimal because:

1. **Platform Native**: Node.js/wasmtime host has access to native `glob` libraries
2. **Filesystem Access**: Glob requires directory traversal, which is already FFI-mediated
3. **Pattern Variations**: `**` (globstar) semantics vary by platform; host handles this
4. **Simplicity**: Clysm code receives a list of paths, no pattern matching needed

### Implementation Approach

**FFI Interface**:
```lisp
;; FFI declaration
(ffi:define-foreign-function glob-expand "fs.glob" (:string) :anyref)
;; Returns array of matching file paths

;; Usage
(let ((paths (glob-expand "src/**/*.lisp")))
  (map 'list #'identity paths))  ; Convert to list
```

**Host Shim (Node.js)**:
```javascript
// host-shim/workflow-host.js
const glob = require('glob');
exports['fs.glob'] = (pattern) => {
  return glob.sync(pattern, { nodir: true });
};
```

**SBCL Bootstrap Path**:
```lisp
;; For SBCL, use UIOP's directory traversal
(defun glob-expand-sbcl (pattern)
  (uiop:directory-files (pathname pattern)))
```

### Alternatives Rejected

| Alternative | Why Rejected |
|------------|--------------|
| A: cl-ppcre port | Regex engine is ~5000 LOC, not in blessed subset |
| B: Hand-written | Globstar (**) is complex; reinventing the wheel |
| Pure Lisp | Requires DIRECTORY which needs deep FFI |

---

## R3: Dependency Discovery Algorithm

### Decision: Option A - Parse `in-package` and Module List

### Rationale

Using `in-package` declarations plus a predefined module order is sufficient because:

1. **Existing Infrastructure**: reader.lisp already has 41 modules in dependency order
2. **Simple Extraction**: `in-package` forms are trivial to parse
3. **Incremental Scope**: For this feature, we need "which files changed" not "full symbol tracking"
4. **Conservative Approach**: Recompile all dependents when a file changes

### Implementation Approach

```lisp
;; Extract package from source file
(defun extract-package (forms)
  "Return package name from (in-package X) form."
  (loop for form in forms
        when (and (consp form) (eq (car form) 'in-package))
        return (second form)))

;; Dependency graph from module order
(defstruct dependency-graph
  (modules nil :type list)           ; List of SourceModule
  (order nil :type list)             ; Topological order (path list)
  (dependents nil :type hash-table)) ; path -> list of dependent paths

;; Build from predefined order
(defun build-dependency-graph (modules)
  "Build graph where later modules depend on earlier ones."
  (let ((graph (make-dependency-graph))
        (order (mapcar #'source-module-path modules)))
    (setf (dependency-graph-order graph) order)
    ;; Each module depends on all modules before it
    (loop for i from 0 below (length modules)
          for mod = (nth i modules)
          do (setf (gethash (source-module-path mod)
                            (dependency-graph-dependents graph))
                   (subseq order (1+ i))))
    graph))
```

### Incremental Recompilation Logic

1. **Changed file detected** → Mark as dirty
2. **All dependents marked** → Files after the changed file in order
3. **Recompile dirty files only** → Skip unchanged files
4. **Update cache** → Store new timestamps and compiled bytes

### Alternatives Rejected

| Alternative | Why Rejected |
|------------|--------------|
| B: Full symbol tracking | Requires macro-expansion of all forms; too complex |
| C: Explicit declarations | Requires changing all source files; invasive |
| Per-form dependencies | Existing reader doesn't track cross-file refs |

---

## R4: Cache Serialization Format

### Decision: Option A - S-expression Format

### Rationale

S-expressions are ideal for Clysm's compilation cache because:

1. **Native Read/Write**: `read` and `print` are in the blessed subset
2. **Human Readable**: Developers can inspect cache manually
3. **No External Dependencies**: No JSON parser needed
4. **Lisp Data Structures**: Direct serialization of structs via print-object

### Implementation Approach

**Cache Structure**:
```lisp
;; .clysm-cache/compilation-cache.sexp
(:version 1
 :timestamp "2025-12-27T12:00:00Z"
 :modules
 ((:path "src/clysm/backend/leb128.lisp"
   :mtime 1735300800
   :hash "a1b2c3d4..."
   :compiled-size 1234
   :dependencies ())
  (:path "src/clysm/backend/sections.lisp"
   :mtime 1735300900
   :hash "e5f6g7h8..."
   :compiled-size 2345
   :dependencies ("src/clysm/backend/leb128.lisp"))))
```

**Read/Write Functions**:
```lisp
(defun load-cache (cache-path)
  "Load compilation cache from file, return nil if not found."
  (when (probe-file cache-path)
    (with-open-file (s cache-path :direction :input)
      (read s))))

(defun save-cache (cache cache-path)
  "Write compilation cache to file."
  (ensure-directories-exist cache-path)
  (with-open-file (s cache-path :direction :output :if-exists :supersede)
    (write cache :stream s :readably t)))
```

### Cache Directory Layout

```text
.clysm-cache/
├── compilation-cache.sexp   # Module metadata
├── modules/                 # Compiled Wasm fragments (optional)
│   ├── leb128.wasm
│   └── sections.wasm
└── dependencies.sexp        # Dependency graph snapshot
```

### Alternatives Rejected

| Alternative | Why Rejected |
|------------|--------------|
| B: JSON | Requires JSON parser; not in blessed subset |
| C: Binary | Not human-readable; debugging harder |
| SQLite | External dependency; overkill for 50 modules |

---

## R5: Self-Hosting Bootstrap Strategy

### Decision: Dual-Path Execution with Gradual Migration

### Rationale

A dual-path strategy enables incremental self-hosting:

1. **SBCL Path**: Initial development and bootstrap (existing)
2. **Stage 1 Path**: Self-hosted compilation (new)
3. **Shared Core**: Same source code, different entry points

### Implementation Approach

**Bootstrap Hierarchy**:
```
Level 0: SBCL + Quicklisp
  └── Compiles Clysm source → Stage 0 binary (minimal)

Level 1: Stage 0 + wasmtime
  └── Compiles Clysm source → Stage 1 binary (fuller)

Level 2: Stage 1 + wasmtime
  └── Compiles Clysm source → Stage 2 binary (self-hosting)

Level 3: Stage 2 + wasmtime  (Fixed-point achieved)
  └── Compiles Clysm source → Stage 3 binary (identical to Stage 2)
```

**CLI Entry Points**:

1. **SBCL-based** (bootstrapping):
```bash
# bin/clysm (shell script)
#!/bin/bash
if command -v sbcl &>/dev/null; then
  sbcl --load build/workflow.lisp -- "$@"
else
  # Fall through to Stage 1
  wasmtime run dist/clysm-stage1.wasm --invoke clysm_main -- "$@"
fi
```

2. **Stage 1-based** (self-hosted):
```javascript
// host-shim/workflow-host.js
const wasm = await WebAssembly.instantiate(stage1Binary, imports);
wasm.instance.exports.clysm_main(argsArray);
```

**Shared Compilation Core**:
```lisp
;; src/clysm/workflow/compiler.lisp
(defun compile-project (patterns &key output verbose force)
  "Main compilation entry point - works in both SBCL and Stage 1."
  (let* ((paths (expand-globs patterns))       ; FFI or UIOP
         (modules (read-all-sources paths))    ; Existing reader
         (graph (build-dependency-graph modules))
         (cache (load-cache))
         (dirty (find-dirty-modules modules cache force)))
    (compile-modules dirty graph
                     :output output
                     :verbose verbose)))
```

### FFI Abstraction Layer

To support dual execution, an abstraction layer wraps platform-specific operations:

```lisp
;; src/clysm/workflow/platform.lisp
(defun expand-globs (patterns)
  "Expand glob patterns to file paths."
  #+clysm-stage1 (ffi:call-host "fs.glob" patterns)
  #-clysm-stage1 (apply #'append (mapcar #'uiop:directory-files patterns)))

(defun file-mtime (path)
  "Get file modification time."
  #+clysm-stage1 (ffi:call-host "fs.mtime" path)
  #-clysm-stage1 (file-write-date path))

(defun read-file-string (path)
  "Read file as UTF-8 string."
  #+clysm-stage1 (ffi:call-host "fs.read" path)
  #-clysm-stage1 (uiop:read-file-string path))
```

### Migration Timeline

| Phase | Capability | Requirement |
|-------|-----------|-------------|
| MVP | SBCL-only CLI (`./clysm compile`) | Feature 041 core |
| Self-Hosted | Stage 1 can compile test files | Extended FFI |
| Full Self-Hosting | Stage 1 can compile Clysm source | Fixed-point (Feature 040) |

### Alternatives Rejected

| Alternative | Why Rejected |
|------------|--------------|
| SBCL-only | Defeats self-hosting goal |
| Stage 1-only | No bootstrap path |
| Separate codebases | Maintenance nightmare |

---

## Summary of Decisions

| Research Area | Decision | Key Benefit |
|--------------|----------|-------------|
| R1: CLI Parsing | Hand-written minimal | Blessed subset compatible |
| R2: Glob Patterns | Host-side FFI | Platform-native behavior |
| R3: Dependencies | in-package + module order | Reuses existing infrastructure |
| R4: Cache Format | S-expressions | Native Lisp read/write |
| R5: Bootstrap | Dual-path with shared core | Gradual self-hosting |

All research areas resolved. Proceed to Phase 1 design.
