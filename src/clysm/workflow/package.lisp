;;;; package.lisp - Package definition for development workflow module
;;;;
;;;; Part of Feature 041: Development Workflow Establishment
;;;; Provides infrastructure for CLI compilation, incremental builds, and REPL integration

(defpackage #:clysm/workflow
  (:use #:cl)
  (:documentation "Development workflow infrastructure for Clysm.
Provides:
- Source module discovery and parsing
- Dependency graph construction and topological sorting
- Incremental compilation with cache persistence
- Error recovery and reporting
- REPL compile-file integration")
  ;; Types (from types.lisp)
  (:export #:source-module
           #:make-source-module
           #:source-module-path
           #:source-module-relative-path
           #:source-module-package
           #:source-module-mtime
           #:source-module-hash
           #:source-module-forms
           #:source-module-form-count
           #:source-module-dependencies
           #:source-module-status

           #:dependency-graph
           #:make-dependency-graph
           #:dependency-graph-modules
           #:dependency-graph-path-index
           #:dependency-graph-order
           #:dependency-graph-dependents
           #:dependency-graph-dependencies

           #:compilation-cache
           #:make-compilation-cache
           #:compilation-cache-version
           #:compilation-cache-timestamp
           #:compilation-cache-project-root
           #:compilation-cache-modules
           #:compilation-cache-dependency-graph

           #:cached-module
           #:make-cached-module
           #:cached-module-path
           #:cached-module-mtime
           #:cached-module-hash
           #:cached-module-compiled-size
           #:cached-module-compiled-bytes
           #:cached-module-compile-time-ms
           #:cached-module-form-results

           #:compilation-result
           #:make-compilation-result
           #:compilation-result-module
           #:compilation-result-success-p
           #:compilation-result-wasm-bytes
           #:compilation-result-byte-count
           #:compilation-result-form-count
           #:compilation-result-forms-compiled
           #:compilation-result-forms-failed
           #:compilation-result-forms-skipped
           #:compilation-result-errors
           #:compilation-result-warnings
           #:compilation-result-compile-time-ms

           #:compilation-error
           #:make-compilation-error
           #:compilation-error-severity
           #:compilation-error-message
           #:compilation-error-path
           #:compilation-error-line
           #:compilation-error-column
           #:compilation-error-form
           #:compilation-error-operator
           #:compilation-error-suggestion

           #:compilation-session
           #:make-compilation-session
           #:compilation-session-start-time
           #:compilation-session-project-root
           #:compilation-session-output-path
           #:compilation-session-input-patterns
           #:compilation-session-modules
           #:compilation-session-dirty-modules
           #:compilation-session-results
           #:compilation-session-errors
           #:compilation-session-progress
           #:compilation-session-options

           #:compilation-options
           #:make-compilation-options
           #:compilation-options-output
           #:compilation-options-verbose
           #:compilation-options-force
           #:compilation-options-continue-on-error
           #:compilation-options-cache-path
           #:compilation-options-progress-callback

           #:progress-info
           #:make-progress-info
           #:progress-info-phase
           #:progress-info-total-modules
           #:progress-info-current-module
           #:progress-info-current-path
           #:progress-info-percentage
           #:progress-info-message)
  ;; Platform abstraction (from platform.lisp)
  (:export #:glob-expand
           #:file-mtime
           #:read-file-string
           #:file-exists-p
           #:ensure-directory)
  ;; Dependency analysis (from deps.lisp)
  (:export #:extract-package
           #:build-dependency-graph
           #:get-dependents
           #:get-compilation-order
           #:detect-cycles)
  ;; Cache management (from cache.lisp)
  (:export #:load-cache
           #:save-cache
           #:find-dirty-modules
           #:prune-deleted-files
           #:invalidate-module)
  ;; Compiler interface (from compiler.lisp)
  (:export #:read-source-modules
           #:compile-module
           #:compile-project
           #:progress-display
           ;; Error handling (US3)
           #:format-error
           #:display-error-summary
           #:report-errors-to-stderr)
  ;; REPL interface (from repl.lisp)
  (:export #:compile-file*
           #:compile-file-error
           #:compile-file-warning
           #:compile-file-not-found
           #:*clysm-compile-verbose*
           #:*clysm-compile-print*))
