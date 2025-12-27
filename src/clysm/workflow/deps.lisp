;;;; deps.lisp - Dependency analysis for development workflow
;;;;
;;;; Part of Feature 041: Development Workflow Establishment
;;;; Implements dependency graph construction and topological sorting

(in-package #:clysm/workflow)

;;; ============================================================
;;; T039: extract-package - Extract package name from source
;;; ============================================================

(defun extract-package (forms)
  "Extract the package name from FORMS.
   Returns the package symbol from the first (in-package ...) form found,
   or NIL if no in-package form is present."
  (dolist (form forms)
    (when (and (consp form)
               (eq (car form) 'in-package)
               (>= (length form) 2))
      (let ((pkg-spec (cadr form)))
        (return-from extract-package
          (etypecase pkg-spec
            (symbol pkg-spec)
            (string (intern (string-upcase pkg-spec) :keyword))
            (cons (if (eq (car pkg-spec) 'quote)
                      (cadr pkg-spec)
                      nil)))))))
  nil)

;;; ============================================================
;;; T040: build-dependency-graph - Build dependency graph from modules
;;; ============================================================

(defun build-dependency-graph (modules)
  "Build a dependency graph from list of MODULES (source-module structs).
   Returns a dependency-graph struct.

   Dependencies are currently inferred from:
   1. Module load order (later modules depend on earlier ones)
   2. Explicit package dependencies (future enhancement)"
  (let ((path-index (make-hash-table :test 'equal))
        (dependents (make-hash-table :test 'equal))
        (dependencies (make-hash-table :test 'equal))
        (order nil))

    ;; Index modules by path
    (dolist (mod modules)
      (let ((path (source-module-relative-path mod)))
        (setf (gethash path path-index) mod)
        ;; Initialize empty lists
        (setf (gethash path dependents) nil)
        (setf (gethash path dependencies) nil)))

    ;; For now, use linear dependency (each module depends on all prior modules)
    ;; This is conservative but correct - more precise dependency tracking
    ;; would require analyzing require/load forms
    (let ((prev-paths nil))
      (dolist (mod modules)
        (let ((path (source-module-relative-path mod)))
          ;; This module depends on all previous modules
          (setf (gethash path dependencies) (copy-list prev-paths))
          ;; All previous modules have this as a dependent
          (dolist (prev prev-paths)
            (push path (gethash prev dependents)))
          ;; Add to order and prev-paths
          (push path order)
          (push path prev-paths))))

    ;; Order is reversed (we built it backwards)
    (setf order (nreverse order))

    (make-dependency-graph
     :modules modules
     :path-index path-index
     :order order
     :dependents dependents
     :dependencies dependencies)))

;;; ============================================================
;;; T041: get-dependents - Get transitive dependents
;;; ============================================================

(defun get-dependents (graph path)
  "Return all modules that depend on PATH (directly or transitively).
   Returns a list of paths sorted in compilation order."
  (let ((visited (make-hash-table :test 'equal))
        (result nil))

    ;; BFS to find all dependents
    (labels ((visit (p)
               (unless (gethash p visited)
                 (setf (gethash p visited) t)
                 (push p result)
                 (dolist (dep (gethash p (dependency-graph-dependents graph)))
                   (visit dep)))))
      ;; Start from path's direct dependents
      (dolist (dep (gethash path (dependency-graph-dependents graph)))
        (visit dep)))

    ;; Sort result by compilation order
    (let ((order-index (make-hash-table :test 'equal)))
      (loop for p in (dependency-graph-order graph)
            for i from 0
            do (setf (gethash p order-index) i))
      (sort result #'< :key (lambda (p) (gethash p order-index 0))))))

(defun get-compilation-order (graph paths)
  "Return PATHS sorted in compilation order according to GRAPH."
  (let ((order-index (make-hash-table :test 'equal)))
    (loop for p in (dependency-graph-order graph)
          for i from 0
          do (setf (gethash p order-index) i))
    (sort (copy-list paths) #'< :key (lambda (p) (gethash p order-index 0)))))

;;; ============================================================
;;; T042: detect-cycles - Detect circular dependencies
;;; ============================================================

(defun detect-cycles (graph)
  "Detect circular dependencies in GRAPH.
   Returns a list of cycle descriptions, or NIL if no cycles exist.

   Each cycle is a list of paths forming the cycle."
  (let ((visited (make-hash-table :test 'equal))
        (rec-stack (make-hash-table :test 'equal))
        (cycles nil))

    (labels ((dfs (path stack)
               (setf (gethash path visited) t)
               (setf (gethash path rec-stack) t)

               ;; Check all dependencies
               (dolist (dep (gethash path (dependency-graph-dependencies graph)))
                 (cond
                   ;; Not visited - recurse
                   ((not (gethash dep visited))
                    (dfs dep (cons path stack)))
                   ;; In current recursion stack - found cycle
                   ((gethash dep rec-stack)
                    (let ((cycle (cons dep (reverse (member dep stack)))))
                      (push cycle cycles)))))

               (setf (gethash path rec-stack) nil)))

      ;; Run DFS from each unvisited node
      (dolist (path (dependency-graph-order graph))
        (unless (gethash path visited)
          (dfs path nil))))

    cycles))
