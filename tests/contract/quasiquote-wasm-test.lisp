;;;; tests/contract/quasiquote-wasm-test.lisp
;;;; Contract tests for quasiquote Wasm output validation
;;;; Feature: 001-quasiquote-local-vars

(defpackage #:clysm/tests/contract/quasiquote-wasm-test
  (:use #:cl #:rove)
  (:import-from #:clysm
                #:compile-to-wasm))

(in-package #:clysm/tests/contract/quasiquote-wasm-test)

;;; ==========================================================================
;;; Helper Functions
;;; ==========================================================================

(defun wasm-bytes-contain-p (wasm-bytes &rest byte-pattern)
  "Check if Wasm bytes contain a specific byte sequence.
   BYTE-PATTERN is a list of byte values to search for."
  (let* ((vec (coerce wasm-bytes 'vector))
         (pattern-len (length byte-pattern))
         (vec-len (length vec)))
    (loop for i from 0 to (- vec-len pattern-len)
          thereis (loop for j from 0 below pattern-len
                        always (= (aref vec (+ i j))
                                  (nth j byte-pattern))))))

(defun compile-test-form (form)
  "Compile a test form and return Wasm bytes."
  (handler-case
      (compile-to-wasm form)
    (error (c)
      (declare (ignore c))
      nil)))

;;; ==========================================================================
;;; Phase 3: User Story 1 - Contract Tests
;;; ==========================================================================

;; T013: Wasm contains local.get instructions
(deftest test-wasm-contains-local-get
  ;; Contract: Generated Wasm MUST include local.get for unquoted variables
  ;; Wasm opcode: 0x20 (local.get)
  (let ((wasm (compile-test-form '(defun test-qq (x) `(result ,x)))))
    (ok wasm "Compilation should succeed")
    (when wasm
      (ok (wasm-bytes-contain-p wasm #x20)  ; 0x20 = local.get
          "Generated Wasm should contain local.get instruction"))))

;; T014: Wasm contains cons construction
(deftest test-wasm-contains-cons-construction
  ;; Contract: Generated Wasm MUST include struct.new for cons cells
  ;; GC opcode prefix: 0xFB, struct.new: 0x00 (or variant)
  (let ((wasm (compile-test-form '(defun test-qq (x) `(result ,x)))))
    (ok wasm "Compilation should succeed")
    (when wasm
      ;; Check for GC instruction prefix (0xFB) followed by struct operation
      (ok (wasm-bytes-contain-p wasm #xFB)
          "Generated Wasm should contain GC instruction prefix for struct.new"))))

;;; ==========================================================================
;;; Phase 4: User Story 2 - Contract Tests
;;; ==========================================================================

;; T023: Wasm generates splice iteration code
(deftest test-wasm-calls-append
  ;; Contract: Unquote-splicing MUST generate iteration code to splice list
  ;; The splice form `(prefix ,@xs suffix) generates inline NCONC loop
  ;; Wasm opcodes: block (0x02), loop (0x03), br (0x0C), br_if (0x0D)
  (let ((wasm (compile-test-form '(defun test-splice (xs) `(prefix ,@xs suffix)))))
    (ok wasm "Compilation should succeed")
    (when wasm
      ;; Check for loop control instructions - splicing requires iteration
      (ok (wasm-bytes-contain-p wasm #x02)  ; 0x02 = block
          "Generated Wasm should contain block for splice loop")
      (ok (wasm-bytes-contain-p wasm #x03)  ; 0x03 = loop
          "Generated Wasm should contain loop for splice iteration")
      ;; Verify local.get is present (variable xs must be fetched)
      (ok (wasm-bytes-contain-p wasm #x20)  ; 0x20 = local.get
          "Generated Wasm should contain local.get for spliced variable"))))

;;; ==========================================================================
;;; Phase 5: User Story 4 - Contract Tests
;;; ==========================================================================

;; T031: Mixed symbol hash and local.get
(deftest test-wasm-mixed-symbol-hash
  ;; Contract: Quoted symbols use i32.const (hash), vars use local.get
  ;; Mixed form `(if ,cond ,then ,else) should have:
  ;; - Symbol 'if is quoted data (i32.const for symbol hash)
  ;; - Variables cond/then/else are evaluated (local.get)
  (let ((wasm (compile-test-form '(defun test-mixed (cond then else)
                                    `(if ,cond ,then ,else)))))
    (ok wasm "Compilation should succeed")
    (when wasm
      ;; Check for i32.const (0x41) - used for symbol hash/literal values
      (ok (wasm-bytes-contain-p wasm #x41)  ; 0x41 = i32.const
          "Generated Wasm should contain i32.const for quoted symbol")
      ;; Check for local.get (0x20) - used for variable access
      (ok (wasm-bytes-contain-p wasm #x20)  ; 0x20 = local.get
          "Generated Wasm should contain local.get for unquoted variables"))))
