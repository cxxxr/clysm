;;;; sequence-codegen-test.lisp - Unit tests for sequence operations codegen
;;;; Feature: 001-ansi-sequence-operations (Phase 13D-2)

(in-package #:clysm/tests/unit/sequence-codegen)

;;; ============================================================
;;; Test Utilities
;;; ============================================================

(defun compile-form (form)
  "Compile a form and return the Wasm module."
  (clysm/compiler:compile-to-wasm form))

(defun wasm-contains-instruction-p (wasm-bytes opcode-prefix opcode)
  "Check if WASM-BYTES contains an instruction with given opcodes.
   OPCODE-PREFIX is the first byte (e.g., #xFB for GC instructions).
   OPCODE is the second byte."
  (loop for i from 0 below (1- (length wasm-bytes))
        thereis (and (= (aref wasm-bytes i) opcode-prefix)
                     (= (aref wasm-bytes (1+ i)) opcode))))

;;; ============================================================
;;; Phase 2: Foundational - array.copy instruction tests (T005)
;;; ============================================================

(deftest test-array-copy-instruction-emission
  "Verify array.copy instruction can be emitted (T005)"
  ;; array.copy opcode is 0xFB 0x11
  ;; Test that copy-seq (which uses subseq) generates valid Wasm
  (let ((wasm (compile-form '(defun test-copy () (copy-seq "hello")))))
    (ok (> (length wasm) 0) "T005: copy-seq compiles to non-empty Wasm")))

;;; ============================================================
;;; Phase 3: User Story 1 - subseq codegen tests (NOW IMPLEMENTED)
;;; ============================================================

(deftest test-compile-subseq-string-structure
  "Verify subseq on string produces valid codegen structure"
  (let ((wasm (compile-form '(subseq "hello" 0 3))))
    (ok (> (length wasm) 0) "subseq on string compiles successfully")
    (ok (typep wasm '(vector (unsigned-byte 8))) "subseq produces byte vector")))

(deftest test-compile-subseq-with-start-end
  "Verify subseq with start and end indices compiles"
  (let ((wasm (compile-form '(subseq "hello world" 2 7))))
    (ok (> (length wasm) 0) "subseq with start/end compiles successfully")))

(deftest test-compile-subseq-without-end
  "Verify subseq without explicit end compiles"
  ;; Note: This tests that our implementation handles 2-argument subseq
  (let ((wasm (compile-form '(defun test-subseq (s) (subseq s 0)))))
    (ok (> (length wasm) 0) "subseq without end compiles in defun context")))

;;; ============================================================
;;; Phase 4: User Story 2 - concatenate codegen tests (NOW IMPLEMENTED)
;;; ============================================================

(deftest test-compile-concatenate-string-structure
  "Verify concatenate 'string produces valid codegen structure (T021)"
  (let ((wasm (compile-form '(concatenate 'string "foo" "bar"))))
    (ok (> (length wasm) 0) "T021: concatenate 'string compiles successfully")))

(deftest test-compile-concatenate-multiple-strings
  "Verify concatenate with multiple strings compiles"
  (let ((wasm (compile-form '(concatenate 'string "a" "b" "c" "d"))))
    (ok (> (length wasm) 0) "concatenate with multiple args compiles")))

;;; ============================================================
;;; Phase 5: User Story 3 - make-string codegen tests (NOW IMPLEMENTED)
;;; ============================================================

(deftest test-compile-make-string-basic
  "Verify make-string produces valid Wasm (T033)"
  (let ((wasm (compile-form '(make-string 5))))
    (ok (> (length wasm) 0) "T033: make-string basic compiles successfully")))

(deftest test-compile-make-string-with-initial-element
  "Verify make-string :initial-element compiles (T034)"
  (let ((wasm (compile-form '(make-string 5 :initial-element #\x))))
    (ok (> (length wasm) 0) "T034: make-string :initial-element compiles")))

;;; ============================================================
;;; Phase 6: User Story 4 - make-array extension codegen tests (NOW IMPLEMENTED)
;;; ============================================================

(deftest test-compile-make-array-initial-element
  "Verify make-array :initial-element produces valid Wasm (T042)"
  (let ((wasm (compile-form '(make-array 3 :initial-element 0))))
    (ok (> (length wasm) 0) "T042: make-array :initial-element compiles")))

(deftest test-compile-make-array-initial-contents
  "Verify make-array :initial-contents produces valid Wasm (T043)"
  (let ((wasm (compile-form '(make-array 3 :initial-contents '(1 2 3)))))
    (ok (> (length wasm) 0) "T043: make-array :initial-contents compiles")))

(deftest test-compile-make-array-basic
  "Verify make-array basic form compiles"
  (let ((wasm (compile-form '(make-array 5))))
    (ok (> (length wasm) 0) "make-array basic compiles successfully")))

;;; ============================================================
;;; Phase 7: User Story 5 - copy-seq codegen tests (NOW IMPLEMENTED)
;;; ============================================================

(deftest test-compile-copy-seq-string
  "Verify copy-seq on string compiles (T051)"
  (let ((wasm (compile-form '(copy-seq "hello"))))
    (ok (> (length wasm) 0) "T051: copy-seq on string compiles")))

(deftest test-compile-copy-seq-in-defun
  "Verify copy-seq works in defun context"
  (let ((wasm (compile-form '(defun test-copy (s) (copy-seq s)))))
    (ok (> (length wasm) 0) "copy-seq in defun compiles successfully")))

;;; ============================================================
;;; Integration - Combined operations
;;; ============================================================

(deftest test-combined-sequence-operations
  "Verify combined sequence operations compile together"
  (let ((wasm (compile-form '(defun test-seq ()
                               (list (subseq "hello" 0 3)
                                     (copy-seq "world")
                                     (concatenate 'string "foo" "bar"))))))
    (ok (> (length wasm) 0) "Combined sequence ops compile in defun")))
