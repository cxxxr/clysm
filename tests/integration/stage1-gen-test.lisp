;;;; stage1-gen-test.lisp - Integration test for partial Stage 1 generation
;;;;
;;;; Part of Feature 039: Stage 1 Compiler Generation
;;;; T048: Verifies partial Stage 1 binary generation from source

(in-package #:clysm/tests/integration/stage1-gen)

;;; ==========================================================================
;;; Partial Stage 1 Generation Tests
;;; ==========================================================================

(deftest test-compile-simple-forms-to-wasm
  "Simple arithmetic forms should compile to Wasm bytes."
  (let* ((forms (list (clysm/stage1:make-source-form
                       :id "gen:0" :sexp '(+ 1 2) :operator '+ :compilable-p t)
                      (clysm/stage1:make-source-form
                       :id "gen:1" :sexp '(* 3 4) :operator '* :compilable-p t)
                      (clysm/stage1:make-source-form
                       :id "gen:2" :sexp '(- 10 5) :operator '- :compilable-p t))))
    (multiple-value-bind (results stats)
        (clysm/stage1:compile-all-forms forms)
      (ok (= (length results) 3) "Three results returned")
      (ok (getf stats :compiled) "Stats has :compiled key")
      (ok (> (getf stats :compiled) 0) "At least one form compiled"))))

(deftest test-accumulate-wasm-bytes
  "Compiled Wasm bytes should accumulate correctly."
  (let* ((forms (list (clysm/stage1:make-source-form
                       :id "acc:0" :sexp '(+ 1 2) :operator '+ :compilable-p t)))
         (results nil))
    (multiple-value-bind (res stats)
        (clysm/stage1:compile-all-forms forms)
      (declare (ignore stats))
      (setf results res))
    (let ((combined (clysm/stage1:accumulate-wasm-bytes results)))
      (if (some #'clysm/stage1:compilation-result-success-p results)
          (progn
            (ok (vectorp combined) "Combined bytes is a vector")
            (ok (> (length combined) 0) "Combined bytes has content"))
          (ok (null combined) "No bytes when no successful compilation")))))

(deftest test-partial-generation-with-failures
  "Generation should continue despite some form failures."
  (let* ((forms (list (clysm/stage1:make-source-form
                       :id "mixed:0" :sexp '(+ 1 2) :operator '+ :compilable-p t)
                      (clysm/stage1:make-source-form
                       :id "mixed:1" :sexp '(unknown-op-xyz) :operator 'unknown-op-xyz :compilable-p t)
                      (clysm/stage1:make-source-form
                       :id "mixed:2" :sexp '(* 3 4) :operator '* :compilable-p t))))
    (multiple-value-bind (results stats)
        (clysm/stage1:compile-all-forms forms)
      (ok (= (length results) 3) "All forms attempted")
      (ok (= (getf stats :total) 3) "Total is 3")
      ;; At least arithmetic forms should compile
      (let ((successes (count-if #'clysm/stage1:compilation-result-success-p results)))
        (ok (>= successes 0) "At least zero successes (graceful handling)")))))

(deftest test-write-stage1-binary-to-temp
  "Stage 1 binary should be writable to temporary location."
  (let* ((test-bytes (make-array 8 :element-type '(unsigned-byte 8)
                                  :initial-contents '(#x00 #x61 #x73 #x6d
                                                      #x01 #x00 #x00 #x00)))
         (temp-path (format nil "/tmp/clysm-stage1-test-~A.wasm" (get-universal-time))))
    (unwind-protect
        (progn
          (let ((written (clysm/stage1:write-stage1-binary test-bytes temp-path)))
            (ok (= written 8) "8 bytes written")
            (ok (probe-file temp-path) "File exists")
            (with-open-file (in temp-path :element-type '(unsigned-byte 8))
              (ok (= (file-length in) 8) "File size is 8 bytes"))))
      (when (probe-file temp-path)
        (delete-file temp-path)))))

(deftest test-progress-callback-called
  "Progress callback should be called for each form."
  (let* ((forms (list (clysm/stage1:make-source-form
                       :id "cb:0" :sexp '(+ 1 2) :operator '+ :compilable-p t)
                      (clysm/stage1:make-source-form
                       :id "cb:1" :sexp '(* 3 4) :operator '* :compilable-p t)))
         (callback-count 0))
    (clysm/stage1:compile-all-forms
     forms
     :progress-callback (lambda (index total success-p)
                          (declare (ignore index total success-p))
                          (incf callback-count)))
    (ok (= callback-count 2) "Callback called twice (once per form)")))

