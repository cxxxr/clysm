;;;; Baseline Capture Script for Contract Tests
;;;; Run with: sbcl --load tests/contract/instruction-collector/capture-baselines.lisp

(require :asdf)
(asdf:load-system :clysm)
(in-package :cl-user)

(defparameter *baseline-dir*
  (merge-pathnames "tests/contract/instruction-collector/baselines/"
                   (asdf:system-source-directory :clysm)))

(defun capture-and-save (name form)
  "Capture Wasm bytecode for FORM and save to NAME-baseline.wasm"
  (let* ((wasm-bytes (clysm:compile-to-wasm form))
         (path (merge-pathnames (format nil "~A-baseline.wasm" name) *baseline-dir*)))
    (ensure-directories-exist path)
    (with-open-file (out path
                         :direction :output
                         :element-type '(unsigned-byte 8)
                         :if-exists :supersede)
      (write-sequence wasm-bytes out))
    (format t "Captured ~A baseline: ~A bytes -> ~A~%" name (length wasm-bytes) path)
    (length wasm-bytes)))

;; T008: Capture equalp baseline
(capture-and-save "equalp" '(defun test-equalp (x y) (equalp x y)))

;; T009: Capture equal baseline
(capture-and-save "equal" '(defun test-equal (x y) (equal x y)))

;; Additional baselines for complex equalp form
(capture-and-save "complex-equalp"
                  '(defun test-complex-equalp (a b c d)
                     (and (equalp a b)
                          (equalp c d)
                          (equalp (cons a b) (cons c d)))))

(format t "~%All baselines captured successfully.~%")
(sb-ext:exit :code 0)
