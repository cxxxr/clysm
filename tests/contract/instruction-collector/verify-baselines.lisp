;;;; Baseline Verification Script for Contract Tests
;;;; Run with: sbcl --load tests/contract/instruction-collector/verify-baselines.lisp

(require :asdf)
(asdf:load-system :clysm)
(in-package :cl-user)

(defparameter *baseline-dir*
  (merge-pathnames "tests/contract/instruction-collector/baselines/"
                   (asdf:system-source-directory :clysm)))

(defun load-baseline (name)
  "Load baseline Wasm bytes from NAME-baseline.wasm"
  (let ((path (merge-pathnames (format nil "~A-baseline.wasm" name) *baseline-dir*)))
    (with-open-file (in path :element-type '(unsigned-byte 8))
      (let* ((size (file-length in))
             (bytes (make-array size :element-type '(unsigned-byte 8))))
        (read-sequence bytes in)
        bytes))))

(defun compare-bytes (baseline current)
  "Compare two byte arrays, return (values match-p diff-report)"
  (if (not (= (length baseline) (length current)))
      (values nil (format nil "Length mismatch: baseline=~A current=~A"
                          (length baseline) (length current)))
      (loop for i from 0 below (length baseline)
            for b = (aref baseline i)
            for c = (aref current i)
            when (/= b c)
              do (return (values nil (format nil "Byte mismatch at offset ~A: baseline=~2,'0X current=~2,'0X"
                                             i b c)))
            finally (return (values t "Byte-identical")))))

(defun verify-baseline (name form)
  "Verify current compilation matches baseline"
  (let* ((baseline (load-baseline name))
         (current (clysm:compile-to-wasm form)))
    (multiple-value-bind (match-p report)
        (compare-bytes baseline current)
      (format t "~A: ~A~%" name (if match-p "PASS" (format nil "FAIL - ~A" report)))
      match-p)))

;; Run verification
(format t "~%Contract Test: Bytecode Verification~%")
(format t "=====================================~%~%")

(let ((all-pass t))
  ;; Verify equalp baseline
  (unless (verify-baseline "equalp" '(defun test-equalp (x y) (equalp x y)))
    (setf all-pass nil))

  ;; Verify equal baseline
  (unless (verify-baseline "equal" '(defun test-equal (x y) (equal x y)))
    (setf all-pass nil))

  ;; Verify complex-equalp baseline
  (unless (verify-baseline "complex-equalp"
                           '(defun test-complex-equalp (a b c d)
                              (and (equalp a b)
                                   (equalp c d)
                                   (equalp (cons a b) (cons c d)))))
    (setf all-pass nil))

  (format t "~%Overall: ~A~%" (if all-pass "ALL PASS" "SOME FAILED"))
  (sb-ext:exit :code (if all-pass 0 1)))
