;;;; debug-combined.lisp - Debug the combined compilation failure

(ql:quickload :clysm :silent t)

;; Don't run bootstrap.lisp (it starts the compilation immediately)
;; Instead, define the package and load just what we need

(defpackage :clysm/bootstrap-debug
  (:use :cl))

(in-package :clysm/bootstrap-debug)

(defparameter *compilation-order*
  '("src/clysm/backend/leb128.lisp"
    "src/clysm/backend/sections.lisp"
    "src/clysm/backend/wasm-emit.lisp"
    "src/clysm/backend/wat-print.lisp"
    "src/clysm/reader/tokenizer.lisp"
    "src/clysm/reader/parser.lisp"
    "src/clysm/reader/package.lisp"
    "src/clysm/reader/reader.lisp"
    "src/clysm/compiler/ast.lisp"
    "src/clysm/compiler/env.lisp"))

(defun read-source-file (path)
  (with-open-file (stream path :direction :input :external-format :utf-8)
    (let ((forms '()) (eof (gensym)))
      (loop for form = (read stream nil eof)
            until (eq form eof)
            do (push form forms))
      (nreverse forms))))

(defun compilable-form-p (form)
  (and (consp form)
       (symbolp (car form))
       (member (car form) '(defun defvar defparameter defconstant
                            progn let let* if when unless cond case)
               :test #'eq)))

(defun collect-compilable-forms ()
  "Collect ALL compilable forms from compiler source"
  (let ((base-dir (asdf:system-source-directory :clysm))
        (compile-fn #'clysm/compiler:compile-to-wasm)
        (compilable '()))
    (dolist (relative-path *compilation-order*)
      (let ((full-path (merge-pathnames relative-path base-dir)))
        (when (probe-file full-path)
          (format t "Reading ~A~%" relative-path)
          (handler-case
              (let ((forms (read-source-file full-path)))
                (dolist (form forms)
                  (when (compilable-form-p form)
                    (handler-case
                        (progn
                          (funcall compile-fn form)
                          (push form compilable))
                      (error () nil)))))
            (error () nil)))))
    (nreverse compilable)))

(defun debug-combined ()
  (format t "Collecting all individually compilable forms...~%")
  (let* ((forms (collect-compilable-forms))
         (compile-fn #'clysm/compiler:compile-to-wasm))
    (format t "Collected ~D forms~%" (length forms))

    ;; Print what kinds of forms we have
    (format t "~%Form types:~%")
    (let ((type-counts (make-hash-table :test 'eq)))
      (dolist (form forms)
        (incf (gethash (car form) type-counts 0)))
      (maphash (lambda (op count)
                 (format t "  ~A: ~D~%" op count))
               type-counts))

    ;; Try combined
    (format t "~%Testing combined compilation...~%")
    (handler-case
        (let ((bytes (funcall compile-fn `(progn ,@forms))))
          (format t "Combined OK! ~D bytes~%" (length bytes)))
      (error (e)
        (format t "Combined FAIL: ~A~%~%" e)
        ;; Bisect
        (bisect-forms forms compile-fn)))))

(defun bisect-forms (forms compile-fn)
  (let ((n (length forms)))
    (when (<= n 2)
      (format t "Cannot bisect further. Forms:~%")
      (dolist (form forms)
        (format t "  (~A ~A ...)~%" (car form)
                (if (eq (car form) 'defun) (cadr form) "")))
      (return-from bisect-forms))

    (let ((mid (floor n 2)))
      (let ((first-half (subseq forms 0 mid))
            (second-half (subseq forms mid)))

        (format t "Testing first half (~D forms)...~%" (length first-half))
        (handler-case
            (progn
              (funcall compile-fn `(progn ,@first-half))
              (format t "  First half OK~%"))
          (error (e)
            (format t "  First half FAILS: ~A~%" e)
            (bisect-forms first-half compile-fn)))

        (format t "Testing second half (~D forms)...~%" (length second-half))
        (handler-case
            (progn
              (funcall compile-fn `(progn ,@second-half))
              (format t "  Second half OK~%"))
          (error (e)
            (format t "  Second half FAILS: ~A~%" e)
            (bisect-forms second-half compile-fn)))))))

(debug-combined)
