;;; A program of libraries from Quicklisp built into one module
;;; (tests/static/run.sh, when ~/quicklisp is there): CL-PPCRE, whose
;;; scanners call generic functions over the parse tree of a regular
;;; expression, and RT, which keeps its tests after the first cons of a
;;; quoted list, (defvar *entries* '(nil)): the list is changed where the
;;; program runs, and nothing else of the program is (once the last cons of
;;; the record of a generic function was that cons, and CL-PPCRE:SCAN died).
(defpackage :static-libs (:use :cl))
(in-package :static-libs)

(rt:deftest scan.1 (multiple-value-list (cl-ppcre:scan "(a+)b" "xaab")) (1 4 #(1) #(3)))
(rt:deftest replace.1 (values (cl-ppcre:regex-replace-all "[0-9]+" "a1b22c333" "#")) "a#b#c#")
(rt:deftest split.1 (cl-ppcre:split "\\s*,\\s*" "x , y,z") ("x" "y" "z"))
(rt:deftest literal.1 (list '(nil) '(1 nil)) ((nil) (1 nil)))

(defun main ()
  (let ((count 0))
    ;; (often enough for the dispatch of the generic functions to be checked)
    (dotimes (i 50)
      (when (cl-ppcre:scan (if (evenp i) "^(\\w+)@(\\w+)\\.com$" "b[aeiou]+d|x{2,3}") "someone@example.com")
        (incf count)))
    (format t "matches ~D~%" count))
  (let ((*standard-output* (make-broadcast-stream)))
    (rt:do-tests))
  (format t "pending ~S~%" (rt:pending-tests)))
