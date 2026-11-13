;;;; Every name that the table "What of CAPI and CLIM is here" of the README
;;;; gives for this framework is an exported symbol of the UI package (or a
;;;; name of Common Lisp) that names a class, a function, a macro or a variable.  tools/test-ui.sh takes
;;;; the names from the README (*README-NAMES*) and compares the output with
;;;; names.expected.

(in-package "CL-USER")

(let ((missing '()) (checked 0))
  (dolist (name *readme-names*)
    (multiple-value-bind (symbol status) (find-symbol (string-upcase name) "UI")
      (incf checked)
      (unless (and (or (eq status :external)
                       ;; a name of Common Lisp, like FORMAT
                       (and symbol (eq (symbol-package symbol) (find-package "COMMON-LISP"))))
                   (or (fboundp symbol) (macro-function symbol) (find-class symbol nil) (boundp symbol)))
        (push name missing))))
  (format t "names of the README that are not in the UI package: ~S~%" (reverse missing))
  (format t "~:[no names were found in the README~;names were checked~]~%" (> checked 40)))
