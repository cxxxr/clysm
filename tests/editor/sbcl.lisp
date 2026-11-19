;;;; Run the tests of the editor's model on SBCL: sbcl --script tests/editor/sbcl.lisp
(let ((dir (merge-pathnames "../../ui/editor/" (directory-namestring *load-truename*))))
  (dolist (f '("buffer" "syntax" "commands"))
    (load (merge-pathnames (concatenate 'string f ".lisp") dir))))
(load (merge-pathnames "model.lisp" (directory-namestring *load-truename*)))
(sb-ext:exit :code (if (zerop (symbol-value (find-symbol "*FAILURES*" "EDITOR-TEST"))) 0 1))
