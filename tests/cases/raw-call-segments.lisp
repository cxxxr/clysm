;;; A function compiled in an earlier segment of a file's module (its main
;;; function had an integer accumulator: the parameter N) and called later
;;; with a fixnum, in a copy that takes N raw (reduced from Lem's
;;; CHECK-READ-ONLY-AT-POINT): the copy's N is its own.
(defun write-forms (path forms)
  (with-open-file (s path :direction :output :if-exists :supersede)
    (with-standard-io-syntax
      (dolist (f forms) (prin1 f s) (terpri s)))))

(write-forms "build/raw-seg-test.lisp"
  '((defstruct rs-line next (len 0))
    (defstruct rs-point line (charpos 0))
    (defun rs-search (line a b) (declare (ignore line)) (and (> a 1000) (> b 1000)))
    (defun rs-check (point n)
      (loop :for l := (rs-point-line point) :then (rs-line-next l)
            :for cp := (rs-point-charpos point) :then 0
            :do (unless l (return))
                (when (rs-search l cp (+ cp n)) (error "read only"))
                (when (>= 0 (decf n (1+ (- (rs-line-len l) cp))))
                  (return))))
    ;; (evaluated at compile time too: the segment of the module ends here)
    (eval-when (:compile-toplevel :load-toplevel :execute) (defvar *rs-var* 1))
    (defun rs-caller (point) (list (rs-check point 3) (rs-check point 30) :ok))))

(load (compile-file "build/raw-seg-test.lisp"))
(print (rs-caller (make-rs-point :line (make-rs-line :len 5 :next (make-rs-line :len 7)))))
