;;; A property, a method or a global named by a literal string: the name is
;;; made JavaScript's once where it is written, and the call does what it did.
;;; (Run on the Node.js host; the expected output is in js-literal-names.expected.)
(defun kind-of (object) (js:get object "kind"))
(defun rename (object name) (js:set object "name" name) object)
(defun text-of (object) (js:call object "toUpperCase"))

(let ((a (js:object "kind" 1 "name" "a")) (b (js:object "kind" 2 "name" "b")))
  ;; the same place asked twice, of two objects
  (print (list (kind-of a) (kind-of b) (kind-of a)))
  (print (js:get (rename a "renamed") "name"))
  ;; a name that is no literal is a name as before
  (print (mapcar (lambda (key) (js:get b key)) '("kind" "name" "none")))
  (let ((key "kind")) (js:set b key 3) (print (js:get b key)))
  ;; a method and a global, in a loop
  (print (loop repeat 3 collect (text-of "abc")))
  (print (loop for i below 3 collect (js:call (js:global "Math") "max" i 1)))
  (print (js:call (js:global "JSON") "stringify" (js:object "é" 1 "a b" 2)))
  ;; a name of other letters, and one that is no identifier
  (let ((o (js:object)))
    (js:set o "日本語" :x)
    (js:set o "a-b.c" 7)
    (print (list (js:get o "日本語") (js:get o "a-b.c") (js:get o "length")))))
;; in a function made at run time
(print (funcall (compile nil '(lambda (o) (js:get o "kind"))) (js:object "kind" :compiled)))
;; a name compiled again and again in the image is one cell, not one more
;; to empty for each form (SYS::*JS-FILLED-NAMES*)
(let ((o (js:object "kind" :again)) (before (length sys::*js-filled-names*)))
  (dotimes (i 5) (funcall (compile nil '(lambda (o) (js:get o "kindAgain"))) o))
  (print (<= (- (length sys::*js-filled-names*) before) 1)))
;; one cell for a name in a module compiled from files, wherever it is
;; written (JS-NAME-CELL-GLOBAL in src/codegen.lisp): a name that the
;; runtime writes too (%JS-SUSPENDING-P, runtime/js.lisp), made
;; JavaScript's there, is made so here too.  (Code compiled in the image
;; takes its cells from *JS-NAME-CELLS*, its own.)
(setq sys::*js-suspending* :unknown)
(sys::%js-suspending-p)
(let ((before (length sys::*js-filled-names*)))
  (js:get (js:global "WebAssembly") "promising")
  (print (or (= (length sys::*js-filled-names*) before)
             (and sys::*js-name-cells* (nth-value 1 (gethash "promising" sys::*js-name-cells*)) t))))
;; a LOAD-TIME-VALUE of a quoted cons that is no name is that constant, one
;; object, whose changes stay
(defun user-cell () (load-time-value '(user . cell)))
(setf (cdr (user-cell)) 'changed)
(print (list (eq (user-cell) (user-cell)) (cdr (user-cell))))
