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
