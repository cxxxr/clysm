;;; A list made by LIST (the rest list of a small function with &REST copied
;;; where it is called, a list walked once) that is only measured and taken
;;; apart is not made: its elements are variables (SCALARIZE-LISTS in
;;; src/inline.lisp).  The values must be those with the list.

(defvar *trace* nil)
(defun note (x) (push x *trace*) x)

;; functions with &rest, small enough to be copied into their callers
(defun count-args (&rest xs) (length xs))
(defun first-arg (&rest xs) (car xs))
(defun second-or (default &rest xs) (if (cdr xs) (cadr xs) default))
(defun sum-args (&rest xs) (let ((s 0)) (dolist (x xs s) (incf s x))))
(defun apply-args (f &rest xs) (apply f xs))
(defun apply-more (f a &rest xs) (apply f a 10 xs))
(defun nth-arg (&rest xs) (list (nth 0 xs) (nth 2 xs) (nth 5 xs)))
(defun tests (&rest xs) (list (null xs) (consp xs) (atom xs) (endp xs) (not xs) (if xs :some :none)
                              (null (cdr xs)) (consp (cddr xs)) (length (cdr xs)) (length (cdddr xs))))
(defun keep-args (&rest xs) xs)                      ; the list itself: made
(defun last-arg (&rest xs) (car (last xs)))          ; given to a function: made
(defun set-first (&rest xs) (setf (car xs) :set) xs) ; changed: made
(defun closed (&rest xs) (lambda () (car xs)))       ; closed over: made
(defun walk-twice (&rest xs)
  (let ((n 0) (l xs))
    (loop (when (null l) (return)) (incf n (car l)) (setq l (cdr l)))
    (dolist (x xs) (incf n (* 10 x)))
    n))
(defun skip-one (&rest xs)
  (let ((r nil)) (dolist (x (cdr xs) (nreverse r)) (push x r))))
(defun early (&rest xs) (dolist (x xs :none) (when (evenp x) (return x))))
(defun nested (&rest xs) (list (count-args) (count-args (car xs)) (sum-args (car xs) (cadr xs) (length xs))))

(defun callers (a b c)
  (list (count-args) (count-args a) (count-args a b c) (count-args a b c a b c a b)
        (first-arg) (first-arg a) (first-arg b c)
        (second-or :d) (second-or :d a) (second-or :d a b) (second-or :d a b c)
        (sum-args) (sum-args a) (sum-args a b c) (sum-args a b c 4 5 6 7 8)
        (apply-args #'list) (apply-args #'list a b) (apply-args #'+ a b c)
        (apply-more #'list a) (apply-more #'list a b c)
        (nth-arg) (nth-arg a b c) (nth-arg a b c 4 5 6)
        (tests) (tests a) (tests a b) (tests a b c) (tests a b c 4)
        (keep-args) (keep-args a b) (last-arg a b c) (set-first a b)
        (funcall (closed a b))
        (walk-twice) (walk-twice a b c) (skip-one) (skip-one a) (skip-one a b c)
        (early) (early 1 3 5) (early a b c) (nested a b c)))
(print (callers 1 2 3))
;; more arguments than a copy takes, and calls through the function object
(print (list (count-args 1 2 3 4 5 6 7 8 9 10) (sum-args 1 2 3 4 5 6 7 8 9 10)
             (funcall #'count-args 1 2) (apply #'sum-args 1 2 '(3 4)) (apply #'first-arg nil)))

;; the arguments are evaluated once, in order, also those that are not used
(defun order ()
  (setq *trace* nil)
  (list (first-arg (note 1) (note 2) (note 3)) (count-args (note 4) (note 5)) (reverse *trace*)))
(print (order))

;; lists made by LIST in the function itself
(defun local-lists (a b)
  (let ((xs (list a b 3)))
    (list (+ (length xs) (car xs) (second xs) (if (null xs) 1 2) (nth 2 xs))
          (let ((s 0)) (dolist (x (list a b) s) (incf s x)))
          (let ((ys (list (note a) (note b)))) (apply #'- ys))
          (let ((zs (list a b))) (list (car zs) zs))
          (let ((ws (list a))) (declare (list ws)) (length ws))
          (let ((l (list a b a))) (do ((p l (cdr p)) (n 0 (+ n (car p)))) ((null p) n)))
          (let ((l (list a b))) (setq l (cdr l)) (setq l (cdr l)) (setq l (cdr l)) (list (car l) (length l)))
          (third (list a b)) (cadddr (list 1 2 3 4)) (first (rest (list a b))))))
(print (local-lists 5 7))

;; an element that is itself a list, NIL elements, a variable changed after the list was made
(defun elements (a)
  (let* ((xs (list a nil (list a a))) (y (car xs)))
    (setq a 100)
    (list y (car xs) (cadr xs) (null (cadr xs)) (length (caddr xs)) (consp (cdr xs))
          (let ((r nil)) (dolist (x xs (nreverse r)) (push x r))))))
(print (elements 1))

;; DOLIST with a result form that uses the variable, a body that returns, nested loops
(defun loops (a b)
  (let ((pairs nil))
    (dolist (x (list a b 3))
      (dolist (y (list x 10) (push :end pairs))
        (when (eql y 3) (return))
        (push (cons x y) pairs)))
    (list (nreverse pairs)
          (dolist (x (list a b) x))
          (block nil (dolist (x (list a b)) (when (eql x b) (return :found)))))))
(print (loops 1 2))

;; errors as with the list
(defun improper () (handler-case (sum-args 1 :x 2) (error () :error)))
(print (improper))

;; Advancing one cursor must not change another cursor or the list's root.
(defun cursors (a b c)
  (let* ((xs (list a b c)) (ys xs))
    (setq xs (cdr xs))
    (list (car xs) (car ys) (length xs) (length ys)
          (progn (setq ys (cddr ys)) (car ys)))))
(print (cursors 1 2 3))

;; An error must retain its datum if the compiler expands its type check.
(defun checked-list (a b)
  (let ((xs (list a b)))
    (handler-case (the fixnum xs)
      (type-error (e) (type-error-datum e)))))
(print (checked-list 1 2))

(defun indexed (n &rest xs) (nth n xs))
(print (loop for n in '(0 1 2 3 4 1000000)
             collect (indexed n :a nil :c)))
(defun index-order (a b c)
  (let ((xs (list a b c)))
    (list (nth (progn (setq xs (cdr xs)) 0) xs) (length xs))))
(print (index-order 1 2 3))
