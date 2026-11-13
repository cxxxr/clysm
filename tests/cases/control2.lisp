;;; non-local exits out of local functions: return-from, go, throw; cleanups,
;;; bindings, values, recursion

(defvar *log* nil)
(defvar *sp* :global)
(defun note (x) (push x *log*) x)
(defun call (f &rest args) (apply f args))
(defmacro logged (&body body) `(progn (setq *log* nil) (let ((r (multiple-value-list (progn ,@body)))) (list r (reverse *log*)))))

;; return-from a function out of a recursive local function
(defun tree-find (tree pred)
  (labels ((walk (x)
             (cond ((consp x) (walk (car x)) (walk (cdr x)))
                   ((and x (funcall pred x)) (return-from tree-find (values x :found))))))
    (walk tree)
    (values nil :not-found)))
(print (multiple-value-list (tree-find '((1 2) (3 (4 5)) 6) (lambda (x) (> x 3)))))
(print (multiple-value-list (tree-find '((1 2) (3 (4 5)) 6) (lambda (x) (> x 9)))))
(print (multiple-value-list (tree-find nil #'identity)))

;; the same through two levels of local functions and mutual recursion
(defun parity-search (n target)
  (block found
    (labels ((ev (k) (when (= k target) (return-from found (list :even k))) (if (= k 0) :none (od (1- k))))
             (od (k) (when (= k target) (return-from found (list :odd k))) (if (= k 0) :none (ev (1- k)))))
      (flet ((start (k) (list :result (ev k))))
        (start n)))))
(print (list (parity-search 10 7) (parity-search 10 4) (parity-search 10 20)))

;; tail calls in such functions stay tail calls
(defun count-down (n)
  (labels ((down (k acc) (cond ((< k 0) (return-from count-down :negative))
                               ((= k 0) acc)
                               (t (down (1- k) (1+ acc))))))
    (down n 0)))
(print (list (count-down 1000000) (count-down -1)))

;; each activation has its own block
(defun nest (n)
  (block b
    (flet ((out (v) (return-from b v)))
      (if (= n 0)
          (out :bottom)
          (list n (nest (1- n)) (out (list :level n)) :not-reached)))))
(print (nest 3))
(defun nest2 (n k)
  (block b
    (labels ((out (v) (return-from b v))
             (rec (i) (if (= i 0) (funcall k :deep) (list (rec (1- i))))))
      (if (= n 0)
          (out :bottom)
          (list n (nest2 (1- n) #'out) (rec 2))))))
(print (nest2 2 (lambda (v) v)))

;; unwind-protect in the local function, in an intermediate one and in the owner
(defun uwp-chain (x)
  (block b
    (labels ((inner (v) (unwind-protect (if (> v 0) (return-from b (note (list :exit v))) :no) (note :inner-cleanup)))
             (middle (v) (unwind-protect (list :middle (inner v)) (note :middle-cleanup))))
      (unwind-protect (list :normal (middle x)) (note :owner-cleanup)))))
(print (logged (uwp-chain 1)))
(print (logged (uwp-chain 0)))

;; special bindings are undone on the way
(defun sp-chain (x)
  (block b
    (labels ((inner (v) (let ((*sp* :inner)) (when v (return-from b (list :exit *sp*))) *sp*))
             (middle (v) (let ((*sp* :middle)) (list (inner v) *sp*))))
      (let ((*sp* :owner))
        (list (middle x) *sp*)))))
(print (list (sp-chain t) *sp* (sp-chain nil) *sp*))
(defun sp-param (x)
  (flet ((inner (*sp* v) (when v (return-from sp-param (list :exit *sp*))) *sp*))
    (list (inner :param x) *sp*)))
(print (list (sp-param t) *sp* (sp-param nil) *sp*))

;; a cleanup that itself calls a function that exits non-locally, while an exit is under way
(defun cleanup-exit (x)
  (block outer
    (labels ((leave (v) (return-from outer v))
             (helper () (block inner (flet ((leave-inner () (return-from inner :helper-done))) (leave-inner) :not-reached)))
             (work () (unwind-protect (leave (note :leaving)) (note (helper)))))
      (list :normal (if x (work) :no-work)))))
(print (logged (cleanup-exit t)))
(print (logged (cleanup-exit nil)))
;; a cleanup that replaces the exit under way by another one
(defun cleanup-replace ()
  (block outer
    (list :outer
          (block middle
            (flet ((to-outer (v) (return-from outer v))
                   (to-middle (v) (return-from middle v)))
              (unwind-protect (to-outer :first) (to-middle :second)))))))
(print (cleanup-replace))

;; multiple values through the exit, and no values
(defun mv-exit (k)
  (flet ((out () (case k (0 (return-from mv-exit (values))) (1 (return-from mv-exit (values 1 2 3))) (t :stay))))
    (unwind-protect (out) (note :c))
    (values :a :b)))
(print (list (multiple-value-list (mv-exit 0)) (multiple-value-list (mv-exit 1)) (multiple-value-list (mv-exit 2))))

;; go out of local functions
(defun go-test (n)
  (let ((steps nil))
    (tagbody
       (labels ((jump (k) (case k (1 (go one)) (2 (go two)) (t (go end))))
                (indirect (k) (unwind-protect (jump k) (push (list :cleanup k) steps))))
         (push :start steps)
         (indirect n))
     one
       (push :one steps)
       (flet ((again () (go two))) (again))
     two
       (push :two steps)
     end
       (push :end steps))
    (reverse steps)))
(print (list (go-test 1) (go-test 2) (go-test 3)))
(defun go-loop (n)
  (let ((i 0) (acc nil))
    (tagbody
     top
       (flet ((next () (incf i) (when (< i n) (go top))))
         (push i acc)
         (next)))
    (reverse acc)))
(print (go-loop 5))
(print (let ((x 0)) (tagbody a (incf x) (labels ((f (d) (if (= d 0) (when (< x 3) (go a)) (f (1- d))))) (f 5))) x))

;; throw out of local functions: catch in the same function, in the caller, a computed tag
(defun throw-local (tag x)
  (list :caught
        (catch tag
          (labels ((walk (l) (cond ((null l) :end)
                                   ((eql (car l) x) (throw tag (list :found (car l))))
                                   (t (walk (cdr l))))))
            (list :walked (walk '(1 2 3 4)))))))
(print (list (throw-local 'a 3) (throw-local 'a 9) (throw-local (list 1) 2)))
(defun throw-outer (x)
  (flet ((check (v) (when (eql v x) (throw 'outer-tag (list :thrown v))) v))
    (list (check 1) (check 2) (check 3))))
(print (list (catch 'outer-tag (throw-outer 2)) (catch 'outer-tag (throw-outer 5))))
(print (catch 'outer-tag (list :wrapper (catch 'other (throw-outer 3)))))
;; the innermost catch of the tag takes it, also when it is in the local function itself
(defun throw-nested (depth)
  (catch 'k
    (labels ((rec (d)
               (if (= d 0)
                   (throw 'k :bottom)
                   (list d (catch 'k (rec (1- d)))))))
      (list :top (rec depth)))))
(print (throw-nested 3))
(defun throw-dynamic (tag1 tag2 which)
  (catch tag1
    (list :outer
          (catch tag2
            (flet ((th (tag) (throw tag (list :to which))))
              (unwind-protect (th (if (eq which 1) tag1 tag2)) (note :uwp))
              :not-reached)))))
(print (logged (throw-dynamic 'a 'b 1)))
(print (logged (throw-dynamic 'a 'b 2)))
(print (logged (throw-dynamic 'a 'a 1)))
;; a throw to a tag that does not exist is an error at the throw
(defun throw-missing ()
  (flet ((th () (throw 'no-such-tag 1)))
    (handler-case (unwind-protect (th) (note :cleanup-after-handler))
      (control-error () (note :control-error)))))
(print (logged (throw-missing)))
;; a throw to a constant tag through an unwind-protect of the same function
(defun throw-uwp (x)
  (catch 'here
    (unwind-protect (progn (when x (throw 'here (values :thrown 2))) :normal)
      (note :cleanup))))
(print (logged (throw-uwp t)))
(print (logged (throw-uwp nil)))

;; local functions that are also used as values are called from unknown code
(defun as-value (list)
  (block b
    (flet ((check (x) (when (minusp x) (return-from b (list :negative x))) x))
      (list (check 1) (mapcar #'check list)))))
(print (list (as-value '(1 2 3)) (as-value '(1 -2 3))))
;; a function that only calls can be called from a closure that escapes
(defun from-closure (list)
  (block b
    (flet ((check (x) (when (minusp x) (return-from b (list :negative x))) x))
      (list (check 1) (mapcar (lambda (x) (check (* 2 x))) list)))))
(print (list (from-closure '(1 2 3)) (from-closure '(1 -2 3))))
(defun from-closure-uwp (list)
  (block b
    (labels ((check (x) (when (minusp x) (return-from b (list :negative x))) x)
             (twice (x) (unwind-protect (check (* 2 x)) (note x))))
      (call (lambda () (mapcar (lambda (x) (twice x)) list))))))
(print (logged (from-closure-uwp '(1 2 3))))
(print (logged (from-closure-uwp '(1 -2 3))))
;; the block has been left: the exit cannot be completed
(defun escaped () (block b (flet ((out () (return-from b 1))) (lambda () (out)))))
(print (functionp (escaped)))

;; local functions with &optional, &key and &rest parameters
(defun lambda-lists (k)
  (block b
    (flet ((opt (a &optional (o (when (eql k 1) (return-from b :from-default)))) (when (eql k 2) (return-from b (list :opt a o))) a)
           (key (&key (x 1) (y (if (eql k 3) (return-from b :key-default) 2))) (when (eql k 4) (return-from b (list :key x y))) (+ x y))
           (rst (&rest r) (when (eql k 5) (return-from b (list :rest r))) (length r)))
      (list (opt 1) (opt 1 2) (key) (key :x 5) (rst 1 2 3)))))
(print (loop for k from 0 to 5 collect (lambda-lists k)))

;; exits in argument position and in a loop
(defun in-args (l)
  (let ((n 0))
    (dolist (x l (list :sum n))
      (flet ((val (v) (if (numberp v) v (return-from in-args (list :bad v n)))))
        (incf n (+ (val x) (val x)))))))
(print (list (in-args '(1 2 3)) (in-args '(1 a 3))))
(print (let ((r nil))
         (dotimes (i 4 (reverse r))
           (push (block b (flet ((f (x) (when (oddp x) (return-from b (list :odd x))) x)) (* 10 (f i)))) r))))

;; handler-case and restarts around and inside
(defun with-handlers (x)
  (block b
    (flet ((fail (v) (if (eq v :error) (error "failed ~A" v) (return-from b (list :returned v)))))
      (handler-case (restart-case (list :value (fail x)) (r () :restart))
        (error (c) (list :handled (princ-to-string c)))))))
(print (list (with-handlers :error) (with-handlers 1)))
(defun handler-exit (x)
  (block b
    (flet ((out (c) (declare (ignore c)) (return-from b :from-handler)))
      (flet ((signaller () (handler-bind ((error (lambda (c) (out c)))) (if x (error "e") :quiet))))
        (list (signaller))))))
(print (list (handler-exit t) (handler-exit nil)))

;; many exits: nothing is left pending
(print (let ((n 0))
         (dotimes (i 2000)
           (incf n (block b (labels ((f (d) (if (= d 0) (return-from b 1) (+ 1 (f (1- d)))))) (+ 100 (f (mod i 7)))))))
         n))
(print (list *log* *sp*))
