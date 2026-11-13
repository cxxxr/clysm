;;; Clysm benchmarks, fourth file - the condition system and dynamic control
;;; transfer: handler-case / handler-bind / restart-case with and without a
;;; signal, non-local exits from local functions, closures and recursion,
;;; catch/throw, unwind-protect, special variables.
;;; Run with tools/bench.sh bench/control.lisp.
;;;
;;; The last group (t-...) are exits that have to be Wasm exceptions (the
;;; frames between the exit and its target are not known to the compiler):
;;; they show what such an exit costs.

(defmacro defbench (name (&optional (reps 1)) &body body)
  `(progn
     (defun ,name () ,@body)
     (run-bench ',name ,reps #',name)))

(defun run-bench (name reps fn)
  (funcall fn)                          ; warm up
  (let ((start (get-internal-real-time)) (r nil))
    (dotimes (i reps) (setq r (funcall fn)))
    (let ((ms (round (* 1000 (- (get-internal-real-time) start))
                     internal-time-units-per-second)))
      (format t "~(~A~) ~D ~A~%" name ms
              (let ((s (prin1-to-string r))) (if (> (length s) 40) (subseq s 0 40) s))))))

;;; ------------------------------------- conditions, exits, specials

(define-condition v-error (error)
  ((code :initarg :code :reader v-code)
   (place :initarg :place :initform nil :reader v-place)
   (detail :initarg :detail :initform "none" :reader v-detail))
  (:report (lambda (c s) (format s "v-error ~A at ~A" (v-code c) (v-place c)))))
(define-condition v-warning (warning) ((n :initarg :n :reader v-n)))
(define-condition v-note (condition) ((n :initarg :n :reader v-n)))
(define-condition v-sub-error (v-error) ((extra :initarg :extra :initform 0 :reader v-extra)))

(defun v-risky (i)
  (case (mod i 7)
    (0 (error 'v-error :code i :place 'here :detail "d"))
    (3 (error 'v-sub-error :code i :extra 1))
    (5 (car i))
    (t i)))

;; handler-case with several clauses and a :no-error clause
(defbench v-hc-clauses (20)
  (let ((s 0))
    (dotimes (i 30000)
      (incf s (handler-case (v-risky i)
                (v-sub-error (e) (+ (v-code e) (v-extra e)))
                (v-error (e) (- (v-code e)))
                (type-error () 1)
                (:no-error (v) (* 2 v)))))
    s))

;; a handler-case that never signals, with three clauses, around a call
(defun v-safe (i) (logand i 15))
(defbench v-hc-nosignal (10)
  (let ((s 0))
    (dotimes (i 1000000)
      (incf s (handler-case (v-safe i)
                (v-error (e) (v-code e))
                (type-error () 1)
                (warning () 2))))
    s))

;; nested handler-binds 5 deep, the signal handled by the outermost
(defun v-nest (i)
  (handler-bind ((type-error (lambda (c) (declare (ignore c)) nil)))
    (handler-bind ((v-warning (lambda (c) (declare (ignore c)) nil)))
      (handler-bind ((arithmetic-error (lambda (c) (declare (ignore c)) nil)))
        (handler-bind ((v-note (lambda (c) (declare (ignore c)) nil)))
          (handler-bind ((control-error (lambda (c) (declare (ignore c)) nil)))
            (error 'v-error :code i)))))))
(defbench v-hb-nested (20)
  (let ((s 0))
    (dotimes (i 20000)
      (incf s (block b
                (handler-bind ((v-error (lambda (c) (return-from b (v-code c)))))
                  (v-nest i)))))
    s))

;; handler-bind that never signals: setup cost with two handlers closing over a variable
(defbench v-hb-nosignal (10)
  (let ((s 0) (seen 0))
    (dotimes (i 1000000)
      (handler-bind ((v-error (lambda (c) (incf seen (v-code c))))
                     (warning (lambda (c) (declare (ignore c)) (incf seen))))
        (incf s (v-safe i))))
    (+ s seen)))

;; ignore-errors around an error raised by the runtime
(defbench v-ignore-errors (20)
  (let ((s 0))
    (dotimes (i 20000)
      (multiple-value-bind (v c) (ignore-errors (if (evenp i) (car i) (aref "abc" (logand i 7))))
        (incf s (if c 1 (char-code v)))))
    s))

;; restart-case with three restarts, invoked by name and through the standard functions
(defun v-parse (i)
  (restart-case (if (zerop (mod i 4)) (error 'v-error :code i) i)
    (use-value (v) v)
    (continue () 0)
    (skip-it () :report "Skip." -1)))
(defbench v-restarts (20)
  (let ((s 0))
    (dotimes (i 20000)
      (incf s (handler-bind ((v-error (lambda (c)
                                        (case (mod (v-code c) 3)
                                          (0 (use-value 7 c))
                                          (1 (continue c))
                                          (t (invoke-restart 'skip-it))))))
                (v-parse i))))
    s))

;; restart-case that is not used, with-simple-restart
(defbench v-restart-nosignal (10)
  (let ((s 0))
    (dotimes (i 500000)
      (incf s (restart-case (v-safe i) (use-value (v) v) (abort () 0)))
      (incf s (with-simple-restart (skip "Skip ~A" i) (v-safe i))))
    s))

;; cerror continued, warn muffled, signal declined
(defbench v-cerror-warn (10)
  (let ((s 0))
    (dotimes (i 20000)
      (handler-bind ((v-error #'continue)
                     (v-warning (lambda (w) (incf s (v-n w)) (muffle-warning w)))
                     (v-note (lambda (c) (incf s (v-n c)))))
        (cerror "go on" 'v-error :code i)
        (warn 'v-warning :n 1)
        (signal 'v-note :n 2)
        (signal 'v-note :n 3)))
    s))

;; conditions with several slots made and read without signalling
(defbench v-make-condition (10)
  (let ((s 0))
    (dotimes (i 100000)
      (let ((c (make-condition 'v-sub-error :code i :place 'p :detail "x" :extra 2)))
        (incf s (+ (v-code c) (v-extra c) (length (v-detail c))))))
    s))

;; return-from across two closure levels
(defun v-call (f) (funcall f))
(defun v-find2 (rows target)
  (block found
    (v-call (lambda ()
              (dolist (row rows)
                (v-call (lambda ()
                          (dolist (x row) (when (eql x target) (return-from found x))))))))
    nil))
(defbench v-return-closures (10)
  (let ((rows '((1 2 3) (4 5 6) (7 8 9))) (s 0))
    (dotimes (i 200000)
      (incf s (or (v-find2 rows (logand i 15)) 0)))
    s))

;; return-from a block out of a recursive local function
(defun v-tree-find (tree pred)
  (labels ((walk (x)
             (cond ((consp x) (walk (car x)) (walk (cdr x)))
                   ((and x (funcall pred x)) (return-from v-tree-find x)))))
    (walk tree)
    nil))
(defbench v-return-labels (10)
  (let ((tree '((1 2 (3 4)) (5 (6 (7 8))) 9 (10 11 (12 (13 14))))) (s 0))
    (dotimes (i 200000)
      (let ((k (logand i 15)))
        (incf s (or (v-tree-find tree (lambda (x) (> x k))) 0))))
    s))

;; go out of a closure called by a library function
(defbench v-go-closure (10)
  (let ((s 0) (v (vector 5 3 8 1 9 2)))
    (dotimes (i 100000)
      (let ((k (logand i 7)))
        (tagbody
           (map nil (lambda (x) (when (> x k) (incf s x) (go done))) v)
           (incf s 100)
         done)))
    s))

;; throw across 100 frames
(defun v-deep (n) (if (= n 0) (throw 'v-tag 1) (+ 1 (v-deep (1- n)))))
(defbench v-throw-100 (10)
  (let ((s 0))
    (dotimes (i 20000)
      (incf s (catch 'v-tag (v-deep 100))))
    s))

;; catch with a computed tag, throw from two frames down
(defun v-thrower (tag i) (if (oddp i) (throw tag i) i))
(defun v-mid (tag i) (+ 1 (v-thrower tag i)))
(defbench v-catch-dynamic (10)
  (let ((s 0) (tags (vector 'a 'b (list 1) "tag")))
    (dotimes (i 200000)
      (let ((tag (svref tags (logand i 3))))
        (incf s (catch tag (v-mid tag i)))))
    s))

;; unwind-protect: normal exit, return-from through it, throw through it
(defvar *v-log* 0)
(defun v-uwp (i)
  (block b
    (unwind-protect
         (cond ((zerop (mod i 3)) (return-from b 1))
               ((zerop (mod i 31)) (throw 'v-u 2))
               (t i))
      (incf *v-log*))))
(defbench v-unwind-protect (10)
  (let ((s 0))
    (setq *v-log* 0)
    (dotimes (i 300000)
      (incf s (catch 'v-u (v-uwp i))))
    (+ s *v-log*)))

;; special variables bound in a loop and read in a callee: 1, 3 and 6 variables
(defvar *v1* 0) (defvar *v2* 0) (defvar *v3* 0) (defvar *v4* 0) (defvar *v5* 0) (defvar *v6* 0)
(defun v-read1 () *v1*)
(defun v-read3 () (+ *v1* *v2* *v3*))
(defun v-read6 () (+ *v1* *v2* *v3* *v4* *v5* *v6*))
(defbench v-special-1 (10)
  (let ((s 0))
    (dotimes (i 1000000) (let ((*v1* i)) (setq s (logand (+ s (v-read1)) #xFFFFF))))
    s))
(defbench v-special-3 (10)
  (let ((s 0))
    (dotimes (i 1000000)
      (let ((*v1* i) (*v2* 2) (*v3* s)) (setq s (logand (+ s (v-read3)) #xFFFFF))))
    s))
(defbench v-special-6 (10)
  (let ((s 0))
    (dotimes (i 500000)
      (let* ((*v1* i) (*v2* (1+ *v1*)) (*v3* 3) (*v4* 4) (*v5* 5) (*v6* s))
        (setq s (logand (+ s (v-read6)) #xFFFFF))))
    s))

;; special parameters, multiple-value-bind of specials, special reads and writes in a loop
(defun v-sp-param (*v1* *v2*) (v-read3))
(defbench v-special-misc (10)
  (let ((s 0))
    (dotimes (i 500000)
      (incf s (v-sp-param i 1))
      (multiple-value-bind (*v4* *v5*) (floor i 7) (incf s (logand (v-read6) 15)))
      (setq *v6* (logand (+ *v6* *v1* i) 255)))
    (logand s #xFFFFFF)))

;; progv
(defbench v-progv (10)
  (let ((s 0) (syms '(*v1* *v2* *v3*)))
    (dotimes (i 300000)
      (progv syms (list i 2 3) (incf s (logand (v-read3) 7)))
      (progv '(*v4*) (list i) (incf s (logand *v4* 3))))
    s))

;; a special bound around a non-local exit
(defun v-sp-exit (i)
  (let ((*v1* i))
    (if (oddp i) (throw 'v-sp *v1*) (v-read1))))
(defbench v-special-exit (10)
  (let ((s 0))
    (dotimes (i 200000)
      (incf s (logand (catch 'v-sp (let ((*v2* i)) (v-sp-exit i))) 7))
      (incf s (block b (let ((*v3* i)) (when (zerop (logand i 3)) (return-from b 1)) (logand *v3* 1)))))
    s))

;;; ------------------------------------------------- inlined lambdas

(defun first-big (list limit)
  (mapc (lambda (x) (when (> x limit) (return-from first-big x))) list)
  nil)
(defun sum-until (list stop)
  (let ((s 0))
    (block nil (mapcar (lambda (x) (if (eql x stop) (return) (incf s x))) list))
    s))
(defun pick (v k)
  (let ((probe (lambda (i) (when (eql (svref v i) k) (return-from pick i)))))
    (funcall probe 0) (funcall probe 1) (funcall probe 2)
    nil))
(defun go-lambda (list)
  (let ((n 0))
    (tagbody
       (mapc (lambda (x) (when (minusp x) (go out)) (incf n x)) list)
     out)
    n))
(defbench r-mapc-return (10)
  (let ((s 0) (l (list 1 2 3 4 5 6 7 8)))
    (dotimes (i 1000000) (incf s (or (first-big l (logand i 15)) 0)))
    s))
(defbench r-mapcar-return (10)
  (let ((s 0) (l (list 1 2 3 4 5 6 7 8)))
    (dotimes (i 500000) (incf s (sum-until l (logand i 15))))
    s))
(defbench r-funcall-return (10)
  (let ((s 0) (v (vector 3 5 7)))
    (dotimes (i 2000000) (incf s (or (pick v (logand i 7)) 0)))
    s))
(defbench r-mapc-go (10)
  (let ((s 0) (l (list 1 2 3 -4 5 6)))
    (dotimes (i 1000000) (incf s (go-lambda l)))
    s))

;;; --------------------------------- exits that are Wasm exceptions

(defvar *t-thrower* (lambda (i) (throw 't-tag i)))
(defvar *t-identity* (lambda (x) x))
(defun t-deep (n f i) (if (= n 0) (funcall f i) (+ 1 (t-deep (1- n) f i))))
;; throw from a closure, one frame below the catch
(defbench t-throw-closure (10)
  (let ((s 0) (f *t-thrower*))
    (dotimes (i 200000)
      (setq s (logand (+ s (catch 't-tag (funcall f i))) #xffffff)))
    s))
;; throw from a closure across 20 frames
(defbench t-throw-closure-20 (10)
  (let ((s 0) (f *t-thrower*))
    (dotimes (i 50000)
      (setq s (logand (+ s (catch 't-tag (t-deep 20 f i))) #xffffff)))
    s))
;; return-from in a closure that escaped to unknown code
(defbench t-return-closure (10)
  (let ((s 0))
    (dotimes (i 200000)
      (setq s (logand (+ s (block b (funcall (funcall *t-identity* (lambda () (return-from b i)))) 5)) #xffffff)))
    s))
;; return-from out of a library function that calls the closure
(defbench t-return-maphash (10)
  (let ((s 0) (h (make-hash-table)))
    (dotimes (i 8) (setf (gethash i h) (* i i)))
    (dotimes (i 100000)
      (let ((k (logand i 7)))
        (incf s (block found (maphash (lambda (key v) (when (eql key k) (return-from found v))) h) 0))))
    s))
;; an error signalled three ordinary calls below the handler-case
(define-condition t-error (error) ((v :initarg :v :reader t-error-v)))
(defun t-level3 (x) (if (zerop (mod x 3)) (error 't-error :v x) x))
(defun t-level2 (x) (let ((r (t-level3 x))) (if (> r 1000000) (t-level2 (- x 1000000)) (+ r 1))))
(defun t-level1 (x) (let ((r (t-level2 x))) (if (> r 1000000) (t-level1 (- x 1000000)) (+ r 1))))
(defbench t-error-3-calls (10)
  (let ((s 0))
    (dotimes (i 100000)
      (incf s (handler-case (+ 1 (t-level1 i)) (t-error (c) (- (t-error-v c))))))
    s))
