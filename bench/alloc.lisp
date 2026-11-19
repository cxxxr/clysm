;;; Allocation and garbage collection (docs/INTERNALS.md, "Allocation, garbage
;;; collection and the engine's flags"): garbage that dies young (a-), and
;;; data that survives (s-). Run by tools/bench.sh like suite.lisp; not in
;;; tools/bench-all.sh.

(defmacro defbench (name (&optional (reps 1)) &body body)
  `(progn (defun ,name () ,@body) (run-bench ',name ,reps #',name)))
(defun run-bench (name reps fn)
  (funcall fn)
  (let ((start (get-internal-real-time)) (r nil))
    (dotimes (i reps) (setq r (funcall fn)))
    (let ((ms (round (* 1000 (- (get-internal-real-time) start)) internal-time-units-per-second)))
      (format t "~(~A~) ~D ~A~%" name ms
              (let ((s (prin1-to-string r))) (if (> (length s) 40) (subseq s 0 40) s))))))

;;; binary trees: short-lived trees beside one that lives long
(defun make-tree (d) (if (= d 0) (cons nil nil) (cons (make-tree (1- d)) (make-tree (1- d)))))
(defun check-tree (n) (if (car n) (+ 1 (check-tree (car n)) (check-tree (cdr n))) 1))
(defun binary-trees (max-depth)
  (let* ((long-lived (make-tree max-depth)) (sum 0))
    (loop for d from 4 to max-depth by 2
          do (let ((iters (ash 1 (+ (- max-depth d) 4))))
               (dotimes (i iters) (incf sum (check-tree (make-tree d))))))
    (+ sum (check-tree long-lived))))
(defbench a-binary-trees (1) (binary-trees 18))

(defstruct node left right val)
(defun make-stree (d) (if (= d 0) (make-node :val 1) (make-node :left (make-stree (1- d)) :right (make-stree (1- d)) :val d)))
(defun check-stree (n) (if (node-left n) (+ (node-val n) (check-stree (node-left n)) (check-stree (node-right n))) (node-val n)))
(defbench a-struct-trees (20) (let ((s 0)) (dotimes (i 40) (incf s (check-stree (make-stree 12)))) s))

(defun rev-copy (l) (let ((r nil)) (dolist (x l r) (push x r))))
(defbench a-list-churn (20)
  (let ((l (loop for i below 1000 collect i)) (s 0))
    (dotimes (i 2000) (setq l (rev-copy l)) (incf s (car l))) s))

(defun adder (n) (lambda (x) (+ x n)))
(defbench a-closures (20)
  (let ((s 0)) (dotimes (i 200000) (incf s (funcall (adder i) 1))) s))

(defun float-sum (v) (let ((acc nil)) (dotimes (i (length v)) (push (* 1.5d0 (aref v i)) acc)) (reduce #'+ acc)))
(defbench a-boxed-floats (20)
  (let ((v (make-array 20000 :initial-element 2d0)) (s 0d0)) (dotimes (i 10) (incf s (float-sum v))) s))

(defbench a-vectors (20)
  (let ((s 0)) (dotimes (i 100000) (let ((v (make-array 8 :initial-element i))) (incf s (svref v 7)))) s))

;;; data that survives: what an editor's buffers, undo lists and indexes are
;;; like while they are built
(defbench s-build-list (3) (let ((l nil)) (dotimes (i 3000000) (push (cons i i) l)) (length l)))
(defbench s-build-hash (3) (let ((h (make-hash-table))) (dotimes (i 500000) (setf (gethash i h) (list i))) (hash-table-count h)))
(defbench s-build-strings (3)
  (let ((v (make-array 300000))) (dotimes (i 300000) (setf (svref v i) (format nil "line ~D" i))) (length (svref v 299999))))
