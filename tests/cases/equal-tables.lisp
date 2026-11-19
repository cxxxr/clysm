;;; EQUAL hash tables of string keys (simple, with a fill pointer,
;;; displaced) and tables made with a constant test: entries found, updated,
;;; removed, grown, and the same results as with the general paths.
(defun buffer (s)
  (let ((b (make-array 2 :element-type 'character :adjustable t :fill-pointer 0)))
    (map nil (lambda (c) (vector-push-extend c b)) s)
    b))
(let ((h (make-hash-table :test #'equal)))
  (setf (gethash "id" h) 1 (gethash (buffer "name") h) 2 (gethash "score" h) 3)
  (setf (gethash (buffer "id") h) 10)                     ; updates "id"
  (setf (gethash (make-array 4 :element-type 'character :displaced-to "xxscore" :displaced-index-offset 2) h) 30)
  (print (list (hash-table-count h) (gethash "id" h) (gethash "name" h) (gethash (buffer "score") h)
               (multiple-value-list (gethash "none" h)) (gethash 'id h) (gethash "ID" h)))
  (remhash (buffer "name") h)
  (print (list (hash-table-count h) (gethash "name" h :gone)))
  ;; strings among other keys, and growth
  (setf (gethash '(1 "a") h) :list (gethash 42 h) :n (gethash #\c h) :c (gethash 1.5d0 h) :d)
  (dotimes (i 200) (setf (gethash (format nil "k~D" i) h) i))
  (print (list (hash-table-count h) (gethash "k199" h) (gethash (buffer "k7") h) (gethash (list 1 "a") h)
               (gethash 42 h) (gethash #\c h) (gethash 1.5d0 h) (hash-table-test h))))
(let ((tables (list (make-hash-table :test 'eq) (make-hash-table :test #'eql) (make-hash-table :test 'equal :size 3)
                    (make-hash-table :size 100 :test #'equalp) (make-hash-table) (make-hash-table :size 0))))
  (dolist (h tables) (setf (gethash "Key" h) 1 (gethash 7 h) 2))
  (print (mapcar (lambda (h) (list (hash-table-test h) (gethash "KEY" h) (gethash 7 h) (>= (hash-table-size h) 1)))
                 tables)))
(print (handler-case (make-hash-table :test #'equal :size 'big) (error () :size-error)))
;; CONCATENATE of strings into a string, with APPLY
(print (list (apply #'concatenate 'string '("ab" "-" "cd" "")) (concatenate 'simple-string "x" "yz")
             (apply #'concatenate 'string (list "a" (make-array 2 :element-type 'character :initial-contents "bc" :fill-pointer 1)))
             (concatenate 'string "a" '(#\b) #(#\c)) (concatenate 'string) (apply #'concatenate 'string nil)))
