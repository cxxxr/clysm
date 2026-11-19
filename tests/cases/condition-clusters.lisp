;;; Signalling a condition of type ERROR that no handler takes returns NIL,
;;; also in code loaded into the image (whose script runner once handled
;;; ERROR itself); RESTART-CASE clusters (restarts made when asked for);
;;; HANDLER-BIND of several lambdas (one closure for them).
(define-condition my-error (error) ((code :initarg :code :reader my-error-code)))
(define-condition my-note () ((n :initarg :n :reader my-note-n)))

(defun signal-declined ()
  (let ((s 0))
    (dotimes (i 4)
      (handler-bind ((warning (lambda (w) (incf s) (muffle-warning w))))
        (when (oddp i) (warn "w ~D" i)))
      (handler-bind ((condition (lambda (c) (declare (ignore c)) (incf s 2))))
        (incf s (if (signal 'my-error :code i) 100 0))))
    s))
(format t "signal declined: ~A~%" (signal-declined))
(format t "signal no handler: ~A~%" (signal 'my-error :code 1))
(format t "error caught: ~A~%" (handler-case (error 'my-error :code 7) (my-error (e) (my-error-code e))))

;; several handlers of HANDLER-BIND: order, declining, the type tested
(defun handlers (datum)
  (let ((seen nil))
    (handler-case
        (handler-bind ((my-note (lambda (c) (push (list :note (my-note-n c)) seen)))
                       (my-error (lambda (c) (push (list :error (my-error-code c)) seen)))
                       (condition (lambda (c) (declare (ignore c)) (push :any seen))))
          (signal datum)
          (push :returned seen))
      (my-error (e) (push (list :caught (my-error-code e)) seen)))
    (reverse seen)))
(format t "handlers note: ~S~%" (handlers (make-condition 'my-note :n 3)))
(format t "handlers error: ~S~%" (handlers (make-condition 'my-error :code 4)))
(format t "nested: ~S~%"
        (let ((log nil))
          (handler-bind ((my-note (lambda (c) (declare (ignore c)) (push :outer log)))
                         (error (lambda (c) (declare (ignore c)) (push :outer-error log))))
            (handler-bind ((my-note (lambda (c) (declare (ignore c)) (push :inner log)
                                      (signal 'my-note :n 9)))
                           (warning (lambda (c) (declare (ignore c)) (push :warning log))))
              (signal 'my-note :n 1)))
          (reverse log)))

;; restarts of RESTART-CASE
(defun restarts-here ()
  (mapcar #'restart-name
          (remove-if-not (lambda (r) (member (restart-name r) '(first second third skip)))
                         (compute-restarts))))
(format t "restart names: ~S~%"
        (restart-case (restart-case (restarts-here)
                        (third () 3))
          (first () 1)
          (second (x) :report "Second." x)))
(format t "find-restart: ~S~%"
        (restart-case (let ((r (find-restart 'second)))
                        (list (restart-name r) (eq r (find-restart 'second))
                              (princ-to-string r)))
          (first () 1)
          (second () :report (lambda (s) (write-string "the second" s)) 2)))
(format t "invoke by name: ~S~%"
        (restart-case (invoke-restart 'second 5 6)
          (first () 1)
          (second (a b) (+ a b))))
(format t "invoke object: ~S~%"
        (restart-case (invoke-restart (find-restart 'first))
          (first () :first)
          (second () :second)))
(format t "with-simple-restart: ~S ~S~%"
        (multiple-value-list (with-simple-restart (skip "Skip ~A" 1) (invoke-restart 'skip)))
        (with-simple-restart (skip "Skip ~A" 2)
          (with-output-to-string (s) (format s "~A" (find-restart 'skip)))))
(format t "use-value: ~S~%"
        (handler-bind ((my-error (lambda (c) (use-value (* 10 (my-error-code c)) c))))
          (restart-case (error 'my-error :code 4)
            (use-value (v) v))))
(format t "associated: ~S~%"
        (handler-case
            (restart-case (signal 'my-error :code 1)
              (first () 1))
          (my-error (e) (mapcar #'restart-name
                                (remove-if-not (lambda (r) (eq (restart-name r) 'first))
                                               (compute-restarts e))))))
(format t "empty restart-case: ~S~%"
        (handler-bind ((my-error (lambda (c) (declare (ignore c)) nil)))
          (restart-case (signal 'my-error :code 1))))
(format t "restart-bind: ~S~%"
        (restart-bind ((ten (lambda () 10) :report-function (lambda (s) (write-string "Ten" s))))
          (list (invoke-restart 'ten) (princ-to-string (find-restart 'ten)))))
(format t "test function: ~S~%"
        (restart-case (list (null (find-restart 'hidden)) (not (null (find-restart 'shown))))
          (hidden () :test (lambda (c) (declare (ignore c)) nil) 1)
          (shown () 2)))
