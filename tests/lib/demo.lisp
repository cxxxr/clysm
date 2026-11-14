;;; A library for tests/lib/run.mjs: functions exported to JavaScript.

(defvar *calls* 0)

(defun count-words (text)
  (incf *calls*)
  (let ((n 0) (in nil))
    (loop for c across text
          do (if (member c '(#\Space #\Newline #\Tab))
                 (setf in nil)
                 (unless in (setf in t) (incf n))))
    n))

(defun deep (n) (if (zerop n) 0 (1+ (deep (1- n)))))

(defun check (x) (error "cannot handle ~S" x))
(defun fails (x) (check x) x)

(defun upcase-all (array)
  ;; a JavaScript array of strings -> a new array
  (map 'vector #'string-upcase
       (loop for i below (js:get array "length") collect (js:get array i))))

(defun fetch-twice (f)
  ;; F is an async JavaScript function; two calls wait at the same time
  (let ((results (js:await-all (list (js:funcall f "a") (js:funcall f "b")))))
    (format nil "~{~A~^+~}" results)))

(js:export "countWords" #'count-words :type "(text: string) => number")
(js:export "calls" (lambda () *calls*) :type "() => number")
(js:export "deep" #'deep)
(js:export "fails" #'fails)
(js:export "upcaseAll" #'upcase-all :type "(items: string[]) => string[]")
(js:export "fetchTwice" #'fetch-twice :async t :type "(f: (k: string) => Promise<string>) => Promise<string>")
(js:export "path" (lambda () (js:call (js:require "node:path") "join" "a" "b")))

;; subprocesses: run and wait, and a long-lived process read while waiting
(js:export "echo" (lambda (x) (nth-value 0 (sys::%run-program-sync (list "echo" x)))))
(js:export "catLines" (lambda (lines)
                        (multiple-value-bind (process in out)
                            (sys::%launch-program '("cat") :input :stream :output :stream)
                          (let ((result (loop for i below (js:get lines "length")
                                              collect (progn (write-line (js:get lines i) in)
                                                             (finish-output in)
                                                             (read-line out)))))
                            (close in)
                            (sys::%process-wait process)
                            (coerce result 'vector))))
           :async t)

;; a string as it is (a byte order mark at its start is a character like the others)
(js:export "same" (lambda (s) s))
