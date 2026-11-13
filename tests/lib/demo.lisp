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
