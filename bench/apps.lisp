;;; Workloads of real libraries other than Lem's (tools/bench-lem.sh), loaded
;;; from Quicklisp (~/quicklisp) by ASDF on both implementations and timed
;;; the same way as bench/libs.lisp: `tools/bench-libs.sh bench/apps.lisp`.
;;; JSON read into hash tables and lists and written back (yason, cl-json:
;;; character streams, generic functions, hash tables of strings), a PEG
;;; grammar (esrap: closures, memoization), dates (local-time: integer
;;; arithmetic, FORMAT) and Lisp source cut into tokens by a regular
;;; expression (cl-ppcre on a real file).  Only the work is timed.
#+sbcl (load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))
#+clysm (require :quicklisp)
(let ((*standard-output* (make-broadcast-stream)) (*error-output* (make-broadcast-stream)))
  (ql:quickload '(:yason :cl-json :esrap :local-time :cl-ppcre) :silent t))

(defun run-bench (name reps fn)
  (funcall fn)
  (let ((start (get-internal-real-time)) (r nil))
    (dotimes (i reps) (setq r (funcall fn)))
    (let ((ms (round (* 1000 (- (get-internal-real-time) start)) internal-time-units-per-second)))
      (format t "~(~A~) ~D ~A~%" name ms
              (let ((s (prin1-to-string r))) (if (> (length s) 40) (subseq s 0 40) s))))))
(defmacro defbench (name (&optional (reps 1)) &body body)
  `(progn (defun ,name () ,@body) (run-bench ',name ,reps #',name)))

;;; a JSON document of 4000 records (about 1 MB): strings with escapes,
;;; integers, floats, booleans, nulls, nested objects and arrays
(defparameter *json*
  (with-output-to-string (o)
    (write-string "{\"records\":[" o)
    (dotimes (i 4000)
      (format o "~:[~;,~]{\"id\":~D,\"name\":\"user ~D \\\"q\\\" \\u00e9\\n\",\"score\":~D.~2,'0D,~
                 \"active\":~:[false~;true~],\"tags\":[\"a~D\",\"b~D\",\"c\"],\"parent\":~:[null~;~:*~D~],~
                 \"address\":{\"city\":\"City ~D\",\"zip\":\"~5,'0D\",\"lines\":[\"~D Main St\",\"Apt ~D\"]}}"
              (plusp i) i i (mod (* i 37) 1000) (mod i 100) (evenp i) i (mod i 7)
              (and (plusp (mod i 3)) (floor i 3)) (mod i 50) (mod (* i 7919) 100000) i (mod i 40)))
    (write-string "]}" o)))

(defun count-leaves (x)
  "The atoms of a JSON value read as hash tables, lists or vectors."
  (cond ((hash-table-p x) (let ((n 0)) (maphash (lambda (k v) (declare (ignore k)) (incf n (count-leaves v))) x) n))
        ((stringp x) 1)
        ((consp x) (let ((n 0))           ; (an alist's entries are dotted pairs)
                     (loop for tail = x then (cdr tail)
                           while (consp tail) do (incf n (count-leaves (car tail)))
                           finally (when tail (incf n (count-leaves tail))))
                     n))
        ((vectorp x) (reduce #'+ x :key #'count-leaves))
        (t 1)))

;;; yason: objects as EQUAL hash tables, arrays as lists
(defvar *yason-data* (yason:parse *json*))
(defbench a-yason-parse (3)
  (count-leaves (yason:parse *json*)))
(defbench a-yason-encode (3)
  (length (with-output-to-string (o) (yason:encode *yason-data* o))))

;;; cl-json: objects as alists with keyword keys (interned by its decoder)
(defvar *cl-json-data* (json:decode-json-from-string *json*))
(defbench a-cl-json-decode (2)
  (count-leaves (json:decode-json-from-string *json*)))
(defbench a-cl-json-encode (2)
  (length (json:encode-json-to-string *cl-json-data*)))

;;; esrap: a grammar of arithmetic with precedence, on 3000 expressions
(esrap:defrule ws (* (or #\space #\tab #\newline)) (:constant nil))
(esrap:defrule digits (+ (digit-char-p character)) (:text t))
(esrap:defrule number (and digits ws) (:function (lambda (l) (parse-integer (first l)))))
(esrap:defrule open (and #\( ws) (:constant nil))
(esrap:defrule close (and #\) ws) (:constant nil))
(esrap:defrule atom (or number (and open sum close))
  (:lambda (x) (if (numberp x) x (second x))))
(esrap:defrule product (and atom (* (and (or #\* #\/) ws atom)))
  (:destructure (first rest)
    (reduce (lambda (acc op) (if (string= (first op) "*") (* acc (third op)) (floor acc (max 1 (third op)))))
            rest :initial-value first)))
(esrap:defrule sum (and ws product (* (and (or #\+ #\-) ws product)))
  (:destructure (w first rest)
    (declare (ignore w))
    (reduce (lambda (acc op) (if (string= (first op) "+") (+ acc (third op)) (- acc (third op))))
            rest :initial-value first)))
(defparameter *expressions*
  (loop for i below 3000
        collect (format nil "(~D + ~D) * ~D - (~D / (~D + 1)) + ~D * (~D - ~D)"
                        i (* 3 i) (mod i 17) (* i i) (mod i 9) (mod i 23) (* 2 i) (mod i 11))))
(defbench a-esrap (2)
  (reduce #'+ *expressions* :key (lambda (e) (esrap:parse 'sum e))))

;;; local-time: timestamps parsed, moved and printed
(defbench a-local-time (3)
  (let ((n 0))
    (dotimes (i 4000 n)
      (let* ((s (format nil "~D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0D.~3,'0DZ"
                        (+ 1990 (mod i 40)) (1+ (mod i 12)) (1+ (mod i 28)) (mod i 24) (mod i 60)
                        (mod (* 7 i) 60) (mod i 1000)))
             (ts (local-time:parse-timestring s))
             (later (local-time:timestamp+ ts (* 37 i) :minute)))
        (incf n (length (local-time:format-timestring nil later :timezone local-time:+utc-zone+)))
        (incf n (local-time:timestamp-day-of-week later))))))

;;; cl-ppcre: the tokens of a Lisp file (src/prims.lisp, 300 KB)
(defparameter *source*
  (with-open-file (in (merge-pathnames "../src/prims.lisp" (or *load-truename* *default-pathname-defaults*)))
    (let ((s (make-string (file-length in))))
      (subseq s 0 (read-sequence s in)))))
(defbench a-ppcre-tokens (2)
  (let ((n 0))
    (ppcre:do-matches (s e "\"(?:[^\"\\\\]|\\\\.)*\"|;[^\\n]*|[()]|#?'|`|,@?|[^\\s()\"';`,]+" *source* n)
      (incf n (- e s)))))
