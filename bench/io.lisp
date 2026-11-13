;;; Clysm benchmarks, fourth file — FORMAT, the printer, the reader and streams.
;;; These are variants of the I/O programs of the other files: other
;;; directives and parameters, other kinds of streams, other data.
;;; Run with tools/bench.sh bench/io.lisp.

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

;; the file benchmarks write build/io-bench.tmp
(ensure-directories-exist "build/")

;;; ------------------------------------------------------------ FORMAT

;; integer directives with parameters, in a table
(defbench i-format-table (20)
  (let ((n 0))
    (dotimes (i 4000)
      (incf n (length (format nil "~8D|~8,'0D|~8@D|~:D|~8:D|~6X|~6,'0X|~12B|~4O|~3R|~36R"
                              i (- i) i (* i 12345) (* i 77) i i (logand i 1023) i i (* i 99)))))
    n))

;; column directives and padding
(defbench i-format-columns (20)
  (let ((n 0) (names '("alpha" "be" "gamma-delta" "x")))
    (dotimes (i 3000)
      (dolist (nm names)
        (incf n (length (format nil "~12A|~12@A|~12S|~3,2,1,'.A|~C~C" nm nm nm nm #\a #\b)))))
    n))

;; English and Roman numbers, plurals
(defbench i-format-english (40)
  (let ((n 0))
    (dotimes (i 3000)
      (incf n (length (format nil "~R ~:R ~@R ~D item~:P ~D famil~:@P" i i (1+ (mod i 3000)) i i))))
    n))

;; iteration over nested lists, ~^ and ~:^, counts
(defbench i-format-nested (20)
  (let ((n 0)
        (rows '((1 2 3) (4 5) (6) () (7 8 9 10)))
        (alist '((a . 1) (b . 2) (c . 3))))
    (dotimes (i 3000)
      (incf n (length (format nil "~{[~{~D~^,~}]~^ ~}" rows)))
      (incf n (length (format nil "~:{~A~^-~A~:^/~}" '((1 2) (3 4) (5)))))
      (incf n (length (format nil "~@{~A~^+~}" i 2 3)))
      (incf n (length (format nil "~2{~A ~}~{~A~}" '(1 2 3 4) '(x y))))
      (incf n (length (format nil "~{~A~^, ~}" (mapcar #'car alist))))
      (incf n (length (format nil "~{~{~A=~A~}~^; ~}" '((k 1) (l 2) (m 3))))))
    n))

;; conditionals, case conversion, argument jumps
(defbench i-format-cond (20)
  (let ((n 0))
    (dotimes (i 5000)
      (incf n (length (format nil "~[none~;one~;two~:;lots~] ~:[f~;t~] ~@[<~A>~] ~(~A~) ~:(~A~) ~@(~A~)"
                              (mod i 5) (evenp i) (and (oddp i) i) "MiXed Case" "hello world" "hello world")))
      (incf n (length (format nil "~A ~:*~A ~2*~A ~@*~A ~#[zero~;one~:;many~]" 1 2 3 4))))
    n))

;; floats of very different magnitudes
(defbench i-format-floats (5)
  (let ((n 0) (xs '(0.0d0 1.0d0 -1.5d0 3.14159265358979d0 1234567.891d0 1.0d-5 6.02d23 -2.5d-10 0.001d0 99999.995d0
                    0.5 1.25 -100.0 1.0e10 3.3333333)))
    (dotimes (i 300)
      (dolist (x xs)
        (incf n (length (format nil "~F|~,3F|~10,2F|~10,4,2F|~,2,,,'*F" x x x x x)))
        (incf n (length (format nil "~E|~,3E|~12,4E|~12,4,2E|~,2,,2E" x x x x x)))
        (incf n (length (format nil "~G|~,3G|~12,4G|~$|~3$|~2,4$|~@$" x x x x x x x)))))
    n))

;; money and percentages computed in a loop
(defbench i-format-money (10)
  (let ((n 0))
    (dotimes (i 8000)
      (incf n (length (format nil "~$ ~,1F% ~6,2F ~,4F" (* i 0.37d0) (/ (* 100d0 i) 8000) (/ i 7.0) (sqrt (float i 1d0))))))
    n))

;; the same report to a string stream, a broadcast stream and a file
(defun io-report (out k)
  (dotimes (i k)
    (format out "~&~4D ~10A ~8,2F ~:[no~;yes~]~30T~S~%" i "name" (* i 1.5d0) (evenp i) (list i 'x "y"))))
(defbench i-format-streams (10)
  (let ((n 0))
    (incf n (length (with-output-to-string (s) (io-report s 1500))))
    (let* ((a (make-string-output-stream)) (b (make-string-output-stream))
           (bs (make-broadcast-stream a b)))
      (io-report bs 1500)
      (incf n (+ (length (get-output-stream-string a)) (length (get-output-stream-string b)))))
    (with-open-file (o "build/io-bench.tmp" :direction :output :if-exists :supersede)
      (io-report o 1500)
      (incf n (file-position o)))
    n))

;; justification and logical blocks: these must stay correct
(defbench i-format-justify (40)
  (let ((n 0))
    (dotimes (i 1500)
      (incf n (length (format nil "~20<~A~;~A~>|~20:@<~A~>|~15<~D~>" "left" "right" "mid" i)))
      (incf n (length (format nil "~<~A ~_~A~:>" (list i "x")))))
    n))

;;; ------------------------------------------------------------ the printer

(defstruct io-point x y (tag :none))
(defclass io-thing () ((name :initarg :name :reader io-thing-name) (size :initarg :size :reader io-thing-size)))
(defmethod print-object ((x io-thing) stream)
  (print-unreadable-object (x stream :type t :identity nil)
    (format stream "~A (~D)" (io-thing-name x) (io-thing-size x))))

;; prin1 of nested data
(defbench i-prin1-nested (10)
  (let ((n 0)
        (data (list 1 "two" #\3 'four 5.0 6.5d0 (vector 7 8 "nine" '(10 11)) '(a . b) :key
                    "a \"quoted\" string with \\ in it" 1234567890123 -42 3/4 nil t)))
    (dotimes (i 4000)
      (incf n (length (prin1-to-string data)))
      (incf n (length (princ-to-string data))))
    n))

(defbench i-prin1-flat (10)
  (let ((n 0) (v (vector 1 2 3 4 5 6 7 8)) (syms '(alpha beta gamma delta)))
    (dotimes (i 5000)
      (incf n (length (prin1-to-string v)))
      (incf n (length (prin1-to-string syms)))
      (incf n (length (prin1-to-string "a string")))
      (incf n (length (prin1-to-string :keyword)))
      (incf n (length (prin1-to-string #\x)))
      (let ((*print-pretty* nil)) (incf n (length (prin1-to-string syms)))))
    n))

(defbench i-print-numbers (30)
  (let ((n 0))
    (dotimes (i 5000)
      (incf n (length (princ-to-string (* i 1000003))))
      (incf n (length (princ-to-string (- (* i i i)))))
      (incf n (length (prin1-to-string (* i 0.25))))
      (incf n (length (prin1-to-string (* i 1.0d-3))))
      (incf n (length (prin1-to-string (/ (1+ i) 3))))
      (incf n (length (write-to-string i :base 16 :radix t)))
      (incf n (length (write-to-string (- i) :base 8))))
    n))

(defbench i-print-structs (10)
  (let ((n 0) (p (make-io-point :x 1 :y 2.5)) (th (make-instance 'io-thing :name "box" :size 3)))
    (dotimes (i 3000)
      (incf n (length (prin1-to-string p)))
      (incf n (length (princ-to-string th)))
      (incf n (length (format nil "~A and ~S" th p)))
      (let ((*print-pretty* nil)) (incf n (length (prin1-to-string (list p th))))))
    n))

;; output functions on a string stream
(defbench i-write-mix (40)
  (let ((n 0))
    (dotimes (k 20)
      (incf n (length (with-output-to-string (s)
                        (dotimes (i 500)
                          (write-string "key" s) (write-char #\= s) (prin1 i s) (write-char #\; s)
                          (write-string "a longer piece of text" s :start 2 :end 15)
                          (write-line "end" s) (princ "str" s) (print 'sym s) (fresh-line s)
                          (write i :stream s :base 2) (terpri s))))))
    n))

;;; ------------------------------------------------------------ the reader

(defbench i-read-data (10)
  (let ((n 0))
    (dotimes (i 3000)
      (incf n (length (read-from-string "(1 2.5 -3 4/5 \"a string\" #\\x sym :key #(1 2 3) (nested (list)) 'q 1.5e3 #xFF)")))
      (incf n (read-from-string "12345"))
      (incf n (length (symbol-name (read-from-string "some-symbol"))))
      (incf n (length (read-from-string "\"a string with \\\" an escape\"")))
      (incf n (round (read-from-string "-17.25d0"))))
    n))

(defbench i-read-stream (40)
  (let ((text (with-output-to-string (o)
                (dotimes (i 400) (format o "(item ~D \"name ~D\" ~F (~D . ~D)) " i i (* i 0.5) i (* 2 i)))))
        (n 0))
    (dotimes (k 5)
      (with-input-from-string (in text)
        (loop for form = (read in nil :eof) until (eq form :eof)
              do (incf n (+ (second form) (length (third form)))))))
    n))

(defbench i-parse-ints (40)
  (let ((n 0) (strs '("0" "42" "-17" "+99999" "123456789" "  77  " "12345678901234567890")))
    (dotimes (i 10000)
      (dolist (s strs) (incf n (logand (parse-integer s) 1023)))
      (incf n (parse-integer "xx1234yy" :start 2 :end 6))
      (incf n (parse-integer "777" :radix 8))
      (incf n (or (parse-integer "zz" :junk-allowed t) 1)))
    n))

;;; ------------------------------------------------------------ streams

(defbench i-read-lines (20)
  (let ((text (with-output-to-string (o)
                (dotimes (i 300)
                  (dotimes (j (1+ (mod i 40))) (write-string "word " o))
                  (terpri o))))
        (n 0))
    (dotimes (k 10)
      (with-input-from-string (in text)
        (loop for line = (read-line in nil) while line do (incf n (length line))))
      (with-input-from-string (in text)
        (loop for c = (read-char in nil) while c
              do (when (char= c #\w) (incf n))
                 (when (char= c #\Space) (unread-char c in) (read-char in)))))
    n))

(defbench i-file-roundtrip (50)
  (let ((n 0))
    (with-open-file (o "build/io-bench.tmp" :direction :output :if-exists :supersede)
      (dotimes (i 2000)
        (write-string "line " o) (princ i o) (write-char #\Space o) (write-line "of text in a file" o)))
    (with-open-file (in "build/io-bench.tmp")
      (loop for line = (read-line in nil) while line do (incf n (length line))))
    (with-open-file (in "build/io-bench.tmp")
      (loop for c = (read-char in nil) while c do (when (char= c #\e) (incf n))))
    (with-open-file (in "build/io-bench.tmp")
      (loop for c = (peek-char nil in nil) while c do (read-char in) (when (char= c #\l) (incf n))))
    n))

(defbench i-string-build (80)
  (let ((n 0))
    (dotimes (i 3000)
      (incf n (length (with-output-to-string (o)
                        (dotimes (j 20) (write-char (code-char (+ 97 (mod (+ i j) 26))) o)))))
      (incf n (length (with-output-to-string (o)
                        (write-string "abc" o) (write-string "defgh" o) (write-char #\! o)))))
    n))
