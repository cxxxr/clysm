// The programs of the playground's menu (site/index.html).  Each is compiled
// to WebAssembly in the page, one top-level form at a time.

export const samples = [
  {
    id: 'numbers',
    title: 'Numbers',
    note: 'Integers have no size limit, division of integers gives exact ratios, '
      + 'and the square root of a negative number is a complex number.',
    code: `;; Integers of any size, exact ratios and complex numbers.

(defun factorial (n)
  (if (< n 2)
      1
      (* n (factorial (- n 1)))))

(format t "100! = ~D~%~%" (factorial 100))
(format t "2^200 / 3^50 = ~D~%" (/ (expt 2 200) (expt 3 50)))
(format t "1/3 + 1/6 = ~A~%" (+ 1/3 1/6))
(format t "(sqrt -4) = ~A~%" (sqrt -4))
(format t "pi = ~,15F~%" pi)
(format t "2026 is ~R, or ~@R~%" 2026 2026)

(list (gcd 1071 462) (isqrt (expt 10 41)) (rationalize 0.75))
`,
  },
  {
    id: 'clos',
    title: 'CLOS',
    note: 'Classes, generic functions with multiple dispatch, an :around method '
      + 'and a print-object method. Generic functions are compiled to dispatch code in WebAssembly.',
    code: `;; Classes and generic functions.

(defclass shape () ())

(defclass circle (shape)
  ((radius :initarg :radius :reader radius)))

(defclass rect (shape)
  ((width :initarg :width :reader width)
   (height :initarg :height :reader height)))

(defgeneric area (shape))

(defmethod area ((c circle))
  (* pi (radius c) (radius c)))

(defmethod area ((r rect))
  (* (width r) (height r)))

;; runs around the primary method of every shape
(defmethod area :around ((s shape))
  (let ((a (call-next-method)))
    (format t "~&area of ~A: ~,2F~%" s a)
    a))

(defmethod print-object ((c circle) stream)
  (format stream "#<circle r=~A>" (radius c)))

(defmethod print-object ((r rect) stream)
  (format stream "#<rect ~Ax~A>" (width r) (height r)))

;; dispatch on both arguments
(defgeneric overlap (a b))
(defmethod overlap ((a circle) (b circle)) "two circles")
(defmethod overlap ((a shape) (b rect)) "something and a rectangle")
(defmethod overlap ((a shape) (b shape)) "two shapes")

(let ((shapes (list (make-instance 'circle :radius 2)
                    (make-instance 'rect :width 3 :height 4))))
  (format t "total: ~,2F~%~%" (reduce #'+ shapes :key #'area))
  (loop for a in shapes
        append (loop for b in shapes collect (overlap a b))))
`,
  },
  {
    id: 'conditions',
    title: 'Conditions and restarts',
    note: 'A handler chooses how to go on from an error without unwinding the stack: '
      + 'the function that signalled it offers restarts, and the handler invokes one.',
    code: `;; The condition system: handlers choose a restart that the code
;; signalling the error offers, and the computation goes on from there.

(define-condition bad-entry (error)
  ((line :initarg :line :reader bad-entry-line))
  (:report (lambda (c stream)
             (format stream "cannot parse ~S" (bad-entry-line c)))))

(defun parse-entry (line)
  (restart-case
      (or (parse-integer line :junk-allowed t)
          (error 'bad-entry :line line))
    (use-value (value)
      :report "Use another value."
      value)
    (skip-entry ()
      :report "Leave the entry out."
      nil)))

(defun parse-log (lines)
  (remove nil (mapcar #'parse-entry lines)))

(defparameter *lines* '("10" "20" "oops" "30" "n/a" "40"))

;; skip what cannot be parsed
(format t "skipped: ~A~%"
        (handler-bind ((bad-entry (lambda (c)
                                    (format t "  ~A~%" c)
                                    (invoke-restart 'skip-entry))))
          (parse-log *lines*)))

;; or count it as zero
(format t "as zero: ~A~%"
        (handler-bind ((bad-entry (lambda (c)
                                    (declare (ignore c))
                                    (invoke-restart 'use-value 0))))
          (parse-log *lines*)))

;; without a handler the error reaches the top level
(parse-log *lines*)
`,
  },
  {
    id: 'macros',
    title: 'Macros',
    note: 'Macros are Lisp functions that write code. Their expansion is compiled like the '
      + 'rest of the program; macroexpand-1 shows it.',
    code: `;; Macros write code at compile time.

(defmacro while (test &body body)
  \`(loop (unless ,test (return))
         ,@body))

(defmacro with-timing ((label) &body body)
  (let ((start (gensym "START")))
    \`(let ((,start (get-internal-real-time)))
       (multiple-value-prog1 (progn ,@body)
         (format t "~&~A took ~D ms~%" ,label
                 (round (* 1000 (- (get-internal-real-time) ,start))
                        internal-time-units-per-second))))))

(defun collatz-steps (n)
  (let ((steps 0))
    (while (/= n 1)
      (setf n (if (evenp n) (/ n 2) (+ (* 3 n) 1)))
      (incf steps))
    steps))

(with-timing ("Collatz below 100,000")
  (let ((best 1) (most 0))
    (loop for n from 1 below 100000
          for steps = (collatz-steps n)
          when (> steps most)
            do (setf best n most steps))
    (format t "~:D takes the most steps: ~D~%" best most)))

;; what the compiler gets
(macroexpand-1 '(while (plusp x) (decf x)))
`,
  },
  {
    id: 'loop-format',
    title: 'LOOP and FORMAT',
    note: 'LOOP iterates, collects and sums; FORMAT writes tables, plurals, lists and numbers in words.',
    code: `;; LOOP and FORMAT.

(defun primep (n)
  (and (> n 1)
       (loop for d from 2 to (isqrt n)
             never (zerop (mod n d)))))

(defparameter *primes*
  (loop for n from 2
        when (primep n) collect n into primes
        when (= (length primes) 25) return primes))

(format t "~D primes: ~{~A~^, ~}~%~%" (length *primes*) *primes*)

;; a table: ~A pads, ~:D groups digits
(format t "~&~5A ~12@A ~18@A~%" "n" "n^3" "2^n")
(loop for n in '(1 10 20 30 40)
      do (format t "~5A ~12:D ~18:D~%" n (expt n 3) (expt 2 n)))

;; plurals and conditional text
(dolist (n '(0 1 3))
  (format t "~&~D file~:P ~:[found~;left~]~%" n (oddp n)))

(loop for x in '(3 1 4 1 5 9 2 6)
      maximize x into most
      sum x into total
      finally (return (list :max most :sum total)))
`,
  },
  {
    id: 'closures',
    title: 'Closures and tail calls',
    note: 'Functions are values that keep their variables. Calls in tail position are '
      + 'WebAssembly tail calls (return_call), so this loop of ten million calls uses no stack.',
    code: `;; Closures, higher-order functions and tail calls.

(defun make-counter (&optional (step 1))
  (let ((n 0))
    (lambda () (incf n step))))

(let ((by-one (make-counter))
      (by-ten (make-counter 10)))
  (dotimes (i 3) (funcall by-one) (funcall by-ten))
  (format t "counters: ~D ~D~%" (funcall by-one) (funcall by-ten)))

(defun compose (&rest fns)
  (lambda (x) (reduce #'funcall fns :from-end t :initial-value x)))

(format t "composed: ~A~%"
        (mapcar (compose #'1+ (lambda (x) (* x x))) '(1 2 3 4)))

;; a loop written as a tail call: ten million calls, no stack used
(defun count-down (n acc)
  (if (zerop n)
      acc
      (count-down (- n 1) (+ acc 1))))

(count-down 10000000 0)
`,
  },
  {
    id: 'speed',
    title: 'Speed',
    note: 'Each form is compiled to WebAssembly before it runs, so this is native code. '
      + 'The time under the output is that of the whole evaluation, compilation included.',
    code: `;; Compiled code: a recursive Fibonacci and the Takeuchi function.

(defun fib (n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(defun tak (x y z)
  (if (not (< y x))
      z
      (tak (tak (- x 1) y z)
           (tak (- y 1) z x)
           (tak (- z 1) x y))))

(defun ms (thunk)
  (let ((start (get-internal-real-time)))
    (values (funcall thunk)
            (round (* 1000 (- (get-internal-real-time) start))
                   internal-time-units-per-second))))

(defparameter *n* 32)

(multiple-value-bind (value ms) (ms (lambda () (fib *n*)))
  (format t "(fib ~D) = ~D in ~D ms~%" *n* value ms))

(multiple-value-bind (value ms) (ms (lambda () (tak 24 16 8)))
  (format t "(tak 24 16 8) = ~D in ~D ms~%" value ms))

;; 200,000 conses, collected by the host's garbage collector
(length (loop for i below 200000 collect (cons i i)))
`,
  },
  {
    id: 'canvas',
    title: 'Drawing',
    note: 'Lisp computes the Mandelbrot set into a vector of octets and hands it to the canvas '
      + 'through the JS package. playground:context is the 2D context of the canvas.',
    code: `;; The Mandelbrot set, computed in Lisp and drawn on a canvas.

(defun mandelbrot (width height &key (max-iter 80))
  (let ((pixels (make-array (* width height 4) :element-type '(unsigned-byte 8))))
    (dotimes (py height pixels)
      (dotimes (px width)
        (let* ((c (complex (- (* 3.0 (/ px width)) 2.2)
                           (- (* 2.0 (/ py height)) 1.0)))
               (z #c(0.0 0.0))
               (n (loop for i below max-iter
                        while (< (abs z) 2.0)
                        do (setf z (+ (* z z) c))
                        finally (return i)))
               (i (* 4 (+ px (* py width))))
               (s (/ n max-iter))
               (u (- 1 s)))
          ;; the points of the set (s = 1) are black
          (setf (aref pixels i) (floor (* 255 9 u s s s))
                (aref pixels (+ i 1)) (floor (* 255 15 u u s s))
                (aref pixels (+ i 2)) (floor (* 255 8.5 u u u s))
                (aref pixels (+ i 3)) 255))))))

(defun draw (pixels width height)
  (let* ((octets (js:from-octets pixels))
         (clamped (js:new (js:global "Uint8ClampedArray") (js:get octets "buffer")))
         (image (js:new (js:global "ImageData") clamped width height)))
    (js:call (playground:context) "putImageData" image 0 0)))

(let ((start (get-internal-real-time)))
  (draw (mandelbrot 480 320) 480 320)
  (format t "480x320 pixels in ~D ms~%"
          (round (* 1000 (- (get-internal-real-time) start))
                 internal-time-units-per-second)))
`,
  },
  {
    id: 'javascript',
    title: 'JavaScript',
    note: 'The JS package calls JavaScript and gives Lisp functions to it: here a Lisp closure '
      + 'is the comparator of Array.prototype.sort, and Intl formats numbers and dates.',
    code: `;; Calling JavaScript from Lisp, and Lisp from JavaScript.

(js:call (js:global "Math") "hypot" 3 4)

(let ((format (js:new (js:get (js:global "Intl") "NumberFormat") "en-US"
                      (js:object "style" "currency" "currency" "EUR"))))
  (format t "~A~%" (js:call format "format" 1234567.891)))

(format t "~A~%" (js:call (js:new (js:global "Date") 0) "toISOString"))

;; a Lisp closure as the comparator of a JavaScript sort
(let ((calls 0)
      (array (js:array 5 3 9 1 7)))
  (js:call array "sort" (lambda (a b) (incf calls) (- a b)))
  (format t "sorted ~A with ~D calls of a Lisp function~%"
          (js:call array "join" " ") calls))

;; JSON both ways
(let ((data (js:call (js:global "JSON") "parse"
                     "{\\"name\\": \\"Clysm\\", \\"target\\": \\"wasm-gc\\"}")))
  (format t "~A compiles to ~A~%" (js:get data "name") (js:get data "target")))

(js:funcall (js:eval "(f, x) => f(f(x))") (lambda (x) (* x 10)) 4)
`,
  },
  {
    id: 'compiler',
    title: 'The compiler',
    note: 'The compiler runs in the page: compile makes a WebAssembly module of a lambda '
      + 'and links it to the running image. A redefined function is used at once by its callers.',
    code: `;; The compiler is in the image: compile and eval make WebAssembly
;; modules in the page and link them with the running program.

(defparameter *square* (compile nil '(lambda (x) (* x x))))

(format t "~A~%" *square*)
(format t "(funcall *square* 12) = ~D~%" (funcall *square* 12))

;; code made at run time
(defun make-polynomial (&rest coefficients)
  (compile nil \`(lambda (x)
                  (+ ,@(loop for c in (reverse coefficients)
                             for power from 0
                             collect \`(* ,c (expt x ,power)))))))

(let ((p (make-polynomial 1 -3 2)))     ; x^2 - 3x + 2
  (format t "p(x) for x from 0 to 4: ~A~%"
          (loop for x from 0 to 4 collect (funcall p x))))

;; redefining a function: its callers use the new definition
(defun greet () "hello")
(defun greet-twice () (list (greet) (greet)))
(format t "~A~%" (greet-twice))
(defun greet () "bonjour")
(format t "~A~%" (greet-twice))

(list (lisp-implementation-type) (machine-type))
`,
  },
];
