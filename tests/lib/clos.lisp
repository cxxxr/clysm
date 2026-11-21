;;; A library compiled with --shake (tests/lib/run.sh, tests/lib/clos.mjs):
;;; the classes it defines, a Gray stream among them, must still dispatch
;;; to the methods they inherit from the runtime's, though the methods of
;;; classes that nothing can make an instance of are left out of a shaken
;;; module (src/preinit.lisp).

(defclass shape () ((name :initarg :name :reader shape-name)))
(defclass circle (shape) ((r :initarg :r)))
(defclass square (shape) ((side :initarg :side)))

(defgeneric area (shape))
(defmethod area ((c circle)) (* 3 (slot-value c 'r) (slot-value c 'r)))
(defmethod area ((s square)) (* (slot-value s 'side) (slot-value s 'side)))

(defmethod print-object ((s shape) stream)
  (print-unreadable-object (s stream :type t)
    (format stream "~A" (shape-name s))))

;; a Gray stream that counts what is written to it: WRITE-STRING, FRESH-LINE
;; and FORMAT go through the methods of FUNDAMENTAL-CHARACTER-OUTPUT-STREAM
(defclass counting-stream (gray:fundamental-character-output-stream)
  ((count :initform 0 :accessor counted)
   (column :initform 0)
   (text :initform (make-string-output-stream))))
(defmethod gray:stream-write-char ((s counting-stream) c)
  (incf (counted s))
  (setf (slot-value s 'column) (if (char= c #\Newline) 0 (1+ (slot-value s 'column))))
  (write-char c (slot-value s 'text))
  c)
(defmethod gray:stream-line-column ((s counting-stream)) (slot-value s 'column))

(defun shapes ()
  (let ((out (make-instance 'counting-stream)))
    (format out "~A ~A ~D ~D~%" (make-instance 'circle :name "c" :r 2) (make-instance 'square :name "s" :side 3)
            (area (make-instance 'circle :name "x" :r 1)) (area (make-instance 'square :name "y" :side 4)))
    (write-string "ab" out)
    (fresh-line out)
    (fresh-line out)
    (format nil "~D ~S ~A ~A" (counted out) (get-output-stream-string (slot-value out 'text))
            (streamp out) (output-stream-p out))))

(js:export "shapes" #'shapes)
(js:export "noArea" (lambda () (handler-case (progn (area 42) "no error")
                                 (error (e) (princ-to-string (type-of e))))))
