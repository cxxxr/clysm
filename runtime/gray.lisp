;;;; gray.lisp — Gray streams (the CLtL2 "Gray proposal", as in SBCL's SB-GRAY)
;;;;
;;;; A Gray stream is an instance of a subclass of FUNDAMENTAL-STREAM, a
;;;; standard class that also inherits the STREAM structure (so STREAMP and
;;;; (TYPEP x 'STREAM) hold).  Its KIND slot is :GRAY, and every stream
;;;; function in stream.lisp hands such streams to the generic functions
;;;; below.  CLOSE, OPEN-STREAM-P, INPUT-STREAM-P, OUTPUT-STREAM-P and
;;;; STREAM-ELEMENT-TYPE are generic functions.  As extensions (those of
;;;; trivial-gray-streams), READ-SEQUENCE / WRITE-SEQUENCE call
;;;; STREAM-READ-SEQUENCE / STREAM-WRITE-SEQUENCE (STREAM SEQ START END &KEY)
;;;; and FILE-POSITION calls STREAM-FILE-POSITION / (SETF STREAM-FILE-POSITION).

;;; ------------------------------------------------------------
;;; the CL stream functions that are generic
;;; ------------------------------------------------------------

(defgeneric close (stream &key abort))
(defmethod close ((stream stream) &key abort) (%native-close stream :abort abort))
(defmethod close ((stream t) &key abort)
  (declare (ignore abort))
  (%type-error stream 'stream))

(defgeneric open-stream-p (stream))
(defmethod open-stream-p ((stream stream)) (%native-open-stream-p stream))
(defmethod open-stream-p ((stream t)) (%type-error stream 'stream))

(defgeneric input-stream-p (stream))
(defmethod input-stream-p ((stream stream)) (%native-input-stream-p stream))
(defmethod input-stream-p ((stream t)) (%type-error stream 'stream))

(defgeneric output-stream-p (stream))
(defmethod output-stream-p ((stream stream)) (%native-output-stream-p stream))
(defmethod output-stream-p ((stream t)) (%type-error stream 'stream))

(defgeneric stream-element-type (stream))
(defmethod stream-element-type ((stream stream)) (%native-stream-element-type stream))
(defmethod stream-element-type ((stream t)) (%type-error stream 'stream))

;;; ------------------------------------------------------------
;;; classes
;;; ------------------------------------------------------------

(defclass fundamental-stream (standard-object stream)
  ;; the STREAM structure slots that stream.lisp reads
  ((kind :initform :gray)
   (open :initform t)
   (unread :initform nil)
   (column :initform 0)
   (streams :initform nil)
   (direction :initform :io)
   (elt-type :initform t)))

(defclass fundamental-input-stream (fundamental-stream) ())
(defclass fundamental-output-stream (fundamental-stream) ())
(defclass fundamental-character-stream (fundamental-stream) ())
(defclass fundamental-binary-stream (fundamental-stream) ())
(defclass fundamental-character-input-stream (fundamental-input-stream fundamental-character-stream) ())
(defclass fundamental-character-output-stream (fundamental-output-stream fundamental-character-stream) ())
(defclass fundamental-binary-input-stream (fundamental-input-stream fundamental-binary-stream) ())
(defclass fundamental-binary-output-stream (fundamental-output-stream fundamental-binary-stream) ())

(defmethod open-stream-p ((stream fundamental-stream)) (stream-open stream))
(defmethod close ((stream fundamental-stream) &key abort)
  (declare (ignore abort))
  (prog1 (stream-open stream) (setf (stream-open stream) nil (stream-fast stream) nil (stream-fast-out stream) nil)))
(defmethod input-stream-p ((stream fundamental-stream)) nil)
(defmethod input-stream-p ((stream fundamental-input-stream)) t)
(defmethod output-stream-p ((stream fundamental-stream)) nil)
(defmethod output-stream-p ((stream fundamental-output-stream)) t)
(defmethod stream-element-type ((stream fundamental-stream)) t)
(defmethod stream-element-type ((stream fundamental-character-stream)) 'character)
(defmethod stream-element-type ((stream fundamental-binary-stream)) '(unsigned-byte 8))

;;; ------------------------------------------------------------
;;; character input
;;; ------------------------------------------------------------

(defun %gray-no-method (fn stream)
  (error 'simple-error
         :format-control "~S is not implemented for ~S"
         :format-arguments (list fn stream)))

(defgeneric stream-read-char (stream))
(defmethod stream-read-char ((stream fundamental-stream))
  (%gray-no-method 'stream-read-char stream))

(defgeneric stream-unread-char (stream character))
(defmethod stream-unread-char ((stream fundamental-stream) character)
  (declare (ignore character))
  (%gray-no-method 'stream-unread-char stream))

(defgeneric stream-read-char-no-hang (stream))
(defmethod stream-read-char-no-hang ((stream fundamental-input-stream))
  (stream-read-char stream))

(defgeneric stream-peek-char (stream))
(defmethod stream-peek-char ((stream fundamental-input-stream))
  (let ((c (stream-read-char stream)))
    (unless (eq c :eof) (stream-unread-char stream c))
    c))

(defgeneric stream-listen (stream))
(defmethod stream-listen ((stream fundamental-input-stream))
  (let ((c (stream-read-char-no-hang stream)))
    (when (characterp c)
      (stream-unread-char stream c)
      t)))

(defgeneric stream-read-line (stream))
(defmethod stream-read-line ((stream fundamental-input-stream))
  (let ((buf (make-array 80 :element-type 'character :fill-pointer 0 :adjustable t)))
    (loop
      (let ((c (stream-read-char stream)))
        (cond ((eq c :eof) (return (values (subseq buf 0) t)))
              ((char= c #\Newline) (return (values (subseq buf 0) nil)))
              (t (vector-push-extend c buf)))))))

(defgeneric stream-clear-input (stream))
(defmethod stream-clear-input ((stream fundamental-input-stream)) nil)

;;; ------------------------------------------------------------
;;; character output
;;; ------------------------------------------------------------

(defgeneric stream-write-char (stream character))
(defmethod stream-write-char ((stream fundamental-stream) character)
  (declare (ignore character))
  (%gray-no-method 'stream-write-char stream))

(defgeneric stream-line-column (stream))
(defmethod stream-line-column ((stream fundamental-output-stream)) nil)

(defgeneric stream-line-length (stream))
(defmethod stream-line-length ((stream fundamental-output-stream)) nil)

(defgeneric stream-start-line-p (stream))
(defmethod stream-start-line-p ((stream fundamental-output-stream))
  (eql (stream-line-column stream) 0))

(defgeneric stream-write-string (stream string &optional start end))
(defmethod stream-write-string ((stream fundamental-output-stream) string &optional (start 0) end)
  (let ((end (or end (length string))))
    (do ((i start (+ i 1))) ((>= i end))
      (stream-write-char stream (char string i)))
    string))

(defgeneric stream-terpri (stream))
(defmethod stream-terpri ((stream fundamental-output-stream))
  (stream-write-char stream #\Newline)
  nil)

(defgeneric stream-fresh-line (stream))
(defmethod stream-fresh-line ((stream fundamental-output-stream))
  (unless (stream-start-line-p stream)
    (stream-terpri stream)
    t))

(defgeneric stream-finish-output (stream))
(defmethod stream-finish-output ((stream fundamental-output-stream)) nil)

(defgeneric stream-force-output (stream))
(defmethod stream-force-output ((stream fundamental-output-stream)) nil)

(defgeneric stream-clear-output (stream))
(defmethod stream-clear-output ((stream fundamental-output-stream)) nil)

(defgeneric stream-advance-to-column (stream column))
(defmethod stream-advance-to-column ((stream fundamental-output-stream) column)
  (let ((current (stream-line-column stream)))
    (when current
      (dotimes (i (- column current)) (stream-write-char stream #\Space))
      t)))

;;; the methods of stream-finish-output and friends on input streams too
(defmethod stream-finish-output ((stream fundamental-stream)) nil)
(defmethod stream-force-output ((stream fundamental-stream)) nil)
(defmethod stream-clear-output ((stream fundamental-stream)) nil)
(defmethod stream-line-column ((stream fundamental-stream)) nil)

;;; ------------------------------------------------------------
;;; binary streams
;;; ------------------------------------------------------------

(defgeneric stream-read-byte (stream))
(defmethod stream-read-byte ((stream fundamental-stream))
  (%gray-no-method 'stream-read-byte stream))

(defgeneric stream-write-byte (stream integer))
(defmethod stream-write-byte ((stream fundamental-stream) integer)
  (declare (ignore integer))
  (%gray-no-method 'stream-write-byte stream))

;;; ------------------------------------------------------------
;;; sequences and positions (extensions, as in trivial-gray-streams)
;;; ------------------------------------------------------------

(defun %gray-binary-p (stream)
  (let ((et (stream-element-type stream)))
    (and et (not (eq et t)) (not (subtypep et 'character)))))

(defgeneric stream-read-sequence (stream sequence start end &key &allow-other-keys))
(defmethod stream-read-sequence ((stream fundamental-input-stream) sequence start end &key)
  (let ((end (or end (length sequence))) (binary (%gray-binary-p stream)))
    (do ((i start (+ i 1))) ((>= i end) end)
      (let ((x (if binary (stream-read-byte stream) (stream-read-char stream))))
        (when (eq x :eof) (return i))
        (setf (elt sequence i) x)))))

(defgeneric stream-write-sequence (stream sequence start end &key &allow-other-keys))
(defmethod stream-write-sequence ((stream fundamental-output-stream) sequence start end &key)
  (let ((end (or end (length sequence))) (binary (%gray-binary-p stream)))
    (if (and (not binary) (stringp sequence))
        (stream-write-string stream sequence start end)
        (do ((i start (+ i 1))) ((>= i end))
          (let ((x (elt sequence i)))
            (if binary (stream-write-byte stream x) (stream-write-char stream x)))))
    sequence))

(defgeneric stream-file-position (stream))
(defmethod stream-file-position ((stream t)) nil)
(defgeneric (setf stream-file-position) (position stream))
(defmethod (setf stream-file-position) (position (stream t))
  (declare (ignore position))
  nil)

;;; ------------------------------------------------------------
;;; the GRAY package
;;; ------------------------------------------------------------

(defpackage "GRAY"
  (:use)
  (:import-from "SYSTEM"
   "FUNDAMENTAL-STREAM" "FUNDAMENTAL-INPUT-STREAM" "FUNDAMENTAL-OUTPUT-STREAM"
   "FUNDAMENTAL-CHARACTER-STREAM" "FUNDAMENTAL-BINARY-STREAM"
   "FUNDAMENTAL-CHARACTER-INPUT-STREAM" "FUNDAMENTAL-CHARACTER-OUTPUT-STREAM"
   "FUNDAMENTAL-BINARY-INPUT-STREAM" "FUNDAMENTAL-BINARY-OUTPUT-STREAM"
   "STREAM-READ-CHAR" "STREAM-UNREAD-CHAR" "STREAM-READ-CHAR-NO-HANG" "STREAM-PEEK-CHAR"
   "STREAM-LISTEN" "STREAM-READ-LINE" "STREAM-CLEAR-INPUT" "STREAM-WRITE-CHAR"
   "STREAM-LINE-COLUMN" "STREAM-LINE-LENGTH" "STREAM-START-LINE-P" "STREAM-WRITE-STRING"
   "STREAM-TERPRI" "STREAM-FRESH-LINE" "STREAM-FINISH-OUTPUT" "STREAM-FORCE-OUTPUT"
   "STREAM-CLEAR-OUTPUT" "STREAM-ADVANCE-TO-COLUMN" "STREAM-READ-BYTE" "STREAM-WRITE-BYTE"
   "STREAM-READ-SEQUENCE" "STREAM-WRITE-SEQUENCE" "STREAM-FILE-POSITION")
  (:export
   "FUNDAMENTAL-STREAM" "FUNDAMENTAL-INPUT-STREAM" "FUNDAMENTAL-OUTPUT-STREAM"
   "FUNDAMENTAL-CHARACTER-STREAM" "FUNDAMENTAL-BINARY-STREAM"
   "FUNDAMENTAL-CHARACTER-INPUT-STREAM" "FUNDAMENTAL-CHARACTER-OUTPUT-STREAM"
   "FUNDAMENTAL-BINARY-INPUT-STREAM" "FUNDAMENTAL-BINARY-OUTPUT-STREAM"
   "STREAM-READ-CHAR" "STREAM-UNREAD-CHAR" "STREAM-READ-CHAR-NO-HANG" "STREAM-PEEK-CHAR"
   "STREAM-LISTEN" "STREAM-READ-LINE" "STREAM-CLEAR-INPUT" "STREAM-WRITE-CHAR"
   "STREAM-LINE-COLUMN" "STREAM-LINE-LENGTH" "STREAM-START-LINE-P" "STREAM-WRITE-STRING"
   "STREAM-TERPRI" "STREAM-FRESH-LINE" "STREAM-FINISH-OUTPUT" "STREAM-FORCE-OUTPUT"
   "STREAM-CLEAR-OUTPUT" "STREAM-ADVANCE-TO-COLUMN" "STREAM-READ-BYTE" "STREAM-WRITE-BYTE"
   "STREAM-READ-SEQUENCE" "STREAM-WRITE-SEQUENCE" "STREAM-FILE-POSITION"))
