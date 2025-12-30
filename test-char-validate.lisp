(asdf:load-system :clysm :silent t)
(in-package :clysm)
(format t "~%Validating character function Wasm output...~%")

(defun test-validate (name form)
  (handler-case
    (let* ((bytes (clysm:compile-to-wasm form))
           (temp-file (format nil "/tmp/test-char-~a.wasm" name)))
      (with-open-file (stream temp-file :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
        (write-sequence bytes stream))
      (format t "~a: ~a bytes written to ~a~%" name (length bytes) temp-file))
    (error (e) (format t "~a: ERROR ~a~%" name e))))

(test-validate "graphic-char-p" '(graphic-char-p #\A))
(test-validate "standard-char-p" '(standard-char-p #\A))
(test-validate "both-case-p" '(both-case-p #\A))
(test-validate "char-name" '(char-name #\Space))
(test-validate "name-char" '(name-char "Space"))
(test-validate "digit-char" '(digit-char 5))
(test-validate "char-int" '(char-int #\A))

(format t "~%Now validate with wasm-tools...~%")
