;;; The printer still uses a dynamically bound or modified readtable when
;;; the program names it; the reader macro functions must remain callable.
;;; The .expected keeps Clysm's existing extra escaping of custom macro
;;; characters (SBCL prints those without escapes).
(let ((*readtable* (copy-readtable nil)) (*print-pretty* nil))
  (set-macro-character #\! (lambda (s c) (declare (ignore s c)) :mark))
  (set-macro-character #\中 (lambda (s c) (declare (ignore s c)) :unicode) t)
  (dolist (name '("A!B" "中ABC" "ABC"))
    (write-line (prin1-to-string (make-symbol name))))
  (write-line (prin1-to-string (read-from-string "!")))
  (write-line (prin1-to-string (read-from-string "中")))
  (set-syntax-from-char #\! #\A)
  (write-line (prin1-to-string (make-symbol "A!B")))
  (dolist (mode '(:upcase :downcase :preserve :invert))
    (setf (readtable-case *readtable*) mode)
    (dolist (name '("ABC" "abc" "AbC"))
      (write-line (prin1-to-string (make-symbol name)))
      (write-line (princ-to-string (make-symbol name)))
      (write-line (write-to-string (make-symbol name) :readably t)))))
