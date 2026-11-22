;;; Printing with the standard syntax must work even when a shaken program
;;; has no reader and never refers to *READTABLE*.
(let ((*print-pretty* nil))
  (dolist (style '(:upcase :downcase :capitalize))
    (let ((*print-case* style))
      (dolist (name '("HELLO" "hello" "MiXeD" "A B" "" "123" "." ".."
                      "#FOO" "A:B" "A|B" "A\\B" "λ" "HELLO-WORLD"))
        (let ((symbol (make-symbol name)))
          (write-line (prin1-to-string symbol))
          (write-line (princ-to-string symbol))
          (write-line (write-to-string symbol :readably t)))))))
