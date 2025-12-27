(ql:quickload :clysm/tests :silent t)

;; Install the macros
(clysm/lib/macros:install-standard-macros
 (clysm/compiler/transform/macro:global-macro-registry))

(format t "~%=== DESTRUCTURING-BIND MACRO TESTS ===~%~%")

;; Test macro expansion
(format t "1. Basic (a b c):~%")
(let ((result (eval (clysm/compiler/transform/macro:macroexpand-1
                     '(destructuring-bind (a b c) '(1 2 3) (list a b c))))))
  (format t "   Result: ~S~%" result)
  (format t "   Pass: ~A~%" (equal result '(1 2 3))))

(format t "~%2. Nested ((a b) c):~%")
(let ((result (eval (clysm/compiler/transform/macro:macroexpand-1
                     '(destructuring-bind ((a b) c) '((1 2) 3) (list a b c))))))
  (format t "   Result: ~S~%" result)
  (format t "   Pass: ~A~%" (equal result '(1 2 3))))

(format t "~%3. Optional (a &optional (b 10)):~%")
(let ((result (eval (clysm/compiler/transform/macro:macroexpand-1
                     '(destructuring-bind (a &optional (b 10)) '(1) (list a b))))))
  (format t "   Result: ~S~%" result)
  (format t "   Pass: ~A~%" (equal result '(1 10))))

(format t "~%4. Rest (a &rest r):~%")
(let ((result (eval (clysm/compiler/transform/macro:macroexpand-1
                     '(destructuring-bind (a &rest r) '(1 2 3 4) (list a r))))))
  (format t "   Result: ~S~%" result)
  (format t "   Pass: ~A~%" (equal result '(1 (2 3 4)))))

(format t "~%5. Key (&key x y):~%")
(let ((result (eval (clysm/compiler/transform/macro:macroexpand-1
                     '(destructuring-bind (&key x y) '(:x 1 :y 2) (list x y))))))
  (format t "   Result: ~S~%" result)
  (format t "   Pass: ~A~%" (equal result '(1 2))))

(format t "~%6. Whole (&whole w a b):~%")
(let ((result (eval (clysm/compiler/transform/macro:macroexpand-1
                     '(destructuring-bind (&whole w a b) '(1 2) (list w a b))))))
  (format t "   Result: ~S~%" result)
  (format t "   Pass: ~A~%" (equal result '((1 2) 1 2))))

(format t "~%7. Body (a &body b):~%")
(let ((result (eval (clysm/compiler/transform/macro:macroexpand-1
                     '(destructuring-bind (a &body b) '(1 2 3 4) (list a b))))))
  (format t "   Result: ~S~%" result)
  (format t "   Pass: ~A~%" (equal result '(1 (2 3 4)))))

(format t "~%8. Supplied-p (a &optional (b nil b-p)):~%")
(let ((result (eval (clysm/compiler/transform/macro:macroexpand-1
                     '(destructuring-bind (a &optional (b nil b-p)) '(1 2) (list a b b-p))))))
  (format t "   Result: ~S~%" result)
  (format t "   Pass: ~A~%" (equal result '(1 2 T))))

;; Additional edge case tests
(format t "~%9. Allow-other-keys (&key x &allow-other-keys):~%")
(let ((result (eval (clysm/compiler/transform/macro:macroexpand-1
                     '(destructuring-bind (&key x &allow-other-keys) '(:x 1 :y 2 :z 3) (list x))))))
  (format t "   Result: ~S~%" result)
  (format t "   Pass: ~A~%" (equal result '(1))))

(format t "~%10. Key with default ((&key (x 10))):~%")
(let ((result (eval (clysm/compiler/transform/macro:macroexpand-1
                     '(destructuring-bind (&key (x 10)) '() (list x))))))
  (format t "   Result: ~S~%" result)
  (format t "   Pass: ~A~%" (equal result '(10))))

(format t "~%11. Combined optional + rest (a &optional b &rest c):~%")
(let ((result (eval (clysm/compiler/transform/macro:macroexpand-1
                     '(destructuring-bind (a &optional b &rest c) '(1 2 3 4 5) (list a b c))))))
  (format t "   Result: ~S~%" result)
  (format t "   Pass: ~A~%" (equal result '(1 2 (3 4 5)))))

(format t "~%12. Key with alternate keyword (((:my-x x))):~%")
(let ((result (eval (clysm/compiler/transform/macro:macroexpand-1
                     '(destructuring-bind (&key ((:my-x x) 100)) '(:my-x 42) (list x))))))
  (format t "   Result: ~S~%" result)
  (format t "   Pass: ~A~%" (equal result '(42))))

(format t "~%13. Deep nesting (((a b) (c d)) e):~%")
(let ((result (eval (clysm/compiler/transform/macro:macroexpand-1
                     '(destructuring-bind (((a b) (c d)) e) '(((1 2) (3 4)) 5) (list a b c d e))))))
  (format t "   Result: ~S~%" result)
  (format t "   Pass: ~A~%" (equal result '(1 2 3 4 5))))

(format t "~%14. Optional with nested (a &optional ((x y) '(10 20))):~%")
(let ((result (eval (clysm/compiler/transform/macro:macroexpand-1
                     '(destructuring-bind (a &optional ((x y) '(10 20))) '(1) (list a x y))))))
  (format t "   Result: ~S~%" result)
  (format t "   Pass: ~A~%" (equal result '(1 10 20))))

(format t "~%15. Key supplied-p (&key (x nil x-p)):~%")
(let ((result (eval (clysm/compiler/transform/macro:macroexpand-1
                     '(destructuring-bind (&key (x nil x-p)) '(:x 5) (list x x-p))))))
  (format t "   Result: ~S~%" result)
  (format t "   Pass: ~A~%" (equal result '(5 T))))

(format t "~%16. Key supplied-p not supplied (&key (x nil x-p)):~%")
(let ((result (eval (clysm/compiler/transform/macro:macroexpand-1
                     '(destructuring-bind (&key (x nil x-p)) '() (list x x-p))))))
  (format t "   Result: ~S~%" result)
  (format t "   Pass: ~A~%" (equal result '(NIL NIL))))

;; Phase 8: Polish - Edge cases
(format t "~%17. 5+ levels of nesting:~%")
(let ((result (eval (clysm/compiler/transform/macro:macroexpand-1
                     '(destructuring-bind ((((((a))))) b) '((((((1))))) 2) (list a b))))))
  (format t "   Result: ~S~%" result)
  (format t "   Pass: ~A~%" (equal result '(1 2))))

(format t "~%18. Dotted list (a b . c):~%")
(let ((result (eval (clysm/compiler/transform/macro:macroexpand-1
                     '(destructuring-bind (a b . c) '(1 2 3 4 5) (list a b c))))))
  (format t "   Result: ~S~%" result)
  (format t "   Pass: ~A~%" (equal result '(1 2 (3 4 5)))))

(format t "~%19. Error on too few elements:~%")
(let ((passed nil))
  (handler-case
      (eval (clysm/compiler/transform/macro:macroexpand-1
             '(destructuring-bind (a b c) '(1 2) (list a b c))))
    (program-error (e)
      (declare (ignore e))
      (setf passed t)))
  (format t "   Pass: ~A~%" passed))

(format t "~%20. Error on too many elements (no &rest):~%")
(let ((passed nil))
  (handler-case
      (eval (clysm/compiler/transform/macro:macroexpand-1
             '(destructuring-bind (a b) '(1 2 3 4) (list a b))))
    (program-error (e)
      (declare (ignore e))
      (setf passed t)))
  (format t "   Pass: ~A~%" passed))

(format t "~%=== ALL TESTS COMPLETE ===~%")
