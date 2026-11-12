(defpackage :greet-test (:use :cl) (:export #:run))
(in-package :greet-test)
(defun run ()
  (format t "~&test: ~:[FAIL~;ok~]~%" (string= (greet:greet "wasm") "Hello, WASM!")))
