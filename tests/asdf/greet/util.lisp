(defpackage :greet-util (:use :cl) (:export #:shout))
(in-package :greet-util)
(defmacro shout (x) `(string-upcase ,x))
