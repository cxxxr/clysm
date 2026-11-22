(in-package :preinit-demo)
(incf *initializations* 10)
(js:export "answer" #'result)
(js:export "initializations" (lambda () *initializations*))
;; JS objects and library metadata belong after the initialization boundary.
(js:export "host" (lambda () (js:get (js:global "Math") "PI")))
(js:export "change" (lambda (n) (setf (slot-value *box* 'value) n)))
