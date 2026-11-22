(js:set (js:global "globalThis") "compressionStarts"
        (1+ (or (js:get (js:global "globalThis") "compressionStarts") 0)))
(defvar *count* 0)
(js:export "next" (lambda () (incf *count*)))
(js:export "text" (lambda (x) (concatenate 'string "prefix:" x)))
(defun compression-failure () (error "compressed library failure"))
(js:export "fail" #'compression-failure)
(js:export "other" (lambda () 42) :entry "other")
