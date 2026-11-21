;;; The module of one export (bin/clysm library --shake): tests/lib/run.sh
;;; fails when it is larger than tests/lib/one.max-bytes, so that what the
;;; runtime adds to every library does not grow unnoticed (issue #30).
;;; bin/clysm library --shake-why REPORT says what keeps each function.
(js:export "one" (lambda () 1))
