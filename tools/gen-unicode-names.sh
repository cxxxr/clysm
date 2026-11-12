#!/bin/sh
# Regenerate data/unicode-names.txt (hex code and name of every named character
# above U+00FF) from SBCL's Unicode database.  CHAR-NAME and NAME-CHAR read it
# the first time they need a name outside Latin-1.
cd "$(dirname "$0")/.." || exit 1
sbcl --noinform --non-interactive --eval '
(with-open-file (o "data/unicode-names.txt" :direction :output :if-exists :supersede)
  (loop for i from 256 below char-code-limit
        for c = (code-char i)
        for n = (and c (char-name c))
        when (and n (not (and (> (length n) 1) (char-equal (char n 0) #\U)
                              (every (lambda (x) (digit-char-p x 16)) (subseq n 1)))))
          do (format o "~X ~A~%" i n)))'
