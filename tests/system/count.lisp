(in-package :wordcount)
(defun count-words (text)
  (length (remove "" (split-sequence:split-sequence #\Space text) :test #'string=)))
(js:export "countWords" #'count-words :type "(text: string) => number")
