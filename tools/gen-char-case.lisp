;;; Writes runtime/char-case.lisp: the case mappings of the characters that
;;; are not ASCII, as SBCL has them (Unicode's simple mappings of the
;;; characters with both cases).  Run it again when they are to follow a newer
;;; Unicode:
;;;
;;;   sbcl --script tools/gen-char-case.lisp
;;;
;;; A table is a vector of ranges, each four numbers: the first and the last
;;; code of the range, the step between the codes of it that have a mapping
;;; (1, or 2 where upper and lower case alternate: Ā ā Ă ă), and what is
;;; added to a code to get the other case.

(defun ranges (mapping)
  "The ranges of the codes from 128 on that MAPPING changes, in order."
  (let ((ranges '()) (start nil) (end nil) (step nil) (delta nil))
    (flet ((close-range ()
             (when start (push (list start end (or step 1) delta) ranges))
             (setf start nil)))
      (loop for code from 128 below char-code-limit
            do (let* ((char (code-char code))
                      (mapped (and char (char-code (funcall mapping char)))))
                 (when (and mapped (/= mapped code))
                   (let ((d (- mapped code)))
                     (cond ((and start (= d delta)
                                 (if step (= (- code end) step) (<= (- code end) 2)))
                            (unless step (setf step (- code end)))
                            (setf end code))
                           (t (close-range)
                              (setf start code end code step nil delta d)))))))
      (close-range))
    (nreverse ranges)))

(defun write-table (name ranges out)
  (format out "~%(defun ~A (n)~%  (+ n (%case-delta n '#(" name)
  (loop for (start end step delta) in ranges
        for i from 0
        do (format out "~:[ ~;~%    ~]~D ~D ~D ~D" (zerop (mod i 4)) start end step delta))
  (format out "))))~%"))

(with-open-file (out (merge-pathnames "../runtime/char-case.lisp" *load-truename*)
                     :direction :output :if-exists :supersede :external-format :utf-8)
  (format out ";;;; char-case.lisp — the case mappings of the characters that are not ASCII~%")
  (format out ";;;;~%")
  (format out ";;;; Written by tools/gen-char-case.lisp (which says what the tables are)~%")
  (format out ";;;; from ~A ~A: do not edit.~%"
          (lisp-implementation-type) (lisp-implementation-version))
  (format out "
(defun %case-delta (n table)
  \"What is added to the code N to get its other case, by TABLE; 0 when N has none.\"
  (let ((lo 0) (hi (- (floor (length table) 4) 1)))
    (do () ((> lo hi) 0)
      (let* ((mid (floor (+ lo hi) 2))
             (i (* mid 4)))
        (cond ((< n (svref table i)) (setq hi (- mid 1)))
              ((> n (svref table (+ i 1))) (setq lo (+ mid 1)))
              (t (return (if (zerop (rem (- n (svref table i)) (svref table (+ i 2))))
                             (svref table (+ i 3))
                             0))))))))
")
  (write-table "%downcase-code" (ranges #'char-downcase) out)
  (write-table "%upcase-code" (ranges #'char-upcase) out))
