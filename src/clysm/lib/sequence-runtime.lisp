;;;; sequence-runtime.lisp - Runtime library sequence functions
;;;; Feature: 001-sequence-runtime-migration
;;;;
;;;; Implements sequence operations (remove, count, substitute, delete)
;;;; using car/cdr/cons primitives only (Layer 1).
;;;; These functions are compiled to Wasm and called via runtime dispatch.
;;;;
;;;; HyperSpec references:
;;;;   [remove](resources/HyperSpec/Body/f_rm_rm.htm)
;;;;   [count](resources/HyperSpec/Body/f_countc.htm)
;;;;   [substitute](resources/HyperSpec/Body/f_sbs_s.htm)
;;;;   [delete](resources/HyperSpec/Body/f_rm_rm.htm)

(in-package #:clysm)

;;; ============================================================
;;; Helper Functions (Layer 1 primitives only)
;;; ============================================================

(defun seq-apply-test (item element test key)
  "Apply the TEST function to compare ITEM with ELEMENT.
   KEY is applied to ELEMENT before comparison.
   Uses only funcall (Layer 1 primitive)."
  (let ((keyed-element (if key (funcall key element) element))
        (test-fn (or test #'eql)))
    (funcall test-fn item keyed-element)))

(defun seq-apply-predicate (predicate element key)
  "Apply PREDICATE to ELEMENT with optional KEY function.
   Uses only funcall (Layer 1 primitive)."
  (let ((keyed-element (if key (funcall key element) element)))
    (funcall predicate keyed-element)))

;;; ============================================================
;;; Remove Family ([remove](resources/HyperSpec/Body/f_rm_rm.htm))
;;;
;;; Returns copies of sequences with specified elements removed.
;;; Non-destructive operations that build new cons cells.
;;; ============================================================

(defun remove-rt (item list test key start end count from-end)
  "Return a copy of LIST without elements matching ITEM.
   See [remove](resources/HyperSpec/Body/f_rm_rm.htm).
   Uses only Layer 1 primitives: car, cdr, cons, consp, funcall."
  (cond
    ;; Handle :from-end by reversing, processing, then reversing back
    (from-end
     (nreverse (remove-rt item (nreverse (copy-list list))
                          test key
                          (if end (- (length list) end) 0)
                          (if start (- (length list) start) nil)
                          count nil)))
    ;; Normal forward processing
    (t
     (let ((result nil)
           (index 0)
           (matched 0)
           (test-fn (or test #'eql)))
       (loop for rest = list then (cdr rest)
             while (consp rest)
             for element = (car rest)
             do (let* ((in-range (and (or (null start) (>= index start))
                                      (or (null end) (< index end))))
                       (keyed (if key (funcall key element) element))
                       (matches (and in-range
                                     (funcall test-fn item keyed)
                                     (or (null count) (< matched count)))))
                  (if matches
                      (incf matched)
                      (push element result))
                  (incf index)))
       (nreverse result)))))

(defun remove-if-rt (predicate list key start end count from-end)
  "Return a copy of LIST without elements satisfying PREDICATE.
   See [remove-if](resources/HyperSpec/Body/f_rm_rm.htm).
   Uses only Layer 1 primitives."
  (cond
    (from-end
     (nreverse (remove-if-rt predicate (nreverse (copy-list list))
                             key
                             (if end (- (length list) end) 0)
                             (if start (- (length list) start) nil)
                             count nil)))
    (t
     (let ((result nil)
           (index 0)
           (matched 0))
       (loop for rest = list then (cdr rest)
             while (consp rest)
             for element = (car rest)
             do (let* ((in-range (and (or (null start) (>= index start))
                                      (or (null end) (< index end))))
                       (keyed (if key (funcall key element) element))
                       (matches (and in-range
                                     (funcall predicate keyed)
                                     (or (null count) (< matched count)))))
                  (if matches
                      (incf matched)
                      (push element result))
                  (incf index)))
       (nreverse result)))))

(defun remove-if-not-rt (predicate list key start end count from-end)
  "Return a copy of LIST keeping only elements satisfying PREDICATE.
   See [remove-if-not](resources/HyperSpec/Body/f_rm_rm.htm).
   Uses only Layer 1 primitives."
  (cond
    (from-end
     (nreverse (remove-if-not-rt predicate (nreverse (copy-list list))
                                 key
                                 (if end (- (length list) end) 0)
                                 (if start (- (length list) start) nil)
                                 count nil)))
    (t
     (let ((result nil)
           (index 0)
           (matched 0))
       (loop for rest = list then (cdr rest)
             while (consp rest)
             for element = (car rest)
             do (let* ((in-range (and (or (null start) (>= index start))
                                      (or (null end) (< index end))))
                       (keyed (if key (funcall key element) element))
                       (should-remove (and in-range
                                           (not (funcall predicate keyed))
                                           (or (null count) (< matched count)))))
                  (if should-remove
                      (incf matched)
                      (push element result))
                  (incf index)))
       (nreverse result)))))

;;; ============================================================
;;; Count Family ([count](resources/HyperSpec/Body/f_countc.htm))
;;;
;;; Returns the number of elements matching the criteria.
;;; ============================================================

(defun count-rt (item list test key start end from-end)
  "Return the number of elements in LIST matching ITEM.
   See [count](resources/HyperSpec/Body/f_countc.htm).
   Uses only Layer 1 primitives."
  (declare (ignore from-end))  ; from-end doesn't affect count result
  (let ((result 0)
        (index 0)
        (test-fn (or test #'eql)))
    (loop for rest = list then (cdr rest)
          while (consp rest)
          for element = (car rest)
          do (when (and (or (null start) (>= index start))
                        (or (null end) (< index end)))
               (let ((keyed (if key (funcall key element) element)))
                 (when (funcall test-fn item keyed)
                   (incf result))))
             (incf index))
    result))

(defun count-if-rt (predicate list key start end from-end)
  "Return the number of elements in LIST satisfying PREDICATE.
   See [count-if](resources/HyperSpec/Body/f_countc.htm).
   Uses only Layer 1 primitives."
  (declare (ignore from-end))
  (let ((result 0)
        (index 0))
    (loop for rest = list then (cdr rest)
          while (consp rest)
          for element = (car rest)
          do (when (and (or (null start) (>= index start))
                        (or (null end) (< index end)))
               (let ((keyed (if key (funcall key element) element)))
                 (when (funcall predicate keyed)
                   (incf result))))
             (incf index))
    result))

(defun count-if-not-rt (predicate list key start end from-end)
  "Return the number of elements in LIST NOT satisfying PREDICATE.
   See [count-if-not](resources/HyperSpec/Body/f_countc.htm).
   Uses only Layer 1 primitives."
  (declare (ignore from-end))
  (let ((result 0)
        (index 0))
    (loop for rest = list then (cdr rest)
          while (consp rest)
          for element = (car rest)
          do (when (and (or (null start) (>= index start))
                        (or (null end) (< index end)))
               (let ((keyed (if key (funcall key element) element)))
                 (when (not (funcall predicate keyed))
                   (incf result))))
             (incf index))
    result))

;;; ============================================================
;;; Substitute Family ([substitute](resources/HyperSpec/Body/f_sbs_s.htm))
;;;
;;; Returns copies with specified elements replaced.
;;; Non-destructive operations.
;;; ============================================================

(defun substitute-rt (newitem olditem list test key start end count from-end)
  "Return a copy of LIST with elements matching OLDITEM replaced by NEWITEM.
   See [substitute](resources/HyperSpec/Body/f_sbs_s.htm).
   Uses only Layer 1 primitives."
  (cond
    (from-end
     (nreverse (substitute-rt newitem olditem (nreverse (copy-list list))
                              test key
                              (if end (- (length list) end) 0)
                              (if start (- (length list) start) nil)
                              count nil)))
    (t
     (let ((result nil)
           (index 0)
           (replaced 0)
           (test-fn (or test #'eql)))
       (loop for rest = list then (cdr rest)
             while (consp rest)
             for element = (car rest)
             do (let* ((in-range (and (or (null start) (>= index start))
                                      (or (null end) (< index end))))
                       (keyed (if key (funcall key element) element))
                       (matches (and in-range
                                     (funcall test-fn olditem keyed)
                                     (or (null count) (< replaced count)))))
                  (if matches
                      (progn (push newitem result) (incf replaced))
                      (push element result))
                  (incf index)))
       (nreverse result)))))

(defun substitute-if-rt (newitem predicate list key start end count from-end)
  "Return a copy of LIST with elements satisfying PREDICATE replaced by NEWITEM.
   See [substitute-if](resources/HyperSpec/Body/f_sbs_s.htm).
   Uses only Layer 1 primitives."
  (cond
    (from-end
     (nreverse (substitute-if-rt newitem predicate (nreverse (copy-list list))
                                 key
                                 (if end (- (length list) end) 0)
                                 (if start (- (length list) start) nil)
                                 count nil)))
    (t
     (let ((result nil)
           (index 0)
           (replaced 0))
       (loop for rest = list then (cdr rest)
             while (consp rest)
             for element = (car rest)
             do (let* ((in-range (and (or (null start) (>= index start))
                                      (or (null end) (< index end))))
                       (keyed (if key (funcall key element) element))
                       (matches (and in-range
                                     (funcall predicate keyed)
                                     (or (null count) (< replaced count)))))
                  (if matches
                      (progn (push newitem result) (incf replaced))
                      (push element result))
                  (incf index)))
       (nreverse result)))))

(defun substitute-if-not-rt (newitem predicate list key start end count from-end)
  "Return a copy of LIST with elements NOT satisfying PREDICATE replaced by NEWITEM.
   See [substitute-if-not](resources/HyperSpec/Body/f_sbs_s.htm).
   Uses only Layer 1 primitives."
  (cond
    (from-end
     (nreverse (substitute-if-not-rt newitem predicate (nreverse (copy-list list))
                                     key
                                     (if end (- (length list) end) 0)
                                     (if start (- (length list) start) nil)
                                     count nil)))
    (t
     (let ((result nil)
           (index 0)
           (replaced 0))
       (loop for rest = list then (cdr rest)
             while (consp rest)
             for element = (car rest)
             do (let* ((in-range (and (or (null start) (>= index start))
                                      (or (null end) (< index end))))
                       (keyed (if key (funcall key element) element))
                       (matches (and in-range
                                     (not (funcall predicate keyed))
                                     (or (null count) (< replaced count)))))
                  (if matches
                      (progn (push newitem result) (incf replaced))
                      (push element result))
                  (incf index)))
       (nreverse result)))))

;;; ============================================================
;;; Delete Family ([delete](resources/HyperSpec/Body/f_rm_rm.htm))
;;;
;;; Destructive versions that may modify the original list.
;;; Uses rplaca/rplacd for efficiency.
;;; ============================================================

(defun delete-rt (item list test key start end count from-end)
  "Destructively remove elements matching ITEM from LIST.
   See [delete](resources/HyperSpec/Body/f_rm_rm.htm).
   May modify the original list structure."
  (cond
    (from-end
     (nreverse (delete-rt item (nreverse list)
                          test key
                          (if end (- (length list) end) 0)
                          (if start (- (length list) start) nil)
                          count nil)))
    (t
     (let ((test-fn (or test #'eql))
           (index 0)
           (deleted 0)
           (prev nil)
           (current list)
           (result list))
       ;; Handle leading matches
       (loop while (and (consp current)
                        (or (null start) (>= index start))
                        (or (null end) (< index end))
                        (or (null count) (< deleted count)))
             for element = (car current)
             for keyed = (if key (funcall key element) element)
             while (funcall test-fn item keyed)
             do (setf result (cdr current))
                (setf current result)
                (incf deleted)
                (incf index))
       ;; Process remaining
       (setf prev current)
       (when (consp current)
         (setf current (cdr current))
         (incf index))
       (loop while (consp current)
             for element = (car current)
             do (let* ((in-range (and (or (null start) (>= index start))
                                      (or (null end) (< index end))))
                       (keyed (if key (funcall key element) element))
                       (matches (and in-range
                                     (funcall test-fn item keyed)
                                     (or (null count) (< deleted count)))))
                  (if matches
                      (progn
                        (rplacd prev (cdr current))
                        (incf deleted))
                      (setf prev current))
                  (setf current (cdr current))
                  (incf index)))
       result))))

(defun delete-if-rt (predicate list key start end count from-end)
  "Destructively remove elements satisfying PREDICATE from LIST.
   See [delete-if](resources/HyperSpec/Body/f_rm_rm.htm).
   May modify the original list structure."
  (cond
    (from-end
     (nreverse (delete-if-rt predicate (nreverse list)
                             key
                             (if end (- (length list) end) 0)
                             (if start (- (length list) start) nil)
                             count nil)))
    (t
     (let ((index 0)
           (deleted 0)
           (prev nil)
           (current list)
           (result list))
       ;; Handle leading matches
       (loop while (and (consp current)
                        (or (null start) (>= index start))
                        (or (null end) (< index end))
                        (or (null count) (< deleted count)))
             for element = (car current)
             for keyed = (if key (funcall key element) element)
             while (funcall predicate keyed)
             do (setf result (cdr current))
                (setf current result)
                (incf deleted)
                (incf index))
       ;; Process remaining
       (setf prev current)
       (when (consp current)
         (setf current (cdr current))
         (incf index))
       (loop while (consp current)
             for element = (car current)
             do (let* ((in-range (and (or (null start) (>= index start))
                                      (or (null end) (< index end))))
                       (keyed (if key (funcall key element) element))
                       (matches (and in-range
                                     (funcall predicate keyed)
                                     (or (null count) (< deleted count)))))
                  (if matches
                      (progn
                        (rplacd prev (cdr current))
                        (incf deleted))
                      (setf prev current))
                  (setf current (cdr current))
                  (incf index)))
       result))))

(defun delete-if-not-rt (predicate list key start end count from-end)
  "Destructively remove elements NOT satisfying PREDICATE from LIST.
   See [delete-if-not](resources/HyperSpec/Body/f_rm_rm.htm).
   May modify the original list structure."
  (cond
    (from-end
     (nreverse (delete-if-not-rt predicate (nreverse list)
                                 key
                                 (if end (- (length list) end) 0)
                                 (if start (- (length list) start) nil)
                                 count nil)))
    (t
     (let ((index 0)
           (deleted 0)
           (prev nil)
           (current list)
           (result list))
       ;; Handle leading matches (elements NOT satisfying predicate)
       (loop while (and (consp current)
                        (or (null start) (>= index start))
                        (or (null end) (< index end))
                        (or (null count) (< deleted count)))
             for element = (car current)
             for keyed = (if key (funcall key element) element)
             while (not (funcall predicate keyed))
             do (setf result (cdr current))
                (setf current result)
                (incf deleted)
                (incf index))
       ;; Process remaining
       (setf prev current)
       (when (consp current)
         (setf current (cdr current))
         (incf index))
       (loop while (consp current)
             for element = (car current)
             do (let* ((in-range (and (or (null start) (>= index start))
                                      (or (null end) (< index end))))
                       (keyed (if key (funcall key element) element))
                       (matches (and in-range
                                     (not (funcall predicate keyed))
                                     (or (null count) (< deleted count)))))
                  (if matches
                      (progn
                        (rplacd prev (cdr current))
                        (incf deleted))
                      (setf prev current))
                  (setf current (cdr current))
                  (incf index)))
       result))))

;;; EOF
