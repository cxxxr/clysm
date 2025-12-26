;;;; macros.lisp - Standard macros for Clysm
;;;; Phase 8 - US6: Standard macro implementations

(in-package #:clysm/lib/macros)

;;; ============================================================
;;; Host-side macro definitions (for use in host SBCL)
;;; ============================================================

(defmacro when* (test &body body)
  "Execute body if test is true."
  `(if ,test (progn ,@body) nil))

(defmacro unless* (test &body body)
  "Execute body if test is false."
  `(if ,test nil (progn ,@body)))

(defmacro cond* (&rest clauses)
  "Conditional with multiple branches."
  (if clauses
      (let ((clause (first clauses)))
        (if (eq (first clause) t)
            `(progn ,@(rest clause))
            `(if ,(first clause)
                 (progn ,@(rest clause))
                 (cond* ,@(rest clauses)))))
      nil))

(defmacro dolist* ((var list &optional result) &body body)
  "Iterate over a list."
  (let ((lst (gensym)))
    `(let ((,lst ,list))
       (loop while ,lst
             do (let ((,var (car ,lst)))
                  ,@body)
                (setf ,lst (cdr ,lst)))
       ,result)))

(defmacro dotimes* ((var count &optional result) &body body)
  "Iterate a fixed number of times."
  (let ((cnt (gensym)))
    `(let ((,cnt ,count))
       (loop for ,var from 0 below ,cnt
             do (progn ,@body))
       ,result)))

(defmacro and* (&rest forms)
  "Evaluate forms left to right, return nil if any is nil."
  (cond
    ((null forms) t)
    ((null (rest forms)) (first forms))
    (t `(if ,(first forms)
            (and* ,@(rest forms))
            nil))))

(defmacro or* (&rest forms)
  "Evaluate forms left to right, return first non-nil value."
  (cond
    ((null forms) nil)
    ((null (rest forms)) (first forms))
    (t (let ((g (gensym)))
         `(let ((,g ,(first forms)))
            (if ,g ,g (or* ,@(rest forms))))))))

;;; ============================================================
;;; Macro expander functions for Clysm compiler
;;; These are the macro functions that get registered in the macro registry
;;; ============================================================

(defun make-when-expander ()
  "Create a macro expander for WHEN."
  (lambda (form)
    (let ((test (second form))
          (body (cddr form)))
      (list 'if test (cons 'progn body) nil))))

(defun make-unless-expander ()
  "Create a macro expander for UNLESS."
  (lambda (form)
    (let ((test (second form))
          (body (cddr form)))
      (list 'if test nil (cons 'progn body)))))

(defun make-cond-expander ()
  "Create a macro expander for COND."
  (lambda (form)
    (labels ((expand-clauses (clauses)
               (if (null clauses)
                   nil
                   (let ((clause (first clauses)))
                     (if (eq (first clause) t)
                         (cons 'progn (rest clause))
                         (list 'if (first clause)
                               (cons 'progn (rest clause))
                               (expand-clauses (rest clauses))))))))
      (expand-clauses (rest form)))))

(defun make-and-expander ()
  "Create a macro expander for AND."
  (lambda (form)
    (let ((args (rest form)))
      (cond
        ((null args) t)
        ((null (rest args)) (first args))
        (t (list 'if (first args)
                 (cons 'and (rest args))
                 nil))))))

(defun make-or-expander ()
  "Create a macro expander for OR."
  (lambda (form)
    (let ((args (rest form)))
      (cond
        ((null args) nil)
        ((null (rest args)) (first args))
        (t (let ((g (gensym "OR-")))
             (list 'let (list (list g (first args)))
                   (list 'if g g (cons 'or (rest args))))))))))

(defun make-dolist-expander ()
  "Create a macro expander for DOLIST."
  (lambda (form)
    (let* ((spec (second form))
           (var (first spec))
           (list-form (second spec))
           (result (third spec))
           (body (cddr form))
           (lst-var (gensym "LST-")))
      ;; Expand to a loop construct
      (list 'let (list (list lst-var list-form))
            (list 'block nil
                  (list 'tagbody
                        'loop-start
                        (list 'if (list 'null lst-var)
                              (list 'go 'loop-end))
                        (list 'let (list (list var (list 'car lst-var)))
                              (cons 'progn body))
                        (list 'setq lst-var (list 'cdr lst-var))
                        (list 'go 'loop-start)
                        'loop-end)
                  result)))))

(defun make-dotimes-expander ()
  "Create a macro expander for DOTIMES."
  (lambda (form)
    (let* ((spec (second form))
           (var (first spec))
           (count-form (second spec))
           (result (third spec))
           (body (cddr form))
           (cnt-var (gensym "CNT-")))
      ;; Expand to a loop construct
      (list 'let (list (list cnt-var count-form)
                       (list var 0))
            (list 'block nil
                  (list 'tagbody
                        'loop-start
                        (list 'if (list '>= var cnt-var)
                              (list 'go 'loop-end))
                        (cons 'progn body)
                        (list 'setq var (list '+ var 1))
                        (list 'go 'loop-start)
                        'loop-end)
                  result)))))

(defun make-case-expander ()
  "Create a macro expander for CASE.
   (case keyform (key form...) ... [(otherwise|t form...)])
   Expands to a let binding the keyform, then nested if/eql tests."
  (lambda (form)
    (let ((keyform (second form))
          (clauses (cddr form))
          (key-var (gensym "KEY-")))
      (labels ((otherwise-clause-p (clause)
                 (let ((keys (first clause)))
                   (or (eq keys 'otherwise)
                       (eq keys 't))))
               (make-key-test (key-var key)
                 (list 'eql key-var (list 'quote key)))
               (make-clause-test (key-var keys)
                 (cond
                   ;; Single key (not a list)
                   ((not (listp keys))
                    (make-key-test key-var keys))
                   ;; Multiple keys in a list
                   ((null (rest keys))
                    (make-key-test key-var (first keys)))
                   (t
                    (cons 'or (mapcar (lambda (k)
                                        (make-key-test key-var k))
                                      keys)))))
               (expand-clauses (clauses)
                 (if (null clauses)
                     nil
                     (let* ((clause (first clauses))
                            (keys (first clause))
                            (body (rest clause)))
                       (if (otherwise-clause-p clause)
                           ;; otherwise/t clause - just the body
                           (cons 'progn body)
                           ;; Normal clause
                           (list 'if
                                 (make-clause-test key-var keys)
                                 (cons 'progn body)
                                 (expand-clauses (rest clauses))))))))
        (list 'let (list (list key-var keyform))
              (expand-clauses clauses))))))

(defun make-prog1-expander ()
  "Create a macro expander for PROG1.
   (prog1 first-form form*) - Evaluates all forms, returns first's value."
  (lambda (form)
    (let ((first-form (second form))
          (rest-forms (cddr form))
          (result-var (gensym "PROG1-")))
      (list 'let (list (list result-var first-form))
            (cons 'progn (append rest-forms (list result-var)))))))

(defun make-prog2-expander ()
  "Create a macro expander for PROG2.
   (prog2 first-form second-form form*) - Evaluates all forms, returns second's value."
  (lambda (form)
    (let ((first-form (second form))
          (second-form (third form))
          (rest-forms (cdddr form))
          (result-var (gensym "PROG2-")))
      (list 'progn
            first-form
            (list 'let (list (list result-var second-form))
                  (cons 'progn (append rest-forms (list result-var))))))))

(defun make-do-expander ()
  "Create a macro expander for DO.
   (do ((var init [step])...) (end-test result...) body...)
   Expands to a block with tagbody for iteration."
  (lambda (form)
    (let ((var-clauses (second form))
          (end-clause (third form))
          (body (cdddr form)))
      (let ((end-test (first end-clause))
            (result-forms (rest end-clause))
            (loop-tag (gensym "DO-LOOP-"))
            (end-tag (gensym "DO-END-")))
        ;; Build initial bindings and step forms
        (let ((bindings (mapcar (lambda (clause)
                                  (list (first clause) (second clause)))
                                var-clauses))
              (step-setqs (loop for clause in var-clauses
                                when (cddr clause)
                                  collect (list (first clause) (third clause)))))
          (list 'block nil
                (list* 'let bindings
                       (list 'tagbody
                             loop-tag
                             (list 'if end-test
                                   (list 'go end-tag))
                             (cons 'progn body)
                             ;; Parallel assignment of step forms
                             (if step-setqs
                                 (list* 'psetq (apply #'append step-setqs))
                                 nil)
                             (list 'go loop-tag)
                             end-tag)
                       ;; Return result forms
                       result-forms)))))))

(defun make-do*-expander ()
  "Create a macro expander for DO*.
   Like DO but with sequential variable binding."
  (lambda (form)
    (let ((var-clauses (second form))
          (end-clause (third form))
          (body (cdddr form)))
      (let ((end-test (first end-clause))
            (result-forms (rest end-clause))
            (loop-tag (gensym "DO*-LOOP-"))
            (end-tag (gensym "DO*-END-")))
        ;; Build initial bindings and step forms
        (let ((bindings (mapcar (lambda (clause)
                                  (list (first clause) (second clause)))
                                var-clauses))
              (step-setqs (loop for clause in var-clauses
                                when (cddr clause)
                                  collect (list 'setq (first clause) (third clause)))))
          (list 'block nil
                (list* 'let* bindings
                       (list 'tagbody
                             loop-tag
                             (list 'if end-test
                                   (list 'go end-tag))
                             (cons 'progn body)
                             ;; Sequential assignment of step forms
                             (if step-setqs
                                 (cons 'progn step-setqs)
                                 nil)
                             (list 'go loop-tag)
                             end-tag)
                       ;; Return result forms
                       result-forms)))))))

;;; ============================================================
;;; Setf macro expanders (028-setf-generalized-refs)
;;; ============================================================

(defun make-setf-expander ()
  "Create a macro expander for SETF.
   (setf place value) - Set place to value and return value.
   (setf place1 value1 place2 value2 ...) - Multiple pairs."
  (lambda (form)
    (let ((pairs (rest form)))
      (cond
        ;; No arguments - return nil
        ((null pairs) nil)
        ;; Odd number of arguments - error
        ((oddp (length pairs))
         (error 'clysm/lib/setf-expanders:odd-argument-count
                :macro-name 'setf
                :argument-count (length pairs)))
        ;; Single pair
        ((= 2 (length pairs))
         (expand-single-setf (first pairs) (second pairs)))
        ;; Multiple pairs - expand to progn of single setf forms
        (t
         (cons 'progn
               (loop for (place value) on pairs by #'cddr
                     collect (list 'setf place value))))))))

(defun expand-single-setf (place value)
  "Expand a single (setf place value) form."
  (cond
    ;; Simple variable case
    ((and (symbolp place)
          (not (keywordp place))
          (not (eq place t))
          (not (eq place nil)))
     (list 'setq place value))
    ;; Compound place - use setf expansion protocol
    ((consp place)
     (multiple-value-bind (temps vals stores store-form access-form)
         (clysm/lib/setf-expanders:get-setf-expansion* place)
       (declare (ignore access-form))
       (let ((store (first stores))
             (bindings (mapcar #'list temps vals)))
         ;; Build the expansion:
         ;; (let ((temp1 val1) (temp2 val2) ...)
         ;;   (let ((store value))
         ;;     store-form))
         (if bindings
             (list 'let bindings
                   (list 'let (list (list store value))
                         store-form))
             ;; No temps needed
             (list 'let (list (list store value))
                   store-form)))))
    ;; Invalid place (constant)
    ((or (keywordp place) (eq place t) (eq place nil))
     (error 'clysm/lib/setf-expanders:constant-modification-error
            :place place))
    ;; Invalid place (other)
    (t
     (error 'clysm/lib/setf-expanders:invalid-place
            :place place))))

(defun make-psetf-expander ()
  "Create a macro expander for PSETF.
   (psetf place1 value1 place2 value2 ...) - Parallel assignment."
  (lambda (form)
    (let ((pairs (rest form)))
      (cond
        ;; No arguments - return nil
        ((null pairs) nil)
        ;; Odd number of arguments - error
        ((oddp (length pairs))
         (error 'clysm/lib/setf-expanders:odd-argument-count
                :macro-name 'psetf
                :argument-count (length pairs)))
        ;; Expand to parallel assignment
        (t
         (let ((temps nil)
               (bindings nil)
               (setfs nil))
           ;; Collect all the information
           (loop for (place value) on pairs by #'cddr
                 do (let ((temp (gensym "PSETF-")))
                      (push temp temps)
                      (push (list temp value) bindings)
                      (push (list 'setf place temp) setfs)))
           ;; Build: (let ((temp1 val1) (temp2 val2) ...) (setf place1 temp1) ... nil)
           (list* 'let (nreverse bindings)
                  (append (nreverse setfs) (list nil)))))))))

(defun make-incf-expander ()
  "Create a macro expander for INCF.
   (incf place [delta]) - Increment place by delta (default 1)."
  (lambda (form)
    (let ((place (second form))
          (delta (or (third form) 1)))
      (list 'setf place (list '+ place delta)))))

(defun make-decf-expander ()
  "Create a macro expander for DECF.
   (decf place [delta]) - Decrement place by delta (default 1)."
  (lambda (form)
    (let ((place (second form))
          (delta (or (third form) 1)))
      (list 'setf place (list '- place delta)))))

(defun make-push-expander ()
  "Create a macro expander for PUSH.
   (push item place) - Cons item onto place."
  (lambda (form)
    (let ((item (second form))
          (place (third form)))
      (list 'setf place (list 'cons item place)))))

(defun make-pop-expander ()
  "Create a macro expander for POP.
   (pop place) - Remove and return first element of place."
  (lambda (form)
    (let ((place (second form))
          (result-var (gensym "POP-")))
      ;; (let ((result (car place)))
      ;;   (setf place (cdr place))
      ;;   result)
      (list 'let (list (list result-var (list 'car place)))
            (list 'setf place (list 'cdr place))
            result-var))))

(defun make-pushnew-expander ()
  "Create a macro expander for PUSHNEW.
   (pushnew item place &key test test-not key) - Push if not member."
  (lambda (form)
    (let* ((item (second form))
           (place (third form))
           (keys (cdddr form))
           (item-var (gensym "ITEM-"))
           (member-call (if keys
                            (list* 'member item-var place keys)
                            (list 'member item-var place))))
      ;; (let ((item-var item))
      ;;   (unless (member item-var place ...)
      ;;     (setf place (cons item-var place)))
      ;;   place)
      (list 'let (list (list item-var item))
            (list 'unless member-call
                  (list 'setf place (list 'cons item-var place)))
            place))))

(defun make-rotatef-expander ()
  "Create a macro expander for ROTATEF.
   (rotatef place1 place2 ...) - Rotate values cyclically."
  (lambda (form)
    (let ((places (rest form)))
      (cond
        ;; No places - return nil
        ((null places) nil)
        ;; Single place - no-op, return nil
        ((null (rest places)) nil)
        ;; Two places - swap
        ((= 2 (length places))
         (let ((temp (gensym "ROTATE-")))
           (list 'let (list (list temp (first places)))
                 (list 'setf (first places) (second places))
                 (list 'setf (second places) temp)
                 nil)))
        ;; Multiple places - rotate
        (t
         ;; Save all values in temps, then assign rotated
         (let ((temps (loop for p in places collect (gensym "ROTATE-"))))
           (list* 'let (mapcar #'list temps places)
                  (append
                   ;; Assign rotated values
                   (loop for place in places
                         for i from 0
                         for temp = (nth (mod (1+ i) (length temps)) temps)
                         collect (list 'setf place temp))
                   (list nil)))))))))

(defun make-shiftf-expander ()
  "Create a macro expander for SHIFTF.
   (shiftf place1 place2 ... newvalue) - Shift values left, return first."
  (lambda (form)
    (let ((args (rest form)))
      (cond
        ;; Need at least 2 arguments (place + newvalue)
        ((< (length args) 2)
         (error "SHIFTF requires at least a place and a new value"))
        ;; Single place + newvalue
        ((= 2 (length args))
         (let ((place (first args))
               (newvalue (second args))
               (result-var (gensym "SHIFTF-")))
           ;; (prog1 place (setf place newvalue))
           (list 'let (list (list result-var place))
                 (list 'setf place newvalue)
                 result-var)))
        ;; Multiple places + newvalue
        (t
         (let* ((places (butlast args))
                (newvalue (car (last args)))
                (temps (loop for p in places collect (gensym "SHIFTF-")))
                (result-var (first temps)))
           ;; Save all values, then shift
           (list* 'let (mapcar #'list temps places)
                  (append
                   ;; Assign shifted values (place[i] = temp[i+1])
                   (loop for place in (butlast places)
                         for temp in (rest temps)
                         collect (list 'setf place temp))
                   ;; Last place gets newvalue
                   (list (list 'setf (car (last places)) newvalue))
                   ;; Return first saved value
                   (list result-var)))))))))

;;; ============================================================
;;; Standard macro installation
;;; ============================================================

(defun install-setf-macros (registry)
  "Install setf-related macros into REGISTRY."
  (clysm/compiler/transform/macro:register-macro
   registry 'setf (make-setf-expander))
  (clysm/compiler/transform/macro:register-macro
   registry 'psetf (make-psetf-expander))
  (clysm/compiler/transform/macro:register-macro
   registry 'incf (make-incf-expander))
  (clysm/compiler/transform/macro:register-macro
   registry 'decf (make-decf-expander))
  (clysm/compiler/transform/macro:register-macro
   registry 'push (make-push-expander))
  (clysm/compiler/transform/macro:register-macro
   registry 'pop (make-pop-expander))
  (clysm/compiler/transform/macro:register-macro
   registry 'pushnew (make-pushnew-expander))
  (clysm/compiler/transform/macro:register-macro
   registry 'rotatef (make-rotatef-expander))
  (clysm/compiler/transform/macro:register-macro
   registry 'shiftf (make-shiftf-expander))
  registry)

(defun install-standard-macros (registry)
  "Install all standard macros into REGISTRY."
  (clysm/compiler/transform/macro:register-macro
   registry 'when (make-when-expander))
  (clysm/compiler/transform/macro:register-macro
   registry 'unless (make-unless-expander))
  (clysm/compiler/transform/macro:register-macro
   registry 'cond (make-cond-expander))
  (clysm/compiler/transform/macro:register-macro
   registry 'case (make-case-expander))
  (clysm/compiler/transform/macro:register-macro
   registry 'and (make-and-expander))
  (clysm/compiler/transform/macro:register-macro
   registry 'or (make-or-expander))
  (clysm/compiler/transform/macro:register-macro
   registry 'prog1 (make-prog1-expander))
  (clysm/compiler/transform/macro:register-macro
   registry 'prog2 (make-prog2-expander))
  (clysm/compiler/transform/macro:register-macro
   registry 'do (make-do-expander))
  (clysm/compiler/transform/macro:register-macro
   registry 'do* (make-do*-expander))
  (clysm/compiler/transform/macro:register-macro
   registry 'dolist (make-dolist-expander))
  (clysm/compiler/transform/macro:register-macro
   registry 'dotimes (make-dotimes-expander))
  ;; Also install setf-related macros
  (install-setf-macros registry)
  registry)
