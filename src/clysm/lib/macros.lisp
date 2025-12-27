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

;; Feature 043: RETURN macro for LOOP's finally clause
(defun make-return-expander ()
  "Create a macro expander for RETURN.
   (return [value]) expands to (return-from nil value).
   Used by LOOP's finally clause."
  (lambda (form)
    (let ((value (if (cdr form) (second form) nil)))
      (list 'return-from nil value))))

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

(defun make-psetq-expander ()
  "Create a macro expander for PSETQ.
   (psetq var1 value1 var2 value2 ...) - Parallel assignment to simple variables.
   Feature 043: Required for LOOP macro expansion."
  (lambda (form)
    (let ((pairs (rest form)))
      (cond
        ;; No arguments - return nil
        ((null pairs) nil)
        ;; Odd number of arguments - error
        ((oddp (length pairs))
         (error "PSETQ requires an even number of arguments"))
        ;; Single pair - expand to setq
        ((= (length pairs) 2)
         (list 'setq (first pairs) (second pairs)))
        ;; Multiple pairs - expand to parallel assignment
        (t
         (let ((temps nil)
               (bindings nil)
               (setqs nil))
           ;; Collect all the information
           (loop for (var value) on pairs by #'cddr
                 do (let ((temp (gensym "PSETQ-")))
                      (push temp temps)
                      (push (list temp value) bindings)
                      (push var setqs)
                      (push temp setqs)))
           ;; Build: (let ((temp1 val1) ...) (setq var1 temp1 ...) nil)
           (list 'let (nreverse bindings)
                 (list* 'setq (nreverse setqs))
                 nil)))))))

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
;;; LOOP Macro Infrastructure (029-loop-macro)
;;; ============================================================

;;; -----------------------------------------------------------
;;; LOOP context and clause structures
;;; -----------------------------------------------------------

(defstruct loop-context
  "Complete LOOP parsing and expansion context"
  (name nil :type (or null symbol))           ; NAMED loop name
  (iteration-clauses nil :type list)          ; List of loop-iteration-clause
  (accumulation-clauses nil :type list)       ; List of loop-accumulation-clause
  (termination-clauses nil :type list)        ; List of loop-termination-clause
  (body-clauses nil :type list)               ; DO/conditional clauses
  (initially-forms nil :type list)            ; INITIALLY forms
  (finally-forms nil :type list)              ; FINALLY forms
  (with-bindings nil :type list)              ; ((var init) ...) from WITH
  (result-form nil :type t)                   ; Final return expression
  (gensym-counter 0 :type fixnum))            ; For unique variable generation

;;; -----------------------------------------------------------
;;; Iteration clause structures (FOR/AS)
;;; -----------------------------------------------------------

(defstruct loop-iteration-clause
  "Base structure for iteration clauses"
  (var nil :type symbol)                      ; Loop variable
  (clause-type nil :type keyword))            ; :arithmetic, :in, :on, :across, :hash-keys, :hash-values, :equals

(defstruct (loop-iter-arithmetic (:include loop-iteration-clause))
  "FOR var FROM x TO y BY z"
  (from nil :type t)                          ; Start expression
  (to nil :type t)                            ; End expression (inclusive)
  (below nil :type t)                         ; End expression (exclusive)
  (above nil :type t)                         ; End expression (exclusive descending)
  (downto nil :type t)                        ; End expression (inclusive descending)
  (downfrom nil :type t)                      ; Start value for descending
  (upfrom nil :type t)                        ; Start value for ascending
  (by nil :type t))                           ; Step expression (default 1)

(defstruct (loop-iter-in (:include loop-iteration-clause))
  "FOR var IN list [BY step-fn]"
  (list-form nil :type t)                     ; List expression
  (step-fn nil :type t)                       ; Step function (default #'cdr)
  (list-var nil :type symbol))                ; Generated list variable for iteration

(defstruct (loop-iter-on (:include loop-iteration-clause))
  "FOR var ON list [BY step-fn]"
  (list-form nil :type t)
  (step-fn nil :type t)
  (list-var nil :type symbol))                ; Generated list variable for iteration

(defstruct (loop-iter-across (:include loop-iteration-clause))
  "FOR var ACROSS vector"
  (vector-form nil :type t)                   ; Vector expression
  (index-var nil :type symbol)                ; Generated index variable
  (vec-var nil :type symbol))                 ; Generated vector variable

(defstruct (loop-iter-hash (:include loop-iteration-clause))
  "FOR var BEING THE HASH-KEYS/VALUES OF hash-table"
  (hash-form nil :type t)                     ; Hash table expression
  (value-var nil :type symbol)                ; Optional USING (HASH-VALUE v)
  (key-var nil :type symbol)                  ; Optional USING (HASH-KEY k)
  (mode :keys :type keyword))                 ; :keys or :values

(defstruct (loop-iter-equals (:include loop-iteration-clause))
  "FOR var = init-form [THEN step-form]"
  (init-form nil :type t)                     ; Initial value
  (then-form nil :type t))                    ; Step expression (nil = no stepping)

;;; -----------------------------------------------------------
;;; Accumulation clause structures (COLLECT/SUM/COUNT/etc.)
;;; -----------------------------------------------------------

(defstruct loop-accumulation-clause
  "Accumulation clause"
  (type nil :type keyword)                    ; :collect, :sum, :count, :maximize, :minimize, :append, :nconc
  (expr nil :type t)                          ; Expression to accumulate
  (into-var nil :type symbol)                 ; Optional INTO variable
  (acc-var nil :type symbol))                 ; Generated accumulator variable

;;; -----------------------------------------------------------
;;; Termination clause structures (WHILE/UNTIL/ALWAYS/etc.)
;;; -----------------------------------------------------------

(defstruct loop-termination-clause
  "Termination or boolean aggregation clause"
  (type nil :type keyword)                    ; :while, :until, :always, :never, :thereis, :return
  (expr nil :type t))                         ; Condition or return expression

;;; -----------------------------------------------------------
;;; Conditional clause structures (IF/WHEN/UNLESS)
;;; -----------------------------------------------------------

(defstruct loop-conditional-clause
  "Conditional execution"
  (type nil :type keyword)                    ; :if, :when, :unless
  (condition nil :type t)                     ; Test expression
  (then-clauses nil :type list)               ; Clauses when true (or false for :unless)
  (else-clauses nil :type list))              ; Optional ELSE clauses

;;; -----------------------------------------------------------
;;; LOOP Keyword Recognition (T009)
;;; -----------------------------------------------------------

(defparameter *loop-keywords*
  '(for as with initially finally do doing return
    collect collecting sum summing count counting
    maximize maximizing minimize minimizing
    append appending nconc nconcing
    while until always never thereis
    if when unless else end and
    from to below above downto downfrom upfrom by
    in on across being the hash-keys hash-values of using
    into named repeat loop-finish)
  "All recognized LOOP keywords.")

(defun loop-keyword-p (symbol)
  "Return T if SYMBOL is a LOOP keyword."
  (and (symbolp symbol)
       (member (if (keywordp symbol)
                   (intern (symbol-name symbol) :cl)
                   symbol)
               *loop-keywords*
               :test #'string-equal)))

;;; -----------------------------------------------------------
;;; LOOP Gensym Generation (T010)
;;; -----------------------------------------------------------

(defun make-loop-gensym (ctx prefix)
  "Generate a unique symbol for LOOP expansion using CTX's counter."
  (let ((counter (loop-context-gensym-counter ctx)))
    (setf (loop-context-gensym-counter ctx) (1+ counter))
    (gensym (format nil "~A-~D-" prefix counter))))

;;; -----------------------------------------------------------
;;; LOOP Clause Parser Dispatcher (T008)
;;; -----------------------------------------------------------

(defun parse-loop-clauses (clauses ctx)
  "Parse LOOP clauses into structured clause objects in CTX.
   CLAUSES is a list of forms from (loop ...).
   Returns the remaining unparsed clauses (should be nil on success)."
  (loop while clauses do
    (let ((keyword (first clauses)))
      (cond
        ;; Named clause
        ((loop-keyword-eq keyword 'named)
         (setf (loop-context-name ctx) (second clauses))
         (setf clauses (cddr clauses)))

        ;; FOR/AS iteration clauses
        ((or (loop-keyword-eq keyword 'for)
             (loop-keyword-eq keyword 'as))
         (multiple-value-bind (clause rest)
             (parse-for-clause (rest clauses) ctx)
           (push clause (loop-context-iteration-clauses ctx))
           (setf clauses rest)))

        ;; WITH variable binding
        ((loop-keyword-eq keyword 'with)
         (multiple-value-bind (bindings rest)
             (parse-with-clause (rest clauses) ctx)
           (setf (loop-context-with-bindings ctx)
                 (append (loop-context-with-bindings ctx) bindings))
           (setf clauses rest)))

        ;; Accumulation clauses
        ((member keyword '(collect collecting sum summing count counting
                           maximize maximizing minimize minimizing
                           append appending nconc nconcing)
                 :test #'loop-keyword-eq)
         (multiple-value-bind (clause rest)
             (parse-accumulation-clause keyword (rest clauses) ctx)
           (push clause (loop-context-accumulation-clauses ctx))
           (setf clauses rest)))

        ;; Termination clauses
        ((member keyword '(while until) :test #'loop-keyword-eq)
         (multiple-value-bind (clause rest)
             (parse-termination-clause keyword (rest clauses) ctx)
           (push clause (loop-context-termination-clauses ctx))
           (setf clauses rest)))

        ;; Boolean aggregation clauses
        ((member keyword '(always never thereis) :test #'loop-keyword-eq)
         (multiple-value-bind (clause rest)
             (parse-boolean-clause keyword (rest clauses) ctx)
           (push clause (loop-context-termination-clauses ctx))
           (setf clauses rest)))

        ;; RETURN clause
        ((loop-keyword-eq keyword 'return)
         (let ((clause (make-loop-termination-clause
                        :type :return
                        :expr (second clauses))))
           (push clause (loop-context-termination-clauses ctx))
           (setf clauses (cddr clauses))))

        ;; Conditional clauses
        ((member keyword '(if when unless) :test #'loop-keyword-eq)
         (multiple-value-bind (clause rest)
             (parse-conditional-clause keyword (rest clauses) ctx)
           (push clause (loop-context-body-clauses ctx))
           (setf clauses rest)))

        ;; DO/DOING clause
        ((or (loop-keyword-eq keyword 'do)
             (loop-keyword-eq keyword 'doing))
         (multiple-value-bind (forms rest)
             (parse-do-forms (rest clauses))
           (dolist (form forms)
             (push form (loop-context-body-clauses ctx)))
           (setf clauses rest)))

        ;; INITIALLY clause
        ((loop-keyword-eq keyword 'initially)
         (multiple-value-bind (forms rest)
             (parse-compound-forms (rest clauses))
           (setf (loop-context-initially-forms ctx)
                 (append (loop-context-initially-forms ctx) forms))
           (setf clauses rest)))

        ;; FINALLY clause
        ((loop-keyword-eq keyword 'finally)
         (multiple-value-bind (forms rest)
             (parse-compound-forms (rest clauses))
           (setf (loop-context-finally-forms ctx)
                 (append (loop-context-finally-forms ctx) forms))
           (setf clauses rest)))

        ;; Unknown - might be a form for simple loop
        (t
         ;; For simple loop, treat remaining as body
         (push keyword (loop-context-body-clauses ctx))
         (setf clauses (rest clauses))))))
  ;; Reverse collected lists to maintain order
  (setf (loop-context-iteration-clauses ctx)
        (nreverse (loop-context-iteration-clauses ctx)))
  (setf (loop-context-accumulation-clauses ctx)
        (nreverse (loop-context-accumulation-clauses ctx)))
  (setf (loop-context-termination-clauses ctx)
        (nreverse (loop-context-termination-clauses ctx)))
  (setf (loop-context-body-clauses ctx)
        (nreverse (loop-context-body-clauses ctx)))
  ctx)

(defun loop-keyword-eq (form keyword)
  "Check if FORM is equivalent to LOOP keyword."
  (and (symbolp form)
       (string-equal (symbol-name form) (symbol-name keyword))))

;;; -----------------------------------------------------------
;;; LOOP Clause Parsers (Stubs - to be implemented in Phase 3+)
;;; -----------------------------------------------------------

(defun parse-for-clause (clauses ctx)
  "Parse a FOR/AS clause. Returns (values clause remaining-clauses)."
  (declare (ignore ctx))
  (let ((var (first clauses))
        (rest (rest clauses)))
    ;; Determine the type of FOR clause
    (cond
      ;; FOR var FROM ... (arithmetic)
      ((member (first rest) '(from upfrom downfrom to below above downto by)
               :test #'loop-keyword-eq)
       (parse-for-arithmetic var rest))
      ;; FOR var IN list
      ((loop-keyword-eq (first rest) 'in)
       (parse-for-in var rest))
      ;; FOR var ON list
      ((loop-keyword-eq (first rest) 'on)
       (parse-for-on var rest))
      ;; FOR var ACROSS vector
      ((loop-keyword-eq (first rest) 'across)
       (parse-for-across var rest))
      ;; FOR var = expr [THEN step-expr]
      ((loop-keyword-eq (first rest) '=)
       (parse-for-equals var rest))
      ;; FOR var BEING THE HASH-KEYS/HASH-VALUES OF hash-table
      ((loop-keyword-eq (first rest) 'being)
       (parse-for-hash var rest))
      ;; Default: treat as = with no THEN
      (t
       (values (make-loop-iter-equals
                :var var
                :clause-type :equals
                :init-form nil
                :then-form nil)
               rest)))))

(defun parse-for-arithmetic (var clauses)
  "Parse FOR var FROM/TO/BY arithmetic iteration."
  (let ((clause (make-loop-iter-arithmetic
                 :var var
                 :clause-type :arithmetic))
        (rest clauses))
    ;; Parse modifiers
    (loop while (and rest (loop-keyword-p (first rest))) do
      (let ((kw (first rest))
            (val (second rest)))
        (cond
          ((loop-keyword-eq kw 'from)
           (setf (loop-iter-arithmetic-from clause) val)
           (setf rest (cddr rest)))
          ((loop-keyword-eq kw 'upfrom)
           (setf (loop-iter-arithmetic-upfrom clause) val)
           (setf rest (cddr rest)))
          ((loop-keyword-eq kw 'downfrom)
           (setf (loop-iter-arithmetic-downfrom clause) val)
           (setf rest (cddr rest)))
          ((loop-keyword-eq kw 'to)
           (setf (loop-iter-arithmetic-to clause) val)
           (setf rest (cddr rest)))
          ((loop-keyword-eq kw 'below)
           (setf (loop-iter-arithmetic-below clause) val)
           (setf rest (cddr rest)))
          ((loop-keyword-eq kw 'above)
           (setf (loop-iter-arithmetic-above clause) val)
           (setf rest (cddr rest)))
          ((loop-keyword-eq kw 'downto)
           (setf (loop-iter-arithmetic-downto clause) val)
           (setf rest (cddr rest)))
          ((loop-keyword-eq kw 'by)
           (setf (loop-iter-arithmetic-by clause) val)
           (setf rest (cddr rest)))
          (t (return)))))
    (values clause rest)))

(defun parse-for-in (var clauses)
  "Parse FOR var IN list iteration."
  (let ((list-form (second clauses))
        (rest (cddr clauses))
        (step-fn nil)
        (list-var (gensym "LOOP-LIST-")))
    ;; Check for BY modifier
    (when (loop-keyword-eq (first rest) 'by)
      (setf step-fn (second rest))
      (setf rest (cddr rest)))
    (values (make-loop-iter-in
             :var var
             :clause-type :in
             :list-form list-form
             :step-fn step-fn
             :list-var list-var)
            rest)))

(defun parse-for-on (var clauses)
  "Parse FOR var ON list iteration."
  (let ((list-form (second clauses))
        (rest (cddr clauses))
        (step-fn nil)
        (list-var (gensym "LOOP-LIST-")))
    ;; Check for BY modifier
    (when (loop-keyword-eq (first rest) 'by)
      (setf step-fn (second rest))
      (setf rest (cddr rest)))
    (values (make-loop-iter-on
             :var var
             :clause-type :on
             :list-form list-form
             :step-fn step-fn
             :list-var list-var)
            rest)))

(defun parse-for-across (var clauses)
  "Parse FOR var ACROSS vector iteration."
  (let ((vector-form (second clauses))
        (rest (cddr clauses)))
    (values (make-loop-iter-across
             :var var
             :clause-type :across
             :vector-form vector-form
             :index-var (gensym "LOOP-IDX-")
             :vec-var (gensym "LOOP-VEC-"))
            rest)))

(defun parse-for-equals (var clauses)
  "Parse FOR var = expr [THEN step-expr] iteration."
  (let ((init-form (second clauses))
        (rest (cddr clauses))
        (then-form nil))
    ;; Check for THEN modifier
    (when (loop-keyword-eq (first rest) 'then)
      (setf then-form (second rest))
      (setf rest (cddr rest)))
    (values (make-loop-iter-equals
             :var var
             :clause-type :equals
             :init-form init-form
             :then-form then-form)
            rest)))

(defun parse-for-hash (var clauses)
  "Parse FOR var BEING THE HASH-KEYS/HASH-VALUES OF hash-table."
  ;; BEING THE HASH-KEYS OF table
  (let* ((rest (rest clauses))  ; skip BEING
         (_ (when (loop-keyword-eq (first rest) 'the)
              (setf rest (rest rest))))  ; skip THE
         (mode (cond
                 ((loop-keyword-eq (first rest) 'hash-keys) :keys)
                 ((loop-keyword-eq (first rest) 'hash-values) :values)
                 (t :keys)))
         (rest2 (rest rest)))  ; skip HASH-KEYS/HASH-VALUES
    (declare (ignore _))
    ;; Skip OF or IN
    (when (or (loop-keyword-eq (first rest2) 'of)
              (loop-keyword-eq (first rest2) 'in))
      (setf rest2 (rest rest2)))
    (let ((hash-form (first rest2))
          (final-rest (rest rest2)))
      (values (make-loop-iter-hash
               :var var
               :clause-type (if (eq mode :keys) :hash-keys :hash-values)
               :hash-form hash-form
               :mode mode)
              final-rest))))

(defun parse-with-clause (clauses ctx)
  "Parse WITH var [= expr] [AND var2 [= expr2] ...]. Returns (values bindings rest)."
  (declare (ignore ctx))
  (let ((bindings nil)
        (rest clauses))
    (loop
      (let ((var (first rest)))
        (setf rest (rest rest))
        (if (loop-keyword-eq (first rest) '=)
            (progn
              (push (list var (second rest)) bindings)
              (setf rest (cddr rest)))
            (push (list var nil) bindings))
        ;; Check for AND - if present, skip it and continue parsing
        (if (loop-keyword-eq (first rest) 'and)
            (setf rest (rest rest))  ; consume AND keyword
            (return))))  ; no AND, done with WITH clause
    (values (nreverse bindings) rest)))

(defun parse-accumulation-clause (keyword clauses ctx)
  "Parse COLLECT/SUM/COUNT/etc accumulation clause."
  (declare (ignore ctx))
  (let* ((type (intern (string-upcase
                        (string-right-trim "ING"
                                           (symbol-name keyword)))
                       :keyword))
         (expr (first clauses))
         (rest (rest clauses))
         (into-var nil))
    ;; Check for INTO
    (when (loop-keyword-eq (first rest) 'into)
      (setf into-var (second rest))
      (setf rest (cddr rest)))
    (values (make-loop-accumulation-clause
             :type type
             :expr expr
             :into-var into-var
             :acc-var (or into-var (gensym "LOOP-ACC-")))
            rest)))

(defun parse-termination-clause (keyword clauses ctx)
  "Parse WHILE/UNTIL termination clause."
  (declare (ignore ctx))
  (let ((type (if (loop-keyword-eq keyword 'while) :while :until))
        (expr (first clauses)))
    (values (make-loop-termination-clause
             :type type
             :expr expr)
            (rest clauses))))

(defun parse-boolean-clause (keyword clauses ctx)
  "Parse ALWAYS/NEVER/THEREIS boolean aggregation clause."
  (declare (ignore ctx))
  (let ((type (cond
                ((loop-keyword-eq keyword 'always) :always)
                ((loop-keyword-eq keyword 'never) :never)
                (t :thereis)))
        (expr (first clauses)))
    (values (make-loop-termination-clause
             :type type
             :expr expr)
            (rest clauses))))

(defun parse-conditional-clause (keyword clauses ctx)
  "Parse IF/WHEN/UNLESS conditional clause."
  (declare (ignore ctx))
  (let* ((type (cond
                 ((loop-keyword-eq keyword 'if) :if)
                 ((loop-keyword-eq keyword 'when) :when)
                 (t :unless)))
         (condition (first clauses))
         (rest (rest clauses))
         (then-clauses nil)
         (else-clauses nil))
    ;; Parse then-clauses until ELSE/END or another major keyword
    (multiple-value-bind (forms remaining)
        (parse-conditional-body rest)
      (setf then-clauses forms)
      (setf rest remaining))
    ;; Check for ELSE
    (when (loop-keyword-eq (first rest) 'else)
      (multiple-value-bind (forms remaining)
          (parse-conditional-body (rest rest))
        (setf else-clauses forms)
        (setf rest remaining)))
    ;; Check for END
    (when (loop-keyword-eq (first rest) 'end)
      (setf rest (rest rest)))
    (values (make-loop-conditional-clause
             :type type
             :condition condition
             :then-clauses then-clauses
             :else-clauses else-clauses)
            rest)))

(defun parse-conditional-body (clauses)
  "Parse forms in a conditional body until ELSE/END/major keyword.
   Handles DO/DOING prefix and accumulation keywords as ANSI CL requires."
  (let ((forms nil)
        (rest clauses))
    ;; Check for optional DO/DOING prefix (valid inside conditionals)
    (when (and rest
               (symbolp (first rest))
               (or (loop-keyword-eq (first rest) 'do)
                   (loop-keyword-eq (first rest) 'doing)))
      (setf rest (rest rest)))  ; skip DO/DOING
    (loop while rest do
      (let ((form (first rest)))
        ;; Stop at ELSE, END, or major clause keywords
        (when (and (symbolp form)
                   (or (loop-keyword-eq form 'else)
                       (loop-keyword-eq form 'end)
                       (member form '(for as with while until
                                      always never thereis return
                                      initially finally
                                      if when unless named)
                               :test #'loop-keyword-eq)))
          (return))
        ;; Handle AND chaining - may have optional DO/DOING after AND
        (when (loop-keyword-eq form 'and)
          (setf rest (rest rest))
          ;; Skip optional DO/DOING after AND
          (when (and rest
                     (symbolp (first rest))
                     (or (loop-keyword-eq (first rest) 'do)
                         (loop-keyword-eq (first rest) 'doing)))
            (setf rest (rest rest)))
          (setf form (first rest)))
        ;; Handle accumulation keywords specially - they consume an expression
        (cond
          ((member form '(collect collecting sum summing count counting
                          maximize maximizing minimize minimizing
                          append appending nconc nconcing)
                   :test #'loop-keyword-eq)
           ;; Accumulation: form + expr = two tokens
           (let ((acc-keyword form)
                 (acc-expr (second rest)))
             ;; Check for INTO
             (setf rest (cddr rest))
             (when (loop-keyword-eq (first rest) 'into)
               (setf rest (cddr rest)))  ; skip INTO and var
             (push (list acc-keyword acc-expr) forms)))
          (t
           (push form forms)
           (setf rest (rest rest))))))
    (values (nreverse forms) rest)))

(defun parse-do-forms (clauses)
  "Parse forms after DO/DOING until next keyword."
  (let ((forms nil)
        (rest clauses))
    (loop while rest do
      (let ((form (first rest)))
        (when (and (symbolp form) (loop-keyword-p form))
          (return))
        (push form forms)
        (setf rest (rest rest))))
    (values (nreverse forms) rest)))

(defun parse-compound-forms (clauses)
  "Parse compound forms (for INITIALLY/FINALLY)."
  (parse-do-forms clauses))

;;; -----------------------------------------------------------
;;; LOOP Expander (T011)
;;; -----------------------------------------------------------

(defun expand-loop (ctx)
  "Expand parsed LOOP context to tagbody-based code."
  (let* ((block-name (or (loop-context-name ctx) nil))
         (loop-start (gensym "LOOP-START-"))
         (loop-end (gensym "LOOP-END-"))
         ;; Generate variable bindings
         (iter-bindings (generate-iteration-bindings ctx))
         (acc-bindings (generate-accumulator-bindings ctx))
         (with-bindings (loop-context-with-bindings ctx))
         (all-bindings (append iter-bindings acc-bindings with-bindings))
         ;; Generate code sections
         (termination-tests (generate-termination-tests ctx loop-end))
         (body-code (generate-body-code ctx))
         (acc-updates (generate-accumulator-updates ctx))
         (step-code (generate-iteration-steps ctx))
         (result-form (generate-result-form ctx)))
    ;; Build the expansion
    ;; Feature 043: Use LET* for sequential bindings (FOR vars may depend on earlier WITH vars)
    `(let* ,all-bindings
       (block ,block-name
         (tagbody
            ,@(loop-context-initially-forms ctx)
            ,loop-start
            ,@termination-tests
            ,@body-code
            ,@acc-updates
            ,@step-code
            (go ,loop-start)
            ,loop-end)
         ,@(loop-context-finally-forms ctx)
         ,result-form))))

;;; -----------------------------------------------------------
;;; LOOP Code Generation Helpers
;;; -----------------------------------------------------------

(defun generate-iteration-bindings (ctx)
  "Generate bindings for iteration variables."
  (let ((bindings nil))
    (dolist (clause (loop-context-iteration-clauses ctx))
      (let ((var (loop-iteration-clause-var clause)))
        (cond
          ;; Arithmetic iteration
          ((loop-iter-arithmetic-p clause)
           (let ((init (or (loop-iter-arithmetic-from clause)
                           (loop-iter-arithmetic-upfrom clause)
                           (loop-iter-arithmetic-downfrom clause)
                           0)))
             (push (list var init) bindings)))
          ;; IN list iteration
          ((loop-iter-in-p clause)
           (let ((list-form (loop-iter-in-list-form clause))
                 (list-var (loop-iter-in-list-var clause)))
             (push (list list-var list-form) bindings)
             (push (list var `(car ,list-var)) bindings)))
          ;; ON list iteration
          ((loop-iter-on-p clause)
           (let ((list-form (loop-iter-on-list-form clause))
                 (list-var (loop-iter-on-list-var clause)))
             (push (list list-var list-form) bindings)
             (push (list var list-var) bindings)))
          ;; ACROSS vector iteration
          ((loop-iter-across-p clause)
           (let ((vec-var (loop-iter-across-vec-var clause))
                 (idx-var (loop-iter-across-index-var clause)))
             (push (list vec-var (loop-iter-across-vector-form clause)) bindings)
             (push (list idx-var 0) bindings)
             (push (list var `(aref ,vec-var ,idx-var)) bindings)))
          ;; = THEN iteration
          ((loop-iter-equals-p clause)
           (push (list var (loop-iter-equals-init-form clause)) bindings)))))
    (nreverse bindings)))

(defun extract-conditional-accum-types (clause)
  "Extract accumulation types from a conditional clause's body."
  (let ((types nil))
    (labels ((check-form (form)
               (when (and (listp form) (= 2 (length form))
                          (member (first form)
                                  '(collect collecting sum summing count counting
                                    maximize maximizing minimize minimizing
                                    append appending nconc nconcing)
                                  :test #'loop-keyword-eq))
                 (push (intern (string-upcase
                                (string-right-trim "ING" (symbol-name (first form))))
                               :keyword)
                       types))))
      (mapc #'check-form (loop-conditional-clause-then-clauses clause))
      (mapc #'check-form (loop-conditional-clause-else-clauses clause)))
    types))

(defun generate-accumulator-bindings (ctx)
  "Generate bindings for accumulator variables.
   Handles both top-level and conditional accumulations."
  (let ((bindings nil)
        (seen-types (make-hash-table :test 'eq))
        (type-to-var (make-hash-table :test 'eq)))
    ;; First collect all accumulation types including from conditionals
    (dolist (clause (loop-context-accumulation-clauses ctx))
      (let ((type (loop-accumulation-clause-type clause))
            (acc-var (loop-accumulation-clause-acc-var clause)))
        (unless (gethash type seen-types)
          (setf (gethash type seen-types) t)
          (setf (gethash type type-to-var) acc-var))))
    ;; Also scan body clauses for conditional accumulations
    (dolist (clause (loop-context-body-clauses ctx))
      (when (loop-conditional-clause-p clause)
        (dolist (type (extract-conditional-accum-types clause))
          (unless (gethash type seen-types)
            (setf (gethash type seen-types) t)
            (setf (gethash type type-to-var) (gensym "LOOP-ACC-"))))))
    ;; Store the type-to-var map in context for later use
    (setf (loop-context-result-form ctx) type-to-var)
    ;; Generate bindings for all seen accumulation types
    (maphash (lambda (type acc-var)
               (let ((init (case type
                             ((:collect :append :nconc) 'nil)
                             ((:sum :count) 0)
                             ((:maximize :minimize) 'nil))))
                 (push (list acc-var init) bindings)
                 ;; For COLLECT, add tail pointer for efficiency
                 (when (eq type :collect)
                   (let ((tail-var (gensym "LOOP-TAIL-")))
                     (push (list tail-var 'nil) bindings)))))
             type-to-var)
    (nreverse bindings)))

(defun generate-termination-tests (ctx loop-end)
  "Generate termination test code."
  (let ((tests nil))
    ;; Add iteration exhaustion tests
    (dolist (clause (loop-context-iteration-clauses ctx))
      (cond
        ;; Arithmetic with TO
        ((and (loop-iter-arithmetic-p clause)
              (loop-iter-arithmetic-to clause))
         (let ((var (loop-iteration-clause-var clause))
               (to (loop-iter-arithmetic-to clause)))
           (push `(if (> ,var ,to) (go ,loop-end)) tests)))
        ;; Arithmetic with BELOW
        ((and (loop-iter-arithmetic-p clause)
              (loop-iter-arithmetic-below clause))
         (let ((var (loop-iteration-clause-var clause))
               (below (loop-iter-arithmetic-below clause)))
           (push `(if (>= ,var ,below) (go ,loop-end)) tests)))
        ;; Arithmetic with DOWNTO
        ((and (loop-iter-arithmetic-p clause)
              (loop-iter-arithmetic-downto clause))
         (let ((var (loop-iteration-clause-var clause))
               (downto (loop-iter-arithmetic-downto clause)))
           (push `(if (< ,var ,downto) (go ,loop-end)) tests)))
        ;; Arithmetic with ABOVE
        ((and (loop-iter-arithmetic-p clause)
              (loop-iter-arithmetic-above clause))
         (let ((var (loop-iteration-clause-var clause))
               (above (loop-iter-arithmetic-above clause)))
           (push `(if (<= ,var ,above) (go ,loop-end)) tests)))
        ;; IN list - terminate when list is exhausted
        ((loop-iter-in-p clause)
         (let ((list-var (loop-iter-in-list-var clause)))
           (push `(if (null ,list-var) (go ,loop-end)) tests)))
        ;; ON list - terminate when list is exhausted
        ((loop-iter-on-p clause)
         (let ((list-var (loop-iter-on-list-var clause)))
           (push `(if (null ,list-var) (go ,loop-end)) tests)))
        ;; ACROSS vector - terminate when index reaches length
        ((loop-iter-across-p clause)
         (let ((idx-var (loop-iter-across-index-var clause))
               (vec-var (loop-iter-across-vec-var clause)))
           (push `(if (>= ,idx-var (length ,vec-var))
                      (go ,loop-end))
                 tests)))))
    ;; Add WHILE/UNTIL termination tests
    (dolist (clause (loop-context-termination-clauses ctx))
      (case (loop-termination-clause-type clause)
        (:while
         (push `(if (not ,(loop-termination-clause-expr clause))
                    (go ,loop-end))
               tests))
        (:until
         (push `(if ,(loop-termination-clause-expr clause)
                    (go ,loop-end))
               tests))))
    (nreverse tests)))

(defun generate-body-code (ctx)
  "Generate body code from DO and conditional clauses."
  (let ((code nil)
        ;; Get the type-to-var map stored during accumulator binding generation
        (acc-var-map (loop-context-result-form ctx)))
    ;; If acc-var-map is a hash table, use it; otherwise create new
    (unless (hash-table-p acc-var-map)
      (setf acc-var-map (make-hash-table :test 'eq)))
    (dolist (clause (loop-context-body-clauses ctx))
      (if (loop-conditional-clause-p clause)
          (push (expand-conditional-clause clause acc-var-map) code)
          (push clause code)))
    (nreverse code)))

(defun expand-conditional-form (form acc-var-map)
  "Expand a single form that may be an accumulation inside a conditional.
   ACC-VAR-MAP maps accumulation types to their accumulator variables."
  (if (and (listp form) (= 2 (length form))
           (member (first form) '(collect collecting sum summing count counting
                                  maximize maximizing minimize minimizing
                                  append appending nconc nconcing)
                   :test #'loop-keyword-eq))
      ;; This is an accumulation form - expand it
      (let* ((keyword (first form))
             (expr (second form))
             (type (intern (string-upcase
                            (string-right-trim "ING" (symbol-name keyword)))
                           :keyword))
             (acc-var (or (gethash type acc-var-map)
                          (gensym "LOOP-ACC-"))))
        ;; Store for later use
        (setf (gethash type acc-var-map) acc-var)
        ;; Return the update form
        (case type
          (:collect `(setq ,acc-var (nconc ,acc-var (list ,expr))))
          (:sum `(setq ,acc-var (+ ,acc-var ,expr)))
          (:count `(when ,expr (setq ,acc-var (+ ,acc-var 1))))
          (:maximize `(setq ,acc-var (if ,acc-var (max ,acc-var ,expr) ,expr)))
          (:minimize `(setq ,acc-var (if ,acc-var (min ,acc-var ,expr) ,expr)))
          (:append `(setq ,acc-var (append ,acc-var ,expr)))
          (:nconc `(setq ,acc-var (nconc ,acc-var ,expr)))))
      ;; Not an accumulation - return as-is
      form))

(defun expand-conditional-clause (clause &optional (acc-var-map nil))
  "Expand a conditional clause to IF/WHEN/UNLESS form.
   ACC-VAR-MAP is used to share accumulator variables across clauses."
  (let ((acc-var-map (or acc-var-map (make-hash-table :test 'eq)))
        (type (loop-conditional-clause-type clause))
        (cond-form (loop-conditional-clause-condition clause))
        (then-forms (loop-conditional-clause-then-clauses clause))
        (else-forms (loop-conditional-clause-else-clauses clause)))
    ;; Expand any accumulation forms in then/else clauses
    (let ((expanded-then (mapcar (lambda (f) (expand-conditional-form f acc-var-map))
                                 then-forms))
          (expanded-else (mapcar (lambda (f) (expand-conditional-form f acc-var-map))
                                 else-forms)))
      (case type
        ((:if :when)
         (if expanded-else
             `(if ,cond-form
                  (progn ,@expanded-then)
                  (progn ,@expanded-else))
             `(when ,cond-form ,@expanded-then)))
        (:unless
         (if expanded-else
             `(if (not ,cond-form)
                  (progn ,@expanded-then)
                  (progn ,@expanded-else))
             `(unless ,cond-form ,@expanded-then)))))))

(defun generate-accumulator-updates (ctx)
  "Generate accumulator update code."
  (let ((updates nil))
    (dolist (clause (loop-context-accumulation-clauses ctx))
      (let* ((type (loop-accumulation-clause-type clause))
             (expr (loop-accumulation-clause-expr clause))
             (acc-var (loop-accumulation-clause-acc-var clause)))
        (push
         (case type
           (:collect `(setq ,acc-var (nconc ,acc-var (list ,expr))))
           (:sum `(setq ,acc-var (+ ,acc-var ,expr)))
           (:count `(when ,expr (setq ,acc-var (+ ,acc-var 1))))
           (:maximize `(setq ,acc-var (if ,acc-var (max ,acc-var ,expr) ,expr)))
           (:minimize `(setq ,acc-var (if ,acc-var (min ,acc-var ,expr) ,expr)))
           (:append `(setq ,acc-var (append ,acc-var ,expr)))
           (:nconc `(setq ,acc-var (nconc ,acc-var ,expr))))
         updates)))
    (nreverse updates)))

(defun generate-iteration-steps (ctx)
  "Generate iteration stepping code using psetq for parallel stepping."
  (let ((step-pairs nil))
    (dolist (clause (loop-context-iteration-clauses ctx))
      (cond
        ;; Arithmetic stepping
        ((loop-iter-arithmetic-p clause)
         (let ((var (loop-iteration-clause-var clause))
               (by (or (loop-iter-arithmetic-by clause) 1))
               (descending (or (loop-iter-arithmetic-downto clause)
                               (loop-iter-arithmetic-downfrom clause)
                               (loop-iter-arithmetic-above clause))))
           (push var step-pairs)
           (push (if descending
                     `(- ,var ,by)
                     `(+ ,var ,by))
                 step-pairs)))
        ;; IN list stepping - step the list-var, then update var
        ((loop-iter-in-p clause)
         (let ((var (loop-iteration-clause-var clause))
               (list-var (loop-iter-in-list-var clause))
               (step-fn (or (loop-iter-in-step-fn clause) 'cdr)))
           ;; Step the list variable
           (push list-var step-pairs)
           (push `(,step-fn ,list-var) step-pairs)
           ;; Update the loop variable to the new car
           (push var step-pairs)
           (push `(car ,list-var) step-pairs)))
        ;; ON list stepping - step the list-var, var is the same
        ((loop-iter-on-p clause)
         (let ((var (loop-iteration-clause-var clause))
               (list-var (loop-iter-on-list-var clause))
               (step-fn (or (loop-iter-on-step-fn clause) 'cdr)))
           ;; Step the list variable
           (push list-var step-pairs)
           (push `(,step-fn ,list-var) step-pairs)
           ;; Update the loop variable (which is the same as list-var content)
           (push var step-pairs)
           (push list-var step-pairs)))
        ;; ACROSS vector stepping
        ((loop-iter-across-p clause)
         (let ((var (loop-iteration-clause-var clause))
               (idx-var (loop-iter-across-index-var clause))
               (vec-var (loop-iter-across-vec-var clause)))
           ;; Increment the index
           (push idx-var step-pairs)
           (push `(+ ,idx-var 1) step-pairs)
           ;; Update the loop variable
           (push var step-pairs)
           (push `(aref ,vec-var ,idx-var) step-pairs)))
        ;; = THEN stepping
        ((and (loop-iter-equals-p clause)
              (loop-iter-equals-then-form clause))
         (let ((var (loop-iteration-clause-var clause)))
           (push var step-pairs)
           (push (loop-iter-equals-then-form clause) step-pairs)))))
    (when step-pairs
      (list (cons 'psetq (nreverse step-pairs))))))

(defun generate-result-form (ctx)
  "Generate the result form for the LOOP.
   Handles both top-level and conditional accumulations."
  (let ((accumulators (loop-context-accumulation-clauses ctx))
        (type-to-var (loop-context-result-form ctx)))
    (cond
      ;; Has top-level accumulators - use first one
      (accumulators
       (loop-accumulation-clause-acc-var (first accumulators)))
      ;; Check for conditional accumulations in type-to-var map
      ((and (hash-table-p type-to-var) (> (hash-table-count type-to-var) 0))
       ;; Return the first accumulator found
       (block find-acc
         (maphash (lambda (type acc-var)
                    (declare (ignore type))
                    (return-from find-acc acc-var))
                  type-to-var)
         'nil))
      ;; No accumulation -> return NIL
      (t 'nil))))

;;; -----------------------------------------------------------
;;; LOOP Macro Expander Factory (T012)
;;; -----------------------------------------------------------

(defun make-loop-expander ()
  "Create a macro expander for LOOP."
  (lambda (form)
    (let* ((clauses (rest form))
           (ctx (make-loop-context)))
      ;; Handle simple infinite loop: (loop body...)
      (if (and clauses
               (not (symbolp (first clauses))))
          ;; Simple loop - just wrap in infinite loop
          `(block nil
             (tagbody
                loop-start
                ,@clauses
                (go loop-start)))
          ;; Extended loop - parse and expand
          (progn
            (parse-loop-clauses clauses ctx)
            (expand-loop ctx))))))

;;; ============================================================
;;; Typecase Macro Infrastructure (030-typecase-macros)
;;; ============================================================

;;; -----------------------------------------------------------
;;; Type Specifier to Predicate Conversion (T004)
;;; -----------------------------------------------------------

(defun type-specifier-to-predicate (typespec value-sym)
  "Convert a type specifier to a predicate form.
   TYPESPEC: Type specifier (symbol or list)
   VALUE-SYM: Symbol to test
   Returns: Form that evaluates to T or NIL

   Examples:
     (type-specifier-to-predicate 'integer 'x) => (integerp x)
     (type-specifier-to-predicate '(or integer symbol) 'x)
       => (or (integerp x) (symbolp x))"
  (cond
    ;; Atomic type specifiers
    ((eq typespec 'integer) (list 'integerp value-sym))
    ((eq typespec 'fixnum) (list 'integerp value-sym))  ; fixnum = integer in Clysm
    ((eq typespec 'symbol) (list 'symbolp value-sym))
    ((eq typespec 'cons) (list 'consp value-sym))
    ((eq typespec 'null) (list 'null value-sym))
    ((eq typespec 'list) (list 'listp value-sym))
    ((eq typespec 'number) (list 'numberp value-sym))
    ((eq typespec 'float) (list 'floatp value-sym))
    ((eq typespec 'single-float) (list 'floatp value-sym))
    ((eq typespec 'double-float) (list 'floatp value-sym))
    ((eq typespec 'ratio) (list 'ratiop value-sym))
    ((eq typespec 'rational) (list 'rationalp value-sym))
    ((eq typespec 'real) (list 'realp value-sym))
    ((eq typespec 'character) (list 'characterp value-sym))
    ((eq typespec 'function) (list 'functionp value-sym))
    ((eq typespec 'string) (list 'stringp value-sym))
    ((eq typespec 'vector) (list 'vectorp value-sym))
    ((eq typespec 'array) (list 'arrayp value-sym))
    ((eq typespec 'hash-table) (list 'hash-table-p value-sym))
    ((eq typespec 'package) (list 'packagep value-sym))
    ((eq typespec 'stream) (list 'streamp value-sym))
    ((eq typespec 'keyword) (list 'keywordp value-sym))
    ((eq typespec 'atom) (list 'atom value-sym))
    ((eq typespec 't) t)
    ;; Compound type specifiers
    ((and (listp typespec) (eq (car typespec) 'or))
     (cons 'or (mapcar (lambda (ts) (type-specifier-to-predicate ts value-sym))
                       (cdr typespec))))
    ((and (listp typespec) (eq (car typespec) 'and))
     (cons 'and (mapcar (lambda (ts) (type-specifier-to-predicate ts value-sym))
                        (cdr typespec))))
    ((and (listp typespec) (eq (car typespec) 'not))
     (list 'not (type-specifier-to-predicate (cadr typespec) value-sym)))
    ((and (listp typespec) (eq (car typespec) 'member))
     (cons 'or (mapcar (lambda (item) (list 'eql value-sym (list 'quote item)))
                       (cdr typespec))))
    ((and (listp typespec) (eq (car typespec) 'satisfies))
     (list 'funcall (list 'function (cadr typespec)) value-sym))
    ((and (listp typespec) (eq (car typespec) 'eql))
     (list 'eql value-sym (list 'quote (cadr typespec))))
    ;; Unknown type - fall back to typep
    (t (list 'typep value-sym (list 'quote typespec)))))

;;; -----------------------------------------------------------
;;; Expected Type Construction (T005)
;;; -----------------------------------------------------------

(defun construct-expected-type (clauses)
  "Construct expected-type from typecase clauses for type-error.
   CLAUSES: List of (type-specifier . body-forms)
   Returns: Type specifier (or type1 type2 ...)

   Example:
     (construct-expected-type '((integer ...) (symbol ...) (cons ...)))
       => (or integer symbol cons)"
  (let ((types nil))
    (dolist (clause clauses)
      (let ((type-spec (first clause)))
        (cond
          ;; Multiple types in a list: ((type1 type2) body...)
          ((and (listp type-spec) (not (member (car type-spec) '(or and not member satisfies eql))))
           (dolist (ts type-spec)
             (push ts types)))
          ;; Single type
          (t
           (push type-spec types)))))
    (setf types (nreverse types))
    (if (= 1 (length types))
        (first types)
        (cons 'or types))))

;;; -----------------------------------------------------------
;;; Exhaustive Clause Validation (T006)
;;; -----------------------------------------------------------

(defun validate-exhaustive-clauses (clauses macro-name)
  "Validate that clauses do not contain otherwise/t (for etypecase/ctypecase).
   CLAUSES: List of clause forms
   MACRO-NAME: Symbol for error message (etypecase or ctypecase)
   Signals: error if otherwise or t clause found"
  (dolist (clause clauses)
    (let ((type-spec (first clause)))
      (when (or (eq type-spec 'otherwise)
                (eq type-spec 't))
        (error "~A: ~A clause not allowed in ~A"
               macro-name type-spec macro-name)))))

;;; -----------------------------------------------------------
;;; Typecase Macro Registration (T007, T008)
;;; -----------------------------------------------------------

;;; -----------------------------------------------------------
;;; Typecase Macro Expander (T015-T018)
;;; -----------------------------------------------------------

(defun make-typecase-expander ()
  "Create a macro expander for TYPECASE.
   (typecase keyform (type form...) ... [(otherwise|t form...)])
   Expands to a let binding the keyform, then nested if/type-predicate tests.

   FR-001: Evaluates keyform once and dispatches based on type
   FR-002: Expands to nested if forms with predicate tests
   FR-003: Supports multiple type specifiers per clause ((type1 type2) body)
   FR-004: Supports otherwise and t as catch-all clause keys
   FR-005: Returns NIL when no clause matches and no otherwise exists
   FR-006: Evaluates the keyform exactly once, binding to a gensym"
  (lambda (form)
    (let ((keyform (second form))
          (clauses (cddr form))
          (key-var (gensym "KEY-")))
      (labels ((otherwise-clause-p (clause)
                 (let ((type-spec (first clause)))
                   (or (eq type-spec 'otherwise)
                       (eq type-spec 't))))
               (make-type-test (key-var type-spec)
                 ;; Handle multiple type specifiers: ((type1 type2) body)
                 (cond
                   ;; List of multiple types
                   ((and (listp type-spec)
                         (not (member (car type-spec) '(or and not member satisfies eql))))
                    (if (= 1 (length type-spec))
                        (type-specifier-to-predicate (first type-spec) key-var)
                        (cons 'or (mapcar (lambda (ts)
                                            (type-specifier-to-predicate ts key-var))
                                          type-spec))))
                   ;; Single type specifier
                   (t
                    (type-specifier-to-predicate type-spec key-var))))
               (expand-clauses (clauses)
                 (if (null clauses)
                     nil  ; FR-005: Return NIL when no match
                     (let* ((clause (first clauses))
                            (type-spec (first clause))
                            (body (rest clause)))
                       (if (otherwise-clause-p clause)
                           ;; FR-004: Otherwise/t clause - just execute body
                           (cons 'progn body)
                           ;; Normal clause - test and branch
                           (list 'if
                                 (make-type-test key-var type-spec)
                                 (cons 'progn body)
                                 (expand-clauses (rest clauses))))))))
        ;; FR-006: Bind keyform to gensym for single evaluation
        (list 'let (list (list key-var keyform))
              (expand-clauses clauses))))))

;;; -----------------------------------------------------------
;;; Etypecase Macro Expander (T027-T031)
;;; -----------------------------------------------------------

(defun make-etypecase-expander ()
  "Create a macro expander for ETYPECASE.
   (etypecase keyform (type form...) ...)
   Like typecase but signals type-error when no clause matches.
   Does NOT allow otherwise/t clauses.

   FR-007: Evaluates keyform once and dispatches based on type
   FR-008: Signals type-error with :datum and :expected-type when no match
   FR-009: otherwise/t clauses are NOT allowed
   FR-010: :expected-type is (or type1 type2 ...) of all clause types"
  (lambda (form)
    (let ((keyform (second form))
          (clauses (cddr form))
          (key-var (gensym "KEY-")))
      ;; FR-009: Validate no otherwise/t clauses
      (validate-exhaustive-clauses clauses 'etypecase)
      (let ((expected-type (construct-expected-type clauses)))
        (labels ((make-type-test (key-var type-spec)
                   ;; Handle multiple type specifiers: ((type1 type2) body)
                   (cond
                     ;; List of multiple types
                     ((and (listp type-spec)
                           (not (member (car type-spec) '(or and not member satisfies eql))))
                      (if (= 1 (length type-spec))
                          (type-specifier-to-predicate (first type-spec) key-var)
                          (cons 'or (mapcar (lambda (ts)
                                              (type-specifier-to-predicate ts key-var))
                                            type-spec))))
                     ;; Single type specifier
                     (t
                      (type-specifier-to-predicate type-spec key-var))))
                 (expand-clauses (clauses)
                   (if (null clauses)
                       ;; FR-008: No match - signal type-error
                       (list 'error 'type-error
                             :datum key-var
                             :expected-type (list 'quote expected-type))
                       (let* ((clause (first clauses))
                              (type-spec (first clause))
                              (body (rest clause)))
                         ;; Normal clause - test and branch
                         (list 'if
                               (make-type-test key-var type-spec)
                               (cons 'progn body)
                               (expand-clauses (rest clauses)))))))
          ;; Bind keyform to gensym for single evaluation
          (list 'let (list (list key-var keyform))
                (expand-clauses clauses)))))))

;;; -----------------------------------------------------------
;;; Check-Type Macro Expander (T040-T044)
;;; -----------------------------------------------------------

(defun make-check-type-expander ()
  "Create a macro expander for CHECK-TYPE.
   (check-type place typespec [type-string])
   Signals type-error with store-value restart if type doesn't match.
   Loops until type matches (after store-value restart).

   FR-015: Signals type-error with :datum and :expected-type if place is not of typespec
   FR-016: Returns NIL if type already matches
   FR-017: Provides store-value restart to set a new value
   FR-018: Re-validates after store-value restart (loop until correct)
   FR-019: Optional type-string included in error message"
  (lambda (form)
    (let* ((place (second form))
           (typespec (third form))
           (type-string (fourth form))
           (value-var (gensym "VALUE-")))
      ;; Build the type test predicate using place directly
      (let ((type-test (type-specifier-to-predicate typespec place)))
        ;; FR-018: Loop until type matches
        `(loop
           ;; Check if type matches
           (when ,(if (eq type-test t)
                      t
                      type-test)
             (return nil))  ; FR-016: Return NIL if correct
           ;; FR-015, FR-017: Signal error with store-value restart
           (restart-case
               (error 'type-error
                      :datum ,place
                      :expected-type ',typespec
                      ,@(when type-string
                          (list :format-control type-string)))
             (store-value (,value-var)
               :report "Supply a new value."
               :interactive (lambda () (list (read)))
               ;; Update the place
               (setf ,place ,value-var))))))))

;;; -----------------------------------------------------------
;;; Ctypecase Macro Expander (T053-T057)
;;; -----------------------------------------------------------

(defun make-ctypecase-expander ()
  "Create a macro expander for CTYPECASE.
   (ctypecase keyplace (type form...) ...)
   Like etypecase but provides store-value restart for correction.
   Loops until a type matches after store-value.
   Does NOT allow otherwise/t clauses.

   FR-011: Evaluates place value once per iteration and dispatches based on type
   FR-012: Signals type-error with :datum and :expected-type when no match
   FR-013: Provides store-value restart to set a new value and re-dispatch
   FR-014: otherwise/t clauses are NOT allowed"
  (lambda (form)
    (let ((keyplace (second form))
          (clauses (cddr form))
          (value-var (gensym "VALUE-")))
      ;; FR-014: Validate no otherwise/t clauses
      (validate-exhaustive-clauses clauses 'ctypecase)
      (let ((expected-type (construct-expected-type clauses)))
        (labels ((make-type-test (key-var type-spec)
                   ;; Handle multiple type specifiers: ((type1 type2) body)
                   (cond
                     ;; List of multiple types
                     ((and (listp type-spec)
                           (not (member (car type-spec) '(or and not member satisfies eql))))
                      (if (= 1 (length type-spec))
                          (type-specifier-to-predicate (first type-spec) key-var)
                          (cons 'or (mapcar (lambda (ts)
                                              (type-specifier-to-predicate ts key-var))
                                            type-spec))))
                     ;; Single type specifier
                     (t
                      (type-specifier-to-predicate type-spec key-var))))
                 (expand-clauses-with-return (clauses)
                   ;; Build nested if with returns on match
                   (if (null clauses)
                       nil
                       (let* ((clause (first clauses))
                              (type-spec (first clause))
                              (body (rest clause)))
                         (list 'if
                               (make-type-test keyplace type-spec)
                               (list 'return (cons 'progn body))
                               (expand-clauses-with-return (rest clauses)))))))
          ;; Loop until type matches
          `(loop
             ;; Try to match a clause - return if matched
             ,(expand-clauses-with-return clauses)
             ;; FR-012, FR-013: No match - signal error with store-value restart
             (restart-case
                 (error 'type-error
                        :datum ,keyplace
                        :expected-type ',expected-type)
               (store-value (,value-var)
                 :report "Supply a new value."
                 :interactive (lambda () (list (read)))
                 ;; Update the place and loop to re-dispatch
                 (setf ,keyplace ,value-var)))))))))

(defun install-typecase-macros (registry)
  "Install typecase-related macros into REGISTRY.
   Registers: typecase, etypecase, ctypecase, check-type"
  ;; Register typecase macro (US1)
  (clysm/compiler/transform/macro:register-macro
   registry 'typecase (make-typecase-expander))
  ;; Register etypecase macro (US2)
  (clysm/compiler/transform/macro:register-macro
   registry 'etypecase (make-etypecase-expander))
  ;; Register check-type macro (US4)
  (clysm/compiler/transform/macro:register-macro
   registry 'check-type (make-check-type-expander))
  ;; Register ctypecase macro (US3)
  (clysm/compiler/transform/macro:register-macro
   registry 'ctypecase (make-ctypecase-expander))
  registry)

;;; ============================================================
;;; Destructuring-Bind Macro Infrastructure (031-destructuring-bind-macro)
;;; ============================================================

(defun make-destructuring-bind-expander ()
  "Create a macro expander for DESTRUCTURING-BIND.
   (destructuring-bind lambda-list expression declaration* form*)
   Binds the variables specified in lambda-list to the corresponding values
   in the list resulting from the evaluation of expression.

   FR-001: Expands to code that binds variables according to lambda-list pattern
   FR-002: Supports required parameters in flat and nested patterns
   FR-003: Supports &optional parameters with default values and supplied-p
   FR-004: Supports &rest to capture remaining list elements
   FR-005: Supports &key parameters with defaults, supplied-p, and alternate names
   FR-006: Supports &body as a synonym for &rest
   FR-007: Supports &whole to bind entire list before destructuring
   FR-008: Supports &allow-other-keys to permit unrecognized keywords
   FR-009: Signals program-error when required parameters cannot be satisfied
   FR-010: Signals error when excess elements exist and no &rest/&body
   FR-011: Signals error when unrecognized keywords present without &allow-other-keys
   FR-012: Supports nested destructuring patterns at any depth
   FR-013: Handles dotted-list patterns
   FR-014: Evaluates default value forms only when parameter not supplied"
  (lambda (form)
    (let ((lambda-list (second form))
          (expression (third form))
          (body (cdddr form))
          (list-var (gensym "LIST-")))
      ;; Parse the lambda-list
      (let ((parsed-ll (clysm/lib/destructuring:parse-destructuring-lambda-list lambda-list)))
        ;; Generate the binding code
        (list 'let (list (list list-var expression))
              (clysm/lib/destructuring:generate-destructuring-code
               parsed-ll list-var body))))))

;;; ============================================================
;;; Standard macro installation
;;; ============================================================

(defun install-setf-macros (registry)
  "Install setf-related macros into REGISTRY."
  (clysm/compiler/transform/macro:register-macro
   registry 'setf (make-setf-expander))
  (clysm/compiler/transform/macro:register-macro
   registry 'psetf (make-psetf-expander))
  ;; Feature 043: PSETQ for LOOP macro support
  (clysm/compiler/transform/macro:register-macro
   registry 'psetq (make-psetq-expander))
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
  ;; Feature 043: RETURN macro for LOOP's finally clause
  (clysm/compiler/transform/macro:register-macro
   registry 'return (make-return-expander))
  (clysm/compiler/transform/macro:register-macro
   registry 'do (make-do-expander))
  (clysm/compiler/transform/macro:register-macro
   registry 'do* (make-do*-expander))
  (clysm/compiler/transform/macro:register-macro
   registry 'dolist (make-dolist-expander))
  (clysm/compiler/transform/macro:register-macro
   registry 'dotimes (make-dotimes-expander))
  ;; LOOP macro (029-loop-macro)
  (clysm/compiler/transform/macro:register-macro
   registry 'loop (make-loop-expander))
  ;; Also install setf-related macros
  (install-setf-macros registry)
  ;; Install typecase macros (030-typecase-macros)
  (install-typecase-macros registry)
  ;; Destructuring-bind macro (031-destructuring-bind-macro)
  (clysm/compiler/transform/macro:register-macro
   registry 'destructuring-bind (make-destructuring-bind-expander))
  registry)

;;; ============================================================
;;; Initialize Global Macro Registry at Load Time
;;; ============================================================
;;; Feature 043: Install all standard macros into the global registry
;;; so that compile-to-wasm can expand LOOP, DO, DOLIST, etc.

(eval-when (:load-toplevel :execute)
  (install-standard-macros (clysm/compiler/transform/macro:global-macro-registry)))
