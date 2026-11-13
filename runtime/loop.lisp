;;;; loop.lisp — the LOOP macro
;;;;
;;;; The expander is plain list processing (no LOOP inside), so that it runs on
;;;; the host at compile time and in the target for EVAL.

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun %lkw (x name)
    (and (symbolp x) x (string= (symbol-name x) name)))

  (defun %lkw-in (x names)
    (and (symbolp x) x (member (symbol-name x) names :test #'string=)))

  (defparameter *loop-clause-keywords*
    '("FOR" "AS" "WITH" "REPEAT" "WHILE" "UNTIL" "ALWAYS" "NEVER" "THEREIS"
      "COLLECT" "COLLECTING" "APPEND" "APPENDING" "NCONC" "NCONCING" "COUNT" "COUNTING"
      "SUM" "SUMMING" "MAXIMIZE" "MAXIMIZING" "MINIMIZE" "MINIMIZING" "DO" "DOING"
      "WHEN" "IF" "UNLESS" "ELSE" "END" "AND" "RETURN" "INITIALLY" "FINALLY" "NAMED"))

  (defun %loop-keyword-p (x) (%lkw-in x *loop-clause-keywords*))

  ;; parser state is kept in a simple vector:
  ;; 0 tokens, 1 bindings(rev), 2 pre(rev), 3 body(rev), 4 steps(rev), 5 initially(rev),
  ;; 6 finally(rev), 7 result-form, 8 name, 9 accumulators alist, 10 default-acc,
  ;; 11 end-tag, 12 first-flag var, 13 post-pre (rev), 14 ALWAYS/NEVER/THEREIS seen,
  ;; 15 variable types, 16 a FOR = clause seen
  (defun %loop-new-state (tokens)
    (let ((s (make-array 17 :initial-element nil)))
      (setf (svref s 0) tokens)
      (setf (svref s 11) (gensym "LOOP-END"))
      s))

  (defmacro %ls (s i) `(svref ,s ,i))

  (defun %loop-peek (s) (car (%ls s 0)))
  (defun %loop-pop (s) (let ((x (car (%ls s 0)))) (setf (%ls s 0) (cdr (%ls s 0))) x))
  (defun %loop-at-end (s) (null (%ls s 0)))

  (defun %loop-add-binding (s var init)
    (when (and (symbolp var) var (symbol-package var) (assoc var (%ls s 1)))
      (error 'simple-program-error :format-control "LOOP: the variable ~S is bound more than once"
                                   :format-arguments (list var)))
    (setf (%ls s 1) (cons (list var init) (%ls s 1))))

  (defun %loop-pattern-vars (pattern)
    (cond ((null pattern) nil)
          ((symbolp pattern) (list pattern))
          ((consp pattern) (append (%loop-pattern-vars (car pattern)) (%loop-pattern-vars (cdr pattern))))
          (t nil)))

  (defun %loop-destructure-setq (pattern form)
    "Returns a list of forms setting the variables of PATTERN from FORM."
    (cond ((null pattern) nil)
          ((symbolp pattern) (list `(setq ,pattern ,form)))
          ((consp pattern)
           (let ((g (gensym)))
             (list `(let ((,g ,form))
                      ,@(%loop-destructure-setq (car pattern) `(car ,g))
                      ,@(%loop-destructure-setq (cdr pattern) `(cdr ,g))))))
          (t nil)))

  (defun %loop-pattern-paths (pattern form)
    "(var expression) pairs destructuring FORM (a variable) by PATTERN."
    (cond ((null pattern) nil)
          ((symbolp pattern) (list (list pattern form)))
          ((consp pattern)
           (append (%loop-pattern-paths (car pattern) `(car ,form))
                   (%loop-pattern-paths (cdr pattern) `(cdr ,form))))
          (t nil)))

  (defun %loop-bind-pattern (s pattern)
    (dolist (v (%loop-pattern-vars pattern)) (%loop-add-binding s v nil)))

  (defun %loop-simple-type-p (x)
    (or (%lkw-in x '("FIXNUM" "FLOAT" "T" "NIL" "INTEGER" "NUMBER" "LIST" "SYMBOL" "SINGLE-FLOAT"
                     "DOUBLE-FLOAT" "SHORT-FLOAT" "LONG-FLOAT" "REAL" "RATIONAL"))
        (and (consp x) (%loop-simple-type-p (car x))
             (or (null (cdr x)) (%loop-simple-type-p (cdr x))))))

  (defun %loop-skip-type (s)
    ;; optional type spec after a variable: of-type x, or a simple type (a
    ;; tree of type symbols for destructuring); returns it
    (cond ((%lkw (%loop-peek s) "OF-TYPE") (%loop-pop s) (%loop-pop s))
          ((%loop-simple-type-p (%loop-peek s)) (%loop-pop s))))

  (defun %loop-note-type (s var type)
    "Record the type given for the variable VAR (declared in the expansion); returns TYPE."
    (when (and type (symbolp var) var (not (eq type t)))
      (setf (%ls s 15) (cons (cons var type) (%ls s 15))))
    type)

  (defun %loop-type-declarations (s)
    "Declarations for the typed variables: a variable that starts as NIL gets
the default value of its type (and no declaration when there is none)."
    (let ((r nil))
      (dolist (e (%ls s 15) r)
        (let ((b (assoc (car e) (%ls s 1))))
          (when b
            (cond ((cadr b) (push (list 'type (cdr e) (car e)) r))
                  ((%loop-type-default (cdr e))
                   (setf (cadr b) (%loop-type-default (cdr e)))
                   (push (list 'type (cdr e) (car e)) r))))))))

  (defun %loop-type-default (type)
    ;; the initial value of a WITH variable without an init form
    (cond ((%lkw-in type '("FIXNUM" "INTEGER" "NUMBER" "REAL" "RATIONAL")) 0)
          ((%lkw-in type '("FLOAT" "SINGLE-FLOAT" "SHORT-FLOAT")) 0.0)
          ((%lkw-in type '("DOUBLE-FLOAT" "LONG-FLOAT")) 0.0d0)
          ((and (consp type) (member (car type) '(integer mod unsigned-byte signed-byte))) 0)
          (t nil)))

  (defun %loop-pattern-defaults (pattern type)
    "(var default) pairs for the variables of PATTERN, with types from TYPE."
    (cond ((null pattern) nil)
          ((symbolp pattern) (list (list pattern (%loop-type-default type))))
          ((consp pattern)
           (append (%loop-pattern-defaults (car pattern) (if (consp type) (car type) type))
                   (%loop-pattern-defaults (cdr pattern) (if (consp type) (cdr type) type))))
          (t nil)))

  ;; the variable bound to the value of the test, while parsing the first
  ;; clause of a conditional (where IT is recognized)
  (defvar *%loop-it* nil)

  (defun %loop-end-test (s form)
    (setf (%ls s 2) (cons `(when ,form (go ,(%ls s 11))) (%ls s 2))))

  (defun %loop-pre (s form) (setf (%ls s 2) (cons form (%ls s 2))))

  ;; The termination test of a clause over a list or a vector is also made
  ;; right after its step, so that the clauses after it are not stepped when
  ;; it ends the loop; but not after a FOR = clause, which is stepped with
  ;; the PRE forms: there the test in the PRE forms comes after that
  ;; clause's step, in the order of the clauses (FOR A = 0 THEN B FOR (C B)
  ;; IN LIST: A is the last B when the loop ends, as in SBCL).
  (defun %loop-step-test (s form)
    (unless (%ls s 16)
      (%loop-step s `(when ,form (go ,(%ls s 11))))))
  (defun %loop-step (s form) (setf (%ls s 4) (cons form (%ls s 4))))

  (defun %loop-parse-for (s &optional group-steps later)
    ;; GROUP-STEPS: the step list when the group of FOR clauses joined by AND
    ;; started; LATER: this is not the first clause of the group
    (let ((var (%loop-pop s)))
      (%loop-note-type s var (%loop-skip-type s))
      (let ((kw (%loop-pop s)))
        (cond
          ((%lkw-in kw '("IN" "ON"))
           (let ((list-form (%loop-pop s)) (g (gensym "LIST")) (step-fn nil))
             (%loop-add-binding s g list-form)
             (when (%lkw (%loop-peek s) "BY")
               (%loop-pop s)
               (setq step-fn (gensym "STEP-FN"))
               (%loop-add-binding s step-fn (%loop-pop s)))
             (%loop-bind-pattern s var)
             (if (%lkw kw "IN")
                 (progn
                   (%loop-end-test s `(endp ,g))
                   (dolist (f (%loop-destructure-setq var `(car ,g))) (%loop-pre s f)))
                 (progn
                   (%loop-end-test s `(atom ,g))
                   (dolist (f (%loop-destructure-setq var g)) (%loop-pre s f))))
             ;; each clause is stepped and then tested (the clauses after
             ;; it are not stepped when it terminates the loop)
             (%loop-step s `(setq ,g ,(if step-fn `(funcall ,step-fn ,g) `(cdr ,g))))
             (%loop-step-test s (if (%lkw kw "IN") `(endp ,g) `(atom ,g)))))
          ((%lkw kw "ACROSS")
           (let ((vec (gensym "VEC")) (i (gensym "I")))
             (%loop-add-binding s vec (%loop-pop s))
             (%loop-add-binding s i 0)
             (%loop-bind-pattern s var)
             (%loop-end-test s `(>= ,i (length ,vec)))
             (dolist (f (%loop-destructure-setq var `(aref ,vec ,i))) (%loop-pre s f))
             (%loop-step s `(setq ,i (+ ,i 1)))
             (%loop-step-test s `(>= ,i (length ,vec)))))
          ((%lkw kw "=")
           (setf (%ls s 16) t)
           (let ((init (%loop-pop s)))
             (%loop-bind-pattern s var)
             (if (%lkw (%loop-peek s) "THEN")
                 (let ((then (progn (%loop-pop s) (%loop-pop s))))
                   (unless (%ls s 12) (setf (%ls s 12) (gensym "FIRST")))
                   (if (or later (%lkw (%loop-peek s) "AND"))
                       ;; stepped in parallel with the other clauses of the
                       ;; group: the THEN form is evaluated before their steps
                       (let ((tmp (gensym "NEXT")) (steps (%ls s 4)) (new nil))
                         (%loop-add-binding s tmp nil)
                         (do () ((eq steps group-steps)) (push (car steps) new) (setq steps (cdr steps)))
                         (setf (%ls s 4) (append (reverse new) (list `(setq ,tmp ,then)) group-steps))
                         (%loop-pre s `(if ,(%ls s 12)
                                           (progn ,@(%loop-destructure-setq var init))
                                           (progn ,@(%loop-destructure-setq var tmp)))))
                       (%loop-pre s `(if ,(%ls s 12)
                                         (progn ,@(%loop-destructure-setq var init))
                                         (progn ,@(%loop-destructure-setq var then))))))
                 (dolist (f (%loop-destructure-setq var init)) (%loop-pre s f)))))
          ((%lkw-in kw '("BEING" "EACH" "THE"))
           (%loop-parse-being s var kw))
          ((%lkw-in kw '("FROM" "UPFROM" "DOWNFROM" "TO" "UPTO" "BELOW" "DOWNTO" "ABOVE" "BY"))
           (%loop-parse-arith s var kw))
          (t (error "LOOP: unknown FOR clause keyword ~S" kw))))))

  (defun %loop-parse-arith (s var kw)
    ;; the FROM/TO/BY forms are evaluated in the order they appear
    (let ((from 0) (from-seen nil) (dir nil) (inclusive t) (have-to nil) (have-by nil)
          (lim (gensym "LIMIT")) (step (gensym "STEP")) (order nil))
      (when (null var) (setq var (gensym "VAR")))
      (setf (%ls s 0) (cons kw (%ls s 0)))
      (do () ((not (%lkw-in (%loop-peek s) '("FROM" "UPFROM" "DOWNFROM" "TO" "UPTO" "BELOW"
                                              "DOWNTO" "ABOVE" "BY"))))
        (let ((k (%loop-pop s)) (form nil))
          (setq form (%loop-pop s))
          (cond ((%lkw-in k '("FROM" "UPFROM" "DOWNFROM"))
                 (setq from form from-seen t)
                 (when (%lkw k "UPFROM") (setq dir :up))
                 (when (%lkw k "DOWNFROM") (setq dir :down))
                 (push (list var form) order))
                ((%lkw-in k '("TO" "UPTO" "BELOW" "DOWNTO" "ABOVE"))
                 (setq have-to t)
                 (when (%lkw-in k '("BELOW" "ABOVE")) (setq inclusive nil))
                 (when (%lkw-in k '("UPTO" "BELOW")) (setq dir (or dir :up)))
                 (when (%lkw-in k '("DOWNTO" "ABOVE")) (setq dir :down))
                 (push (list lim form) order))
                (t (setq have-by t) (push (list step form) order)))))
      (unless from-seen (push (list var from) order))
      (unless have-by (push (list step 1) order))
      (dolist (b (reverse order)) (%loop-add-binding s (car b) (cadr b)))
      (let ((down (eq dir :down)))
        (when have-to
          (%loop-end-test s (if down
                                (if inclusive `(< ,var ,lim) `(<= ,var ,lim))
                                (if inclusive `(> ,var ,lim) `(>= ,var ,lim)))))
        ;; stepped, then tested: after the loop the variable has the first
        ;; value beyond the limit (as in the MIT LOOP)
        (%loop-step s `(setq ,var ,(if down `(- ,var ,step) `(+ ,var ,step))))
        (when have-to
          (%loop-step s `(when ,(if down
                                    (if inclusive `(< ,var ,lim) `(<= ,var ,lim))
                                    (if inclusive `(> ,var ,lim) `(>= ,var ,lim)))
                           (go ,(%ls s 11))))))))

  (defun %loop-parse-being (s var kw)
    (when (%lkw-in kw '("BEING")) (when (%lkw-in (%loop-peek s) '("THE" "EACH")) (%loop-pop s)))
    (let ((what (%loop-pop s)) (obj nil))
      (cond ((%lkw-in (%loop-peek s) '("OF" "IN")) (%loop-pop s) (setq obj (%loop-pop s)))
            ((%lkw-in what '("SYMBOL" "SYMBOLS" "PRESENT-SYMBOL" "PRESENT-SYMBOLS"
                             "EXTERNAL-SYMBOL" "EXTERNAL-SYMBOLS"))
             (setq obj '*package*))
            (t (error 'simple-program-error :format-control "LOOP: expected OF after ~S"
                                            :format-arguments (list what))))
      (let ((other nil) (g (gensym "ENTRIES")))
        (when (%lkw (%loop-peek s) "USING")
          (%loop-pop s)
          (setq other (cadr (%loop-pop s))))
        (cond
          ((%lkw-in what '("HASH-KEY" "HASH-KEYS" "HASH-VALUE" "HASH-VALUES"))
           (let ((keysp (%lkw-in what '("HASH-KEY" "HASH-KEYS"))))
             ;; the entries are visited in place, by index (-1 after the last)
             (let ((i (gensym "INDEX")))
               (%loop-add-binding s g `(%ht-checked ,obj))
               (%loop-add-binding s i `(%ht-next-index ,g 0))
               (%loop-bind-pattern s var)
               (when other (%loop-bind-pattern s other))
               (%loop-end-test s `(< ,i 0))
               (dolist (f (%loop-destructure-setq var (if keysp `(%ht-key-at ,g ,i) `(%ht-value-at ,g ,i))))
                 (%loop-pre s f))
               (when other
                 (dolist (f (%loop-destructure-setq other (if keysp `(%ht-value-at ,g ,i) `(%ht-key-at ,g ,i))))
                   (%loop-pre s f)))
               (%loop-step s `(setq ,i (%ht-next-index ,g (+ ,i 1)))))))
          ((%lkw-in what '("SYMBOL" "SYMBOLS" "PRESENT-SYMBOL" "PRESENT-SYMBOLS"
                           "EXTERNAL-SYMBOL" "EXTERNAL-SYMBOLS"))
           (%loop-add-binding s g `(%package-symbol-list ,obj
                                                         ,(cond ((%lkw-in what '("EXTERNAL-SYMBOL" "EXTERNAL-SYMBOLS")) :external)
                                                                ((%lkw-in what '("PRESENT-SYMBOL" "PRESENT-SYMBOLS")) :present)
                                                                (t :all))))
           (%loop-add-binding s var nil)
           (%loop-end-test s `(endp ,g))
           (%loop-pre s `(setq ,var (car ,g)))
           (%loop-step s `(setq ,g (cdr ,g))))
          ((%lkw-in what '("ELEMENT" "ELEMENTS"))
           (%loop-add-binding s g `(coerce ,obj 'list))
           (%loop-bind-pattern s var)
           (%loop-end-test s `(endp ,g))
           (dolist (f (%loop-destructure-setq var `(car ,g))) (%loop-pre s f))
           (%loop-step s `(setq ,g (cdr ,g))))
          (t (error "LOOP: unknown BEING clause ~S" what))))))

  (defun %loop-accumulator (s into kind &optional type)
    "Returns the variable used for accumulation."
    (let ((var (or into (%ls s 10))))
      (unless var
        (setq var (gensym "ACC"))
        (setf (%ls s 10) var))
      (unless (assoc var (%ls s 9))
        (let ((tail (gensym "TAIL")))
          (setf (%ls s 9) (cons (list var kind tail) (%ls s 9)))
          ;; MAXIMIZE and MINIMIZE never executed give zero, as in SBCL;
          ;; their TAIL variable tells whether a value was accumulated
          (%loop-add-binding s var (cond ((member kind '(:sum :count :max :min))
                                          (if (and type (%lkw-in type '("FLOAT" "SINGLE-FLOAT" "DOUBLE-FLOAT"
                                                                        "SHORT-FLOAT" "LONG-FLOAT")))
                                              (if (%lkw-in type '("DOUBLE-FLOAT" "LONG-FLOAT")) 0.0d0 0.0)
                                              0))
                                         (t nil)))
          (%loop-add-binding s tail nil)
          (unless into
            (setf (%ls s 7) var))))
      var))

  (defun %loop-parse-accumulation (s kw)
    (let* ((form (let ((f (%loop-pop s))) (if (and *%loop-it* (%lkw f "IT")) *%loop-it* f)))
           (into (when (%lkw (%loop-peek s) "INTO") (%loop-pop s) (%loop-pop s)))
           (kind (cond ((%lkw-in kw '("COLLECT" "COLLECTING")) :list)
                       ((%lkw-in kw '("APPEND" "APPENDING")) :list)
                       ((%lkw-in kw '("NCONC" "NCONCING")) :list)
                       ((%lkw-in kw '("COUNT" "COUNTING")) :count)
                       ((%lkw-in kw '("SUM" "SUMMING")) :sum)
                       ((%lkw-in kw '("MAXIMIZE" "MAXIMIZING")) :max)
                       (t :min)))
           (type (%loop-skip-type s))
           (var (%loop-accumulator s into kind type))
           (tail (caddr (assoc var (%ls s 9)))))
      (cond ((%lkw-in kw '("COLLECT" "COLLECTING"))
             `(let ((c (list ,form)))
                (if ,var (setf (cdr ,tail) c) (setq ,var c))
                (setq ,tail c)))
            ((%lkw-in kw '("APPEND" "APPENDING" "NCONC" "NCONCING"))
             `(let ((c ,(if (%lkw-in kw '("APPEND" "APPENDING")) `(copy-list ,form) form)))
                (when c
                  (if ,var (setf (cdr (last ,tail)) c) (setq ,var c))
                  (setq ,tail (last c)))))
            ((eq kind :count) `(when ,form (setq ,var (+ ,var 1))))
            ((eq kind :sum) `(setq ,var (+ ,var ,form)))
            ((eq kind :max) `(let ((v ,form)) (when (or (null ,tail) (> v ,var)) (setq ,var v ,tail t))))
            (t `(let ((v ,form)) (when (or (null ,tail) (< v ,var)) (setq ,var v ,tail t)))))))

  (defun %loop-parse-compound-forms (s)
    (let ((forms nil))
      (do () ((or (%loop-at-end s) (atom (%loop-peek s))))
        (setq forms (cons (%loop-pop s) forms)))
      (reverse forms)))

  (defun %loop-parse-selectable (s)
    "Parse one clause usable inside WHEN/IF; returns a form."
    (let ((kw (%loop-pop s)))
      (cond ((%lkw-in kw '("COLLECT" "COLLECTING" "APPEND" "APPENDING" "NCONC" "NCONCING"
                           "COUNT" "COUNTING" "SUM" "SUMMING" "MAXIMIZE" "MAXIMIZING"
                           "MINIMIZE" "MINIMIZING"))
             (%loop-parse-accumulation s kw))
            ((%lkw-in kw '("DO" "DOING")) `(progn ,@(%loop-parse-compound-forms s)))
            ((%lkw kw "RETURN")
             (if (and *%loop-it* (%lkw (%loop-peek s) "IT"))
                 (progn (%loop-pop s) `(return-from ,(%ls s 8) ,*%loop-it*))
                 `(return-from ,(%ls s 8) ,(%loop-pop s))))
            ((%lkw-in kw '("WHEN" "IF" "UNLESS")) (%loop-parse-conditional s kw))
            (t (error "LOOP: unexpected ~S in conditional" kw)))))

  (defun %loop-parse-conditional (s kw)
    (let* ((test (%loop-pop s))
           (it (gensym "IT"))
           (then (list (let ((*%loop-it* it)) (%loop-parse-selectable s))))
           (else nil))
      (do () ((not (%lkw (%loop-peek s) "AND")))
        (%loop-pop s)
        (setq then (cons (let ((*%loop-it* nil)) (%loop-parse-selectable s)) then)))
      (when (%lkw (%loop-peek s) "ELSE")
        (%loop-pop s)
        (setq else (list (let ((*%loop-it* it)) (%loop-parse-selectable s))))
        (do () ((not (%lkw (%loop-peek s) "AND")))
          (%loop-pop s)
          (setq else (cons (let ((*%loop-it* nil)) (%loop-parse-selectable s)) else))))
      (when (%lkw (%loop-peek s) "END") (%loop-pop s))
      `(let ((,it ,test))
         (declare (ignorable ,it))
         (if ,(if (%lkw kw "UNLESS") `(not ,it) it)
             (progn ,@(reverse then))
             (progn ,@(reverse else))))))

  (defun %loop-parse-clause (s)
    (let ((kw (%loop-pop s)))
      (cond
        ((%lkw kw "NAMED") (setf (%ls s 8) (%loop-pop s)))
        ((%lkw-in kw '("FOR" "AS"))
         (let ((group-steps (%ls s 4)) (pre (%ls s 2)))
           (%loop-parse-for s group-steps nil)
           (do () ((not (%lkw (%loop-peek s) "AND")))
             (%loop-pop s) (%loop-parse-for s group-steps t))
           ;; after a main clause (e.g. WHILE), the variables are set and
           ;; tested where the clause is, as other implementations do
           (when (%ls s 3)
             (let ((new nil) (p (%ls s 2)))
               (do () ((eq p pre)) (push (car p) new) (setq p (cdr p)))
               (setf (%ls s 2) pre
                     (%ls s 3) (append (reverse new) (%ls s 3)))))))
        ((%lkw kw "WITH")
         ;; WITH clauses joined by AND bind in parallel: the init forms are
         ;; evaluated into temporaries first
         (let ((more t) (group nil))
           (do () ((not more))
             (let* ((var (%loop-pop s)) (type (%loop-note-type s var (%loop-skip-type s))))
               (if (%lkw (%loop-peek s) "=")
                   (progn (%loop-pop s) (push (list var (%loop-pop s) t) group))
                   (push (list var type nil) group)))
             (if (%lkw (%loop-peek s) "AND") (%loop-pop s) (setq more nil)))
           (setq group (reverse group))
           (when (cdr group)
             (dolist (e group)
               (when (third e)
                 (let ((g (gensym "WITH")))
                   (%loop-add-binding s g (second e))
                   (setf (second e) g)))))
           (dolist (e group)
             (let ((var (first e)))
               (if (third e)
                   (if (consp var)
                       ;; bind the pattern's variables right away (from a
                       ;; temporary), so that later WITH init forms see them
                       (let ((g (gensym)))
                         (%loop-add-binding s g (second e))
                         (dolist (b (%loop-pattern-paths var g))
                           (%loop-add-binding s (first b) (second b))))
                       (%loop-add-binding s var (second e)))
                   (dolist (d (%loop-pattern-defaults var (second e)))
                     (%loop-add-binding s (first d) (second d))))))))
        ((%lkw kw "REPEAT")
         (let ((g (gensym "COUNT")))
           (%loop-add-binding s g (%loop-pop s))
           (%loop-end-test s `(<= ,g 0))
           (%loop-pre s `(setq ,g (- ,g 1)))))
        ((%lkw kw "WHILE") (setf (%ls s 3) (cons `(unless ,(%loop-pop s) (go ,(%ls s 11))) (%ls s 3))))
        ((%lkw kw "UNTIL") (setf (%ls s 3) (cons `(when ,(%loop-pop s) (go ,(%ls s 11))) (%ls s 3))))
        ((%lkw kw "ALWAYS")
         (setf (%ls s 7) t (%ls s 14) t)
         (setf (%ls s 3) (cons `(unless ,(%loop-pop s) (return-from ,(%ls s 8) nil)) (%ls s 3))))
        ((%lkw kw "NEVER")
         (setf (%ls s 7) t (%ls s 14) t)
         (setf (%ls s 3) (cons `(when ,(%loop-pop s) (return-from ,(%ls s 8) nil)) (%ls s 3))))
        ((%lkw kw "THEREIS")
         (setf (%ls s 14) t)
         (setf (%ls s 3) (cons `(let ((v ,(%loop-pop s))) (when v (return-from ,(%ls s 8) v)))
                               (%ls s 3))))
        ((%lkw-in kw '("INITIALLY"))
         (setf (%ls s 5) (append (reverse (%loop-parse-compound-forms s)) (%ls s 5))))
        ((%lkw-in kw '("FINALLY"))
         (setf (%ls s 6) (append (reverse (%loop-parse-compound-forms s)) (%ls s 6))))
        (t
         (setf (%ls s 0) (cons kw (%ls s 0)))
         (setf (%ls s 3) (cons (%loop-parse-selectable s) (%ls s 3)))))))

  (defun %expand-loop (body)
    (if (or (null body) (not (symbolp (car body))) (not (%loop-keyword-p (car body))))
        ;; simple loop
        (let ((top (gensym "LOOP")))
          `(block nil (tagbody ,top (progn ,@body) (go ,top))))
        (let ((s (%loop-new-state body)))
          (when (%lkw (%loop-peek s) "NAMED")
            (%loop-pop s) (setf (%ls s 8) (%loop-pop s)))
          (do () ((%loop-at-end s)) (%loop-parse-clause s))
          (when (and (%ls s 14) (%ls s 10))
            (error 'simple-program-error
                   :format-control "LOOP: ALWAYS, NEVER or THEREIS together with an accumulation without INTO"
                   :format-arguments nil))
          (let* ((name (%ls s 8))
                 (top (gensym "LOOP-TOP"))
                 (end (%ls s 11))
                 (first (%ls s 12))
                 (result (%ls s 7))
                 (type-decls nil))
            `(block ,name
               (let* (,@(progn (setq type-decls (%loop-type-declarations s)) (reverse (%ls s 1)))
                      ,@(when first `((,first t))))
                 (declare (ignorable ,@(mapcar #'car (%ls s 1))) ,@type-decls)
                 (macrolet ((loop-finish () '(go ,end)))
                 (tagbody
                    ,@(reverse (%ls s 5))
                    ,top
                    ,@(reverse (%ls s 2))
                    ,@(reverse (%ls s 3))
                    ,@(when first `((setq ,first nil)))
                    ,@(reverse (%ls s 4))
                    (go ,top)
                    ,end
                    ,@(reverse (%ls s 6))
                    (return-from ,name ,result)))))))))
  )

(defmacro loop (&rest body) (%expand-loop body))

(defmacro loop-finish () '(error "LOOP-FINISH outside of LOOP"))
