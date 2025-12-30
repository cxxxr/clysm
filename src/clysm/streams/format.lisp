;;;; format.lisp - Format function implementation
;;;; FFI-based stream I/O (015-ffi-stream-io)
;;;; Extended for 032-format-function

(in-package #:clysm/streams)

;;; ============================================================
;;; Column Tracking (T011, T012)
;;; ============================================================

(defvar *format-column* 0
  "Current column position for format output.
   T011: Column tracking for ~& fresh-line support.
   0 after newline, incremented for other characters.")

(defun format-output-char (char stream result-string)
  "Output CHAR to stream or result-string, tracking column.
   T012: Update column after each character output."
  (if result-string
      (vector-push-extend char result-string)
      (write-char char stream))
  ;; Update column tracking
  (if (char= char #\Newline)
      (setf *format-column* 0)
      (incf *format-column*)))

(defun format-output-string (str stream result-string)
  "Output STR to stream or result-string, tracking column."
  (loop for c across str
        do (format-output-char c stream result-string)))

;;; ============================================================
;;; Format Error Signaling (T013)
;;; ============================================================

(defun signal-format-error (control-string position format-control &rest format-args)
  "Signal a format-error condition.
   T013: Replace cl:error with format-error signaling."
  (cl:error 'clysm/conditions:format-error
            :control-string control-string
            :position position
            :format-control format-control
            :format-arguments format-args))

;;; ============================================================
;;; Format Directive Structures (T065, T066)
;;; ============================================================

(defstruct format-directive
  "Represents a single format directive.
   T065: FormatDirective struct."
  (type nil :type keyword)
  (start 0 :type fixnum)
  (end 0 :type fixnum))

(defstruct format-string-info
  "Parsed format string information.
   T066: FormatStringInfo struct."
  (control-string "" :type string)
  (directives nil :type list))

;;; ============================================================
;;; Nested Directive Structures (T027, T028, T040, T041, T050)
;;; ============================================================

;; Iteration directive: ~{body~}
(defstruct (iteration-directive (:include format-directive))
  "Iteration directive with nested body.
   T027: iteration-directive struct with body-directives."
  (body-directives nil :type list)
  (body-string "" :type string))

;; Escape directive: ~^ (exit iteration when no more elements)
(defstruct (escape-directive (:include format-directive))
  "Escape directive ~^ for iteration exit.
   T028: escape-directive struct.")

;; Conditional directive: ~[clause~;clause~]
(defstruct (conditional-directive (:include format-directive))
  "Conditional directive with clauses.
   T040: conditional-directive struct."
  (clauses nil :type list)
  (boolean-p nil :type boolean)
  (default-index nil))

;; Clause info for conditional directive
(defstruct clause-info
  "Information about a conditional clause.
   T041: clause-info struct."
  (index 0 :type fixnum)
  (directives nil :type list)
  (default-p nil :type boolean)
  (text "" :type string))  ; The clause body text for literal output

;; Recursive directive: ~?
(defstruct (recursive-directive (:include format-directive))
  "Recursive directive for nested format strings.
   T050: recursive-directive struct.")

;;; ============================================================
;;; Format String Parser (T067, T014)
;;; ============================================================

(defun parse-simple-directive (control-string start next-char)
  "Parse a simple (non-nested) directive at position START.
   T014: Dispatcher helper for simple directives.
   Returns (directive . end-position)."
  (let ((upchar (char-upcase next-char)))
    (cond
      ;; ~^ escape directive (T031)
      ((char= upchar #\^)
       (cons (make-escape-directive :type :escape :start start :end (+ start 2))
             (+ start 2)))
      ;; ~? recursive directive (T051)
      ((char= upchar #\?)
       (cons (make-recursive-directive :type :recursive :start start :end (+ start 2))
             (+ start 2)))
      ;; Other simple directives
      (t
       (let ((type (case upchar
                     (#\A :aesthetic)
                     (#\S :standard)
                     (#\D :decimal)
                     (#\% :newline)
                     (#\~ :tilde)
                     (#\& :fresh-line)  ; T018: Add fresh-line case
                     (t nil))))
         (if type
             (cons (make-format-directive :type type :start start :end (+ start 2))
                   (+ start 2))
             nil))))))

(defun parse-directive (control-string start)
  "Parse a directive starting at position START.
   T014: Dispatcher for simple and nested directives.
   Returns (directive . end-position) or signals format-error."
  (let ((len (length control-string)))
    (when (>= (1+ start) len)
      (signal-format-error control-string start "Format string ends with tilde"))
    (let ((next-char (char control-string (1+ start))))
      ;; Check for modifiers (colon for boolean conditional)
      (cond
        ;; ~:[ boolean conditional form
        ((and (char= next-char #\:)
              (< (+ start 2) len)
              (char= (char-upcase (char control-string (+ start 2))) #\[))
         (parse-conditional-directive control-string start t))
        ;; ~[ conditional
        ((char= (char-upcase next-char) #\[)
         (parse-conditional-directive control-string start nil))
        ;; ~{ iteration
        ((char= (char-upcase next-char) #\{)
         (parse-iteration-directive control-string start))
        ;; Simple directives
        (t
         (let ((result (parse-simple-directive control-string start next-char)))
           (if result
               result
               (signal-format-error control-string start
                                    "Unknown format directive: ~~~C" next-char))))))))

(defun parse-format-string (control-string)
  "Parse CONTROL-STRING into format-string-info.
   T067: Compile-time format string parsing.
   T014: Uses parse-directive dispatcher for extensibility."
  (let ((directives nil)
        (len (length control-string))
        (i 0))
    (loop while (< i len)
          for char = (char control-string i)
          do (if (char= char #\~)
                 (let ((result (parse-directive control-string i)))
                   (push (car result) directives)
                   (setf i (cdr result)))
                 (incf i)))
    (make-format-string-info
     :control-string control-string
     :directives (nreverse directives))))

;;; ============================================================
;;; Nested Directive Parsers (T029, T042)
;;; ============================================================

(defun find-matching-close (control-string start open-char close-char)
  "Find position of matching close character, handling nesting.
   START is position after the opening character."
  (let ((len (length control-string))
        (depth 1)
        (i start))
    (loop while (and (< i len) (> depth 0))
          do (let ((char (char control-string i)))
               (cond
                 ((and (char= char #\~)
                       (< (1+ i) len))
                  (let ((next (char-upcase (char control-string (1+ i)))))
                    (cond
                      ((char= next open-char) (incf depth) (incf i 2))
                      ((char= next close-char) (decf depth) (incf i 2))
                      (t (incf i 2)))))
                 (t (incf i)))))
    (if (zerop depth)
        (- i 2)  ; Position of ~} or ~]
        nil)))

(defun parse-iteration-directive (control-string start)
  "Parse ~{body~} iteration directive.
   T029: Parse iteration with nested body.
   Returns (iteration-directive . end-position)."
  (let* ((body-start (+ start 2))  ; After ~{
         (close-pos (find-matching-close control-string body-start #\{ #\})))
    (unless close-pos
      (signal-format-error control-string start "Unclosed ~{ iteration directive"))
    (let* ((body-string (subseq control-string body-start close-pos))
           (body-info (parse-format-string body-string)))
      (cons (make-iteration-directive
             :type :iteration
             :start start
             :end (+ close-pos 2)
             :body-directives (format-string-info-directives body-info)
             :body-string body-string)
            (+ close-pos 2)))))

(defun parse-conditional-directive (control-string start boolean-p)
  "Parse ~[clause~;clause~] or ~:[false~;true~] conditional directive.
   T042: Parse conditional with clause separation.
   BOOLEAN-P indicates ~:[ form.
   Returns (conditional-directive . end-position)."
  (let* ((body-start (if boolean-p (+ start 3) (+ start 2)))  ; After ~[ or ~:[
         (close-pos (find-matching-close control-string body-start #\[ #\])))
    (unless close-pos
      (signal-format-error control-string start "Unclosed ~[ conditional directive"))
    ;; Parse clauses separated by ~;
    (let ((clauses nil)
          (clause-index 0)
          (default-index nil)
          (clause-start body-start)
          (i body-start))
      (loop while (< i close-pos)
            do (let ((char (char control-string i)))
                 (cond
                   ;; Found ~; clause separator or ~:; default
                   ((and (char= char #\~)
                         (< (1+ i) close-pos))
                    (let ((next (char control-string (1+ i))))
                      (cond
                        ;; ~:; marks default clause
                        ((and (char= next #\:)
                              (< (+ i 2) close-pos)
                              (char= (char control-string (+ i 2)) #\;))
                         ;; Save current clause
                         (let ((clause-text (subseq control-string clause-start i)))
                           (push (make-clause-info
                                  :index clause-index
                                  :directives (format-string-info-directives
                                               (parse-format-string clause-text))
                                  :default-p nil
                                  :text clause-text)
                                 clauses))
                         (setf default-index (1+ clause-index))
                         (incf clause-index)
                         (setf i (+ i 3))
                         (setf clause-start i))
                        ;; ~; regular separator
                        ((char= next #\;)
                         (let ((clause-text (subseq control-string clause-start i)))
                           (push (make-clause-info
                                  :index clause-index
                                  :directives (format-string-info-directives
                                               (parse-format-string clause-text))
                                  :default-p nil
                                  :text clause-text)
                                 clauses))
                         (incf clause-index)
                         (setf i (+ i 2))
                         (setf clause-start i))
                        (t (incf i 2)))))
                   (t (incf i)))))
      ;; Final clause (or default clause after ~:;)
      (let ((clause-text (subseq control-string clause-start close-pos)))
        (push (make-clause-info
               :index clause-index
               :directives (format-string-info-directives
                            (parse-format-string clause-text))
               :default-p (not (null default-index))
               :text clause-text)
              clauses))
      (cons (make-conditional-directive
             :type :conditional
             :start start
             :end (+ close-pos 2)
             :clauses (nreverse clauses)
             :boolean-p boolean-p
             :default-index default-index)
            (+ close-pos 2)))))

;;; ============================================================
;;; Format Directive Handlers (T068-T072)
;;; ============================================================

(defun format-aesthetic (object stream)
  "Format OBJECT using aesthetic (human-readable) representation.
   T068: ~A directive handler - princ style."
  (write-string (princ-to-string object) stream))

(defun format-standard (object stream)
  "Format OBJECT using standard (machine-readable) representation.
   T069: ~S directive handler - prin1 style."
  (write-string (prin1-to-string object) stream))

(defun format-decimal (object stream)
  "Format OBJECT as decimal integer.
   T070: ~D directive handler."
  (unless (integerp object)
    (cl:error 'type-error :datum object :expected-type 'integer))
  (write-string (princ-to-string object) stream))

(defun format-newline (stream)
  "Output a newline.
   T071: ~% directive handler."
  (write-char #\Newline stream))

(defun format-tilde (stream)
  "Output a literal tilde.
   T072: ~~ directive handler."
  (write-char #\~ stream))

;;; ============================================================
;;; Princ/Prin1 to String (helpers)
;;; ============================================================

(defun princ-to-string (object)
  "Return aesthetic string representation of OBJECT.
   Uses host CL's princ-to-string for non-trivial cases to avoid recursion."
  (typecase object
    (string object)
    (character (string object))
    ;; For other types, use host CL's princ-to-string
    (t (cl:princ-to-string object))))

(defun prin1-to-string (object)
  "Return standard string representation of OBJECT.
   Uses host CL's prin1-to-string for non-trivial cases to avoid recursion."
  (typecase object
    (string (concatenate 'string "\"" object "\""))
    (character (cl:format nil "#\\~C" object))
    ;; For other types, use host CL's prin1-to-string
    (t (cl:prin1-to-string object))))

;;; ============================================================
;;; Write-to-String (001-numeric-format T031)
;;; ============================================================

(defun write-to-string (object &key (base 10))
  "Return string representation of OBJECT.
   T031: Host-side write-to-string for interpreter support.
   Feature: 001-numeric-format (Phase 14C)

   Supports :BASE keyword for integer output (2-36).
   For ratios, outputs numerator/denominator in specified base.
   For floats, uses standard decimal notation."
  ;; Validate base
  (unless (<= 2 base 36)
    (error 'type-error :datum base :expected-type '(integer 2 36)))
  (typecase object
    ;; Integer: convert with base
    (integer
     (let ((*print-base* base)
           (*print-radix* nil))
       (cl:write-to-string object)))
    ;; Ratio: convert numerator/denominator with base
    (ratio
     (let ((*print-base* base)
           (*print-radix* nil))
       (cl:format nil "~A/~A" (numerator object) (denominator object))))
    ;; Float: standard decimal notation
    (float
     (cl:write-to-string object))
    ;; Other types: use host CL's write-to-string
    (t (cl:write-to-string object))))

;;; ============================================================
;;; Format Function (FR-008 to FR-013, T073, T074, US3)
;;; ============================================================

(defun format (destination control-string &rest args)
  "Format output according to CONTROL-STRING with ARGS.
   FR-008: System MUST provide format function supporting destination argument.
   FR-009: ~A directive (aesthetic printing).
   FR-010: ~S directive (standard printing).
   FR-011: ~D directive (decimal printing).
   FR-012: ~% directive (newline).
   FR-013: ~~ directive (tilde).
   FR-014: ~& directive (fresh-line).
   FR-016: ~{~} directive (iteration).
   FR-017: ~^ directive (escape).
   FR-018: ~[~] directive (conditional).
   FR-019: ~? directive (recursive).

   DESTINATION:
   - T: output to *standard-output*, return NIL
   - NIL: return formatted string
   - stream: output to stream, return NIL"
  (let* ((info (parse-format-string control-string))
         (output-stream (cond
                          ((eq destination t) *standard-output*)
                          ((null destination) nil)
                          ((streamp destination) destination)
                          (t (signal-format-error control-string 0
                                                  "Invalid destination: ~S" destination))))
         (result-string (when (null destination)
                          (make-array 0 :element-type 'character
                                       :adjustable t :fill-pointer 0))))
    ;; Reset column tracking for string output
    (when (null destination)
      (setf *format-column* 0))
    ;; Execute directives with args
    (multiple-value-bind (final-arg-index)
        (execute-format-directives
         (format-string-info-control-string info)
         (format-string-info-directives info)
         args 0
         output-stream result-string)
      (declare (ignore final-arg-index))
      ;; Return value based on destination
      (if result-string
          (coerce result-string 'string)
          nil))))

;;; ============================================================
;;; Format Directive Execution (T019, T020, T032-T034, T044-T045, T052-T053)
;;; ============================================================

(defun execute-format-directives (control directives args arg-index stream result-string)
  "Execute a list of directives, returning final arg-index.
   Helper for recursive directive processing."
  (let ((last-pos 0))
    (dolist (directive directives)
      ;; Output literal text before directive
      (let ((start last-pos)
            (end (format-directive-start directive)))
        (when (< start end)
          (format-output-string (subseq control start end) stream result-string)))

      ;; Process directive based on type
      (setf arg-index
            (execute-single-directive directive control args arg-index stream result-string))

      (setf last-pos (format-directive-end directive)))

    ;; Output remaining literal text
    (when (< last-pos (length control))
      (format-output-string (subseq control last-pos) stream result-string))

    arg-index))

(defun execute-single-directive (directive control args arg-index stream result-string)
  "Execute a single directive, returning updated arg-index."
  (let ((dtype (format-directive-type directive)))
    (case dtype
      ;; ~A aesthetic
      (:aesthetic
       (when (>= arg-index (length args))
         (signal-format-error control (format-directive-start directive)
                              "Not enough arguments for ~~A"))
       (format-output-string (princ-to-string (nth arg-index args)) stream result-string)
       (1+ arg-index))

      ;; ~S standard
      (:standard
       (when (>= arg-index (length args))
         (signal-format-error control (format-directive-start directive)
                              "Not enough arguments for ~~S"))
       (format-output-string (prin1-to-string (nth arg-index args)) stream result-string)
       (1+ arg-index))

      ;; ~D decimal
      (:decimal
       (when (>= arg-index (length args))
         (signal-format-error control (format-directive-start directive)
                              "Not enough arguments for ~~D"))
       (let ((arg (nth arg-index args)))
         (unless (integerp arg)
           (signal-format-error control (format-directive-start directive)
                                "~~D requires integer, got ~S" arg))
         (format-output-string (princ-to-string arg) stream result-string))
       (1+ arg-index))

      ;; ~% newline
      (:newline
       (format-output-char #\Newline stream result-string)
       arg-index)

      ;; ~~ tilde
      (:tilde
       (format-output-char #\~ stream result-string)
       arg-index)

      ;; ~& fresh-line (T019, T020)
      (:fresh-line
       (unless (zerop *format-column*)
         (format-output-char #\Newline stream result-string))
       arg-index)

      ;; ~{~} iteration (T032, T033, T034)
      (:iteration
       (execute-iteration-directive directive control args arg-index stream result-string))

      ;; ~^ escape - only valid inside iteration, handled there
      (:escape
       (signal-format-error control (format-directive-start directive)
                            "~~^ outside of iteration context"))

      ;; ~[~] conditional (T044, T045)
      (:conditional
       (execute-conditional-directive directive control args arg-index stream result-string))

      ;; ~? recursive (T052, T053)
      (:recursive
       (execute-recursive-directive directive control args arg-index stream result-string))

      ;; Unknown directive type
      (otherwise
       (signal-format-error control (format-directive-start directive)
                            "Unknown directive type: ~S" dtype)))))

;;; ============================================================
;;; Iteration Execution (T032, T033)
;;; ============================================================

(defun execute-iteration-directive (directive control args arg-index stream result-string)
  "Execute ~{body~} iteration directive.
   T032: Loop over list elements.
   T033: Handle ~^ escape.
   Returns updated arg-index."
  (when (>= arg-index (length args))
    (signal-format-error control (format-directive-start directive)
                         "Not enough arguments for ~~{~~}"))
  (let ((list-arg (nth arg-index args)))
    (unless (listp list-arg)
      (signal-format-error control (format-directive-start directive)
                           "~~{ requires a list argument, got ~S" list-arg))
    (let ((body-directives (iteration-directive-body-directives directive))
          (body-string (iteration-directive-body-string directive)))
      ;; Process each element
      (loop for remaining on list-arg
            for item = (car remaining)
            do (block iteration-body
                 (let ((body-last-pos 0))
                   (dolist (body-dir body-directives)
                     ;; Literal text
                     (let ((start body-last-pos)
                           (end (format-directive-start body-dir)))
                       (when (< start end)
                         (format-output-string (subseq body-string start end)
                                               stream result-string)))
                     ;; Handle ~^ escape
                     (when (eq (format-directive-type body-dir) :escape)
                       (when (null (cdr remaining))
                         (return-from iteration-body)))
                     ;; Other directives use item as single-element arg list
                     (unless (eq (format-directive-type body-dir) :escape)
                       (execute-single-directive body-dir body-string (list item) 0
                                                 stream result-string))
                     (setf body-last-pos (format-directive-end body-dir)))
                   ;; Remaining literal text
                   (when (< body-last-pos (length body-string))
                     (format-output-string (subseq body-string body-last-pos)
                                           stream result-string)))))))
  (1+ arg-index))

;;; ============================================================
;;; Conditional Execution (T044)
;;; ============================================================

(defun execute-conditional-directive (directive control args arg-index stream result-string)
  "Execute ~[clause~;clause~] or ~:[false~;true~] conditional directive.
   T044: Select clause by index or boolean.
   Returns updated arg-index."
  (when (>= arg-index (length args))
    (signal-format-error control (format-directive-start directive)
                         "Not enough arguments for ~~[~~]"))
  (let* ((arg (nth arg-index args))
         (clauses (conditional-directive-clauses directive))
         (boolean-p (conditional-directive-boolean-p directive))
         (default-index (conditional-directive-default-index directive))
         ;; Determine which clause to execute
         (index (if boolean-p
                    (if arg 1 0)  ; Non-nil -> 1, nil -> 0
                    (progn
                      (unless (integerp arg)
                        (signal-format-error control (format-directive-start directive)
                                             "~~[ requires integer argument, got ~S" arg))
                      arg)))
         ;; Find the clause to execute
         (selected-clause (or (find index clauses :key #'clause-info-index)
                              (and default-index
                                   (find default-index clauses :key #'clause-info-index)))))
    (when selected-clause
      ;; Execute the clause using its stored text and directives
      (execute-format-directives (clause-info-text selected-clause)
                                 (clause-info-directives selected-clause)
                                 nil 0  ; Conditionals don't consume additional args
                                 stream result-string)))
  (1+ arg-index))

;;; ============================================================
;;; Recursive Execution (T052)
;;; ============================================================

(defun execute-recursive-directive (directive control args arg-index stream result-string)
  "Execute ~? recursive directive.
   T052: Consume format-string and args-list, call format recursively.
   Returns updated arg-index (+ 2)."
  (when (>= (1+ arg-index) (length args))
    (signal-format-error control (format-directive-start directive)
                         "Not enough arguments for ~~? (need format-string and args-list)"))
  (let ((sub-control (nth arg-index args))
        (sub-args (nth (1+ arg-index) args)))
    (unless (stringp sub-control)
      (signal-format-error control (format-directive-start directive)
                           "~~? first argument must be a string, got ~S" sub-control))
    (unless (listp sub-args)
      (signal-format-error control (format-directive-start directive)
                           "~~? second argument must be a list, got ~S" sub-args))
    ;; Recursive format call
    (let ((sub-result (apply #'format nil sub-control sub-args)))
      (format-output-string sub-result stream result-string)))
  (+ arg-index 2))
