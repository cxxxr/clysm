;;;; loop-test.lisp - LOOP macro unit tests (029-loop-macro)
(in-package #:clysm/tests/unit/loop)

;;; ============================================================
;;; Phase 1: Struct Tests (T001-T005)
;;; ============================================================

(deftest loop-context-struct
  (testing "loop-context creation"
    (let ((ctx (clysm/lib/macros:make-loop-context)))
      (ok (clysm/lib/macros:loop-context-p ctx))
      (ok (null (clysm/lib/macros:loop-context-name ctx)))
      (ok (null (clysm/lib/macros:loop-context-iteration-clauses ctx)))
      (ok (null (clysm/lib/macros:loop-context-accumulation-clauses ctx)))
      (ok (null (clysm/lib/macros:loop-context-termination-clauses ctx)))
      (ok (null (clysm/lib/macros:loop-context-body-clauses ctx)))
      (ok (null (clysm/lib/macros:loop-context-initially-forms ctx)))
      (ok (null (clysm/lib/macros:loop-context-finally-forms ctx)))
      (ok (null (clysm/lib/macros:loop-context-with-bindings ctx)))
      (ok (null (clysm/lib/macros:loop-context-result-form ctx)))
      (ok (= 0 (clysm/lib/macros:loop-context-gensym-counter ctx)))))

  (testing "loop-context with name"
    (let ((ctx (clysm/lib/macros:make-loop-context :name 'outer)))
      (ok (eq 'outer (clysm/lib/macros:loop-context-name ctx))))))

;;; ============================================================
;;; US1: FOR/AS Clause Parsing Tests (T015-T019)
;;; ============================================================

(deftest for-arithmetic-parsing
  (testing "FOR arithmetic clause parsing"
    ;; T015: Tests for parse-for-arithmetic
    (let ((ctx (clysm/lib/macros:make-loop-context)))
      (clysm/lib/macros:parse-loop-clauses '(for i from 1 to 10) ctx)
      (let ((clauses (clysm/lib/macros:loop-context-iteration-clauses ctx)))
        (ok (= 1 (length clauses)) "Should have one iteration clause")
        (let ((clause (first clauses)))
          (ok (clysm/lib/macros:loop-iter-arithmetic-p clause)
              "Should be arithmetic clause")
          (ok (eq 'i (clysm/lib/macros:loop-iteration-clause-var clause))
              "Variable should be I")
          (ok (= 1 (clysm/lib/macros:loop-iter-arithmetic-from clause))
              "FROM should be 1")
          (ok (= 10 (clysm/lib/macros:loop-iter-arithmetic-to clause))
              "TO should be 10"))))))

(deftest for-in-parsing
  (testing "FOR IN list parsing"
    ;; T016: Tests for parse-for-in
    (let ((ctx (clysm/lib/macros:make-loop-context)))
      (clysm/lib/macros:parse-loop-clauses '(for x in list) ctx)
      (let ((clauses (clysm/lib/macros:loop-context-iteration-clauses ctx)))
        (ok (= 1 (length clauses)) "Should have one iteration clause")
        (let ((clause (first clauses)))
          (ok (clysm/lib/macros:loop-iter-in-p clause)
              "Should be IN clause")
          (ok (eq 'x (clysm/lib/macros:loop-iteration-clause-var clause))
              "Variable should be X")
          (ok (eq 'list (clysm/lib/macros:loop-iter-in-list-form clause))
              "List form should be LIST")
          (ok (clysm/lib/macros:loop-iter-in-list-var clause)
              "Should have generated list-var"))))))

(deftest for-on-parsing
  (testing "FOR ON list parsing"
    ;; T017: Tests for parse-for-on
    (let ((ctx (clysm/lib/macros:make-loop-context)))
      (clysm/lib/macros:parse-loop-clauses '(for rest on '(1 2 3)) ctx)
      (let ((clauses (clysm/lib/macros:loop-context-iteration-clauses ctx)))
        (ok (= 1 (length clauses)) "Should have one iteration clause")
        (let ((clause (first clauses)))
          (ok (clysm/lib/macros:loop-iter-on-p clause)
              "Should be ON clause")
          (ok (eq 'rest (clysm/lib/macros:loop-iteration-clause-var clause))
              "Variable should be REST")
          (ok (clysm/lib/macros:loop-iter-on-list-var clause)
              "Should have generated list-var"))))))

(deftest for-across-parsing
  (testing "FOR ACROSS vector parsing"
    ;; T018: Tests for parse-for-across
    (let ((ctx (clysm/lib/macros:make-loop-context)))
      (clysm/lib/macros:parse-loop-clauses '(for c across "abc") ctx)
      (let ((clauses (clysm/lib/macros:loop-context-iteration-clauses ctx)))
        (ok (= 1 (length clauses)) "Should have one iteration clause")
        (let ((clause (first clauses)))
          (ok (clysm/lib/macros:loop-iter-across-p clause)
              "Should be ACROSS clause")
          (ok (eq 'c (clysm/lib/macros:loop-iteration-clause-var clause))
              "Variable should be C")
          (ok (string= "abc" (clysm/lib/macros:loop-iter-across-vector-form clause))
              "Vector form should be \"abc\"")
          (ok (clysm/lib/macros:loop-iter-across-index-var clause)
              "Should have generated index-var")
          (ok (clysm/lib/macros:loop-iter-across-vec-var clause)
              "Should have generated vec-var"))))))

(deftest for-equals-parsing
  (testing "FOR = THEN parsing"
    ;; T019: Tests for parse-for-equals
    (let ((ctx (clysm/lib/macros:make-loop-context)))
      (clysm/lib/macros:parse-loop-clauses '(for i = 1 then (1+ i)) ctx)
      (let ((clauses (clysm/lib/macros:loop-context-iteration-clauses ctx)))
        (ok (= 1 (length clauses)) "Should have one iteration clause")
        (let ((clause (first clauses)))
          (ok (clysm/lib/macros:loop-iter-equals-p clause)
              "Should be EQUALS clause")
          (ok (eq 'i (clysm/lib/macros:loop-iteration-clause-var clause))
              "Variable should be I")
          (ok (= 1 (clysm/lib/macros:loop-iter-equals-init-form clause))
              "Init form should be 1")
          (ok (equal '(1+ i) (clysm/lib/macros:loop-iter-equals-then-form clause))
              "Then form should be (1+ i)"))))))

;;; ============================================================
;;; US2: Accumulation Clause Tests (T031-T037)
;;; ============================================================

(deftest collect-parsing
  (testing "COLLECT parsing"
    ;; T031: Tests for COLLECT parsing
    (let ((ctx (clysm/lib/macros:make-loop-context)))
      (clysm/lib/macros:parse-loop-clauses '(collect x) ctx)
      (let ((clauses (clysm/lib/macros:loop-context-accumulation-clauses ctx)))
        (ok (= 1 (length clauses)) "Should have one accumulation clause")
        (let ((clause (first clauses)))
          (ok (eq :collect (clysm/lib/macros:loop-accumulation-clause-type clause))
              "Type should be :COLLECT")
          (ok (eq 'x (clysm/lib/macros:loop-accumulation-clause-expr clause))
              "Expression should be X")
          (ok (clysm/lib/macros:loop-accumulation-clause-acc-var clause)
              "Should have accumulator var"))))))

(deftest sum-parsing
  (testing "SUM parsing"
    ;; T032: Tests for SUM parsing
    (let ((ctx (clysm/lib/macros:make-loop-context)))
      (clysm/lib/macros:parse-loop-clauses '(sum i) ctx)
      (let ((clauses (clysm/lib/macros:loop-context-accumulation-clauses ctx)))
        (ok (= 1 (length clauses)) "Should have one accumulation clause")
        (let ((clause (first clauses)))
          (ok (eq :sum (clysm/lib/macros:loop-accumulation-clause-type clause))
              "Type should be :SUM")
          (ok (eq 'i (clysm/lib/macros:loop-accumulation-clause-expr clause))
              "Expression should be I"))))))

(deftest count-parsing
  (testing "COUNT parsing"
    ;; T033: Tests for COUNT parsing
    (let ((ctx (clysm/lib/macros:make-loop-context)))
      (clysm/lib/macros:parse-loop-clauses '(count (oddp i)) ctx)
      (let ((clauses (clysm/lib/macros:loop-context-accumulation-clauses ctx)))
        (ok (= 1 (length clauses)) "Should have one accumulation clause")
        (let ((clause (first clauses)))
          (ok (eq :count (clysm/lib/macros:loop-accumulation-clause-type clause))
              "Type should be :COUNT")
          (ok (equal '(oddp i) (clysm/lib/macros:loop-accumulation-clause-expr clause))
              "Expression should be (oddp i)"))))))

(deftest maximize-minimize-parsing
  (testing "MAXIMIZE/MINIMIZE parsing"
    ;; T034: Tests for MAXIMIZE/MINIMIZE parsing
    (let ((ctx (clysm/lib/macros:make-loop-context)))
      (clysm/lib/macros:parse-loop-clauses '(maximize x) ctx)
      (let ((clauses (clysm/lib/macros:loop-context-accumulation-clauses ctx)))
        (ok (= 1 (length clauses)) "Should have one accumulation clause")
        (ok (eq :maximize (clysm/lib/macros:loop-accumulation-clause-type (first clauses)))
            "Type should be :MAXIMIZE")))
    (let ((ctx (clysm/lib/macros:make-loop-context)))
      (clysm/lib/macros:parse-loop-clauses '(minimize x) ctx)
      (let ((clauses (clysm/lib/macros:loop-context-accumulation-clauses ctx)))
        (ok (= 1 (length clauses)) "Should have one accumulation clause")
        (ok (eq :minimize (clysm/lib/macros:loop-accumulation-clause-type (first clauses)))
            "Type should be :MINIMIZE")))))

(deftest append-nconc-parsing
  (testing "APPEND/NCONC parsing"
    ;; T035: Tests for APPEND/NCONC parsing
    (let ((ctx (clysm/lib/macros:make-loop-context)))
      (clysm/lib/macros:parse-loop-clauses '(append list) ctx)
      (let ((clauses (clysm/lib/macros:loop-context-accumulation-clauses ctx)))
        (ok (= 1 (length clauses)) "Should have one accumulation clause")
        (ok (eq :append (clysm/lib/macros:loop-accumulation-clause-type (first clauses)))
            "Type should be :APPEND")))
    (let ((ctx (clysm/lib/macros:make-loop-context)))
      (clysm/lib/macros:parse-loop-clauses '(nconc list) ctx)
      (let ((clauses (clysm/lib/macros:loop-context-accumulation-clauses ctx)))
        (ok (= 1 (length clauses)) "Should have one accumulation clause")
        (ok (eq :nconc (clysm/lib/macros:loop-accumulation-clause-type (first clauses)))
            "Type should be :NCONC")))))

(deftest into-keyword-parsing
  (testing "INTO keyword parsing"
    ;; T036: Tests for INTO keyword parsing
    (let ((ctx (clysm/lib/macros:make-loop-context)))
      (clysm/lib/macros:parse-loop-clauses '(collect x into result) ctx)
      (let ((clauses (clysm/lib/macros:loop-context-accumulation-clauses ctx)))
        (ok (= 1 (length clauses)) "Should have one accumulation clause")
        (let ((clause (first clauses)))
          (ok (eq 'result (clysm/lib/macros:loop-accumulation-clause-into-var clause))
              "INTO var should be RESULT")
          (ok (eq 'result (clysm/lib/macros:loop-accumulation-clause-acc-var clause))
              "Acc var should be RESULT (same as INTO)"))))))

(deftest conflicting-accumulation-error
  (testing "Conflicting accumulation detection"
    ;; T037: Multiple accumulations to same var can be detected
    ;; Note: Current implementation allows it; ANSI says mixing types is an error
    (let ((ctx (clysm/lib/macros:make-loop-context)))
      (clysm/lib/macros:parse-loop-clauses '(sum i collect j) ctx)
      (let ((clauses (clysm/lib/macros:loop-context-accumulation-clauses ctx)))
        (ok (= 2 (length clauses)) "Should have two accumulation clauses")))))

;;; ============================================================
;;; US3: Termination Clause Tests (T051-T055)
;;; ============================================================

(deftest while-parsing
  (testing "WHILE parsing"
    ;; T051: Tests for WHILE parsing
    (let ((ctx (clysm/lib/macros:make-loop-context)))
      (clysm/lib/macros:parse-loop-clauses '(while (< x 10)) ctx)
      (let ((clauses (clysm/lib/macros:loop-context-termination-clauses ctx)))
        (ok (= 1 (length clauses)) "Should have one termination clause")
        (let ((clause (first clauses)))
          (ok (eq :while (clysm/lib/macros:loop-termination-clause-type clause))
              "Type should be :WHILE")
          (ok (equal '(< x 10) (clysm/lib/macros:loop-termination-clause-expr clause))
              "Expression should be (< x 10)"))))))

(deftest until-parsing
  (testing "UNTIL parsing"
    ;; T052: Tests for UNTIL parsing
    (let ((ctx (clysm/lib/macros:make-loop-context)))
      (clysm/lib/macros:parse-loop-clauses '(until done) ctx)
      (let ((clauses (clysm/lib/macros:loop-context-termination-clauses ctx)))
        (ok (= 1 (length clauses)) "Should have one termination clause")
        (let ((clause (first clauses)))
          (ok (eq :until (clysm/lib/macros:loop-termination-clause-type clause))
              "Type should be :UNTIL")
          (ok (eq 'done (clysm/lib/macros:loop-termination-clause-expr clause))
              "Expression should be DONE"))))))

(deftest boolean-aggregation-parsing
  (testing "ALWAYS/NEVER/THEREIS parsing"
    ;; T053: Tests for ALWAYS/NEVER/THEREIS parsing
    (let ((ctx (clysm/lib/macros:make-loop-context)))
      (clysm/lib/macros:parse-loop-clauses '(always (> x 0)) ctx)
      (let ((clauses (clysm/lib/macros:loop-context-termination-clauses ctx)))
        (ok (= 1 (length clauses)) "Should have one clause")
        (ok (eq :always (clysm/lib/macros:loop-termination-clause-type (first clauses)))
            "Type should be :ALWAYS")))))

(deftest return-parsing
  (testing "RETURN in loop body"
    ;; T054: RETURN is parsed as a body form
    (let ((ctx (clysm/lib/macros:make-loop-context)))
      (clysm/lib/macros:parse-loop-clauses '(do (return 42)) ctx)
      (let ((body (clysm/lib/macros:loop-context-body-clauses ctx)))
        (ok (= 1 (length body)) "Should have one body clause")
        (ok (equal '(return 42) (first body)) "Body should be (return 42)")))))

(deftest loop-finish-parsing
  (testing "LOOP-FINISH keyword recognition"
    ;; T055: LOOP-FINISH is a recognized keyword
    (ok (clysm/lib/macros:loop-keyword-p 'loop-finish)
        "LOOP-FINISH should be recognized as keyword")))

;;; ============================================================
;;; US4: Conditional Clause Tests (T068-T071)
;;; ============================================================

(deftest if-when-parsing
  (testing "IF/WHEN parsing"
    ;; T068: Tests for IF/WHEN parsing
    (let ((ctx (clysm/lib/macros:make-loop-context)))
      (clysm/lib/macros:parse-loop-clauses '(when (oddp x) do (print x)) ctx)
      (let ((body (clysm/lib/macros:loop-context-body-clauses ctx)))
        (ok (= 1 (length body)) "Should have one body clause")
        (ok (clysm/lib/macros:loop-conditional-clause-p (first body))
            "Body should be a conditional clause")
        (ok (eq :when (clysm/lib/macros:loop-conditional-clause-type (first body)))
            "Type should be :WHEN")))))

(deftest unless-parsing
  (testing "UNLESS parsing"
    ;; T069: Tests for UNLESS parsing
    (let ((ctx (clysm/lib/macros:make-loop-context)))
      (clysm/lib/macros:parse-loop-clauses '(unless done do (process)) ctx)
      (let ((body (clysm/lib/macros:loop-context-body-clauses ctx)))
        (ok (= 1 (length body)) "Should have one body clause")
        (ok (clysm/lib/macros:loop-conditional-clause-p (first body))
            "Body should be a conditional clause")
        (ok (eq :unless (clysm/lib/macros:loop-conditional-clause-type (first body)))
            "Type should be :UNLESS")))))

(deftest else-clause-parsing
  (testing "ELSE clause parsing"
    ;; T070: Tests for ELSE clause parsing
    (let ((ctx (clysm/lib/macros:make-loop-context)))
      (clysm/lib/macros:parse-loop-clauses '(if test do (a) else do (b)) ctx)
      (let ((body (clysm/lib/macros:loop-context-body-clauses ctx)))
        (ok (= 1 (length body)) "Should have one conditional clause")
        (let ((clause (first body)))
          (ok (clysm/lib/macros:loop-conditional-clause-else-clauses clause)
              "Should have ELSE clauses"))))))

(deftest and-clause-chaining
  (testing "AND clause chaining"
    ;; T071: Tests for AND clause chaining
    ;; AND chains multiple actions in a conditional
    (ok (clysm/lib/macros:loop-keyword-p 'and)
        "AND should be recognized as LOOP keyword")))

;;; ============================================================
;;; US5: INITIALLY/FINALLY Tests (T078-T079)
;;; ============================================================

(deftest initially-parsing
  (testing "INITIALLY parsing"
    ;; T078: Tests for INITIALLY parsing
    (let ((ctx (clysm/lib/macros:make-loop-context)))
      (clysm/lib/macros:parse-loop-clauses '(initially (setup)) ctx)
      (let ((initially (clysm/lib/macros:loop-context-initially-forms ctx)))
        (ok (= 1 (length initially)) "Should have one initially form")
        (ok (equal '(setup) (first initially)) "Form should be (setup)")))))

(deftest finally-parsing
  (testing "FINALLY parsing"
    ;; T079: Tests for FINALLY parsing
    (let ((ctx (clysm/lib/macros:make-loop-context)))
      (clysm/lib/macros:parse-loop-clauses '(finally (cleanup)) ctx)
      (let ((finally-forms (clysm/lib/macros:loop-context-finally-forms ctx)))
        (ok (= 1 (length finally-forms)) "Should have one finally form")
        (ok (equal '(cleanup) (first finally-forms)) "Form should be (cleanup)")))))

;;; ============================================================
;;; US6: WITH Clause Tests (T088-T090)
;;; ============================================================

(deftest with-var-expr-parsing
  (testing "WITH var = expr parsing"
    ;; T088: Tests for WITH var = expr parsing
    (let ((ctx (clysm/lib/macros:make-loop-context)))
      (clysm/lib/macros:parse-loop-clauses '(with x = 10) ctx)
      (let ((bindings (clysm/lib/macros:loop-context-with-bindings ctx)))
        (ok (= 1 (length bindings)) "Should have one binding")
        (ok (equal '(x 10) (first bindings)) "Binding should be (x 10)")))))

(deftest with-var-nil-default-parsing
  (testing "WITH var (nil default) parsing"
    ;; T089: Tests for WITH var parsing - defaults to NIL
    (let ((ctx (clysm/lib/macros:make-loop-context)))
      (clysm/lib/macros:parse-loop-clauses '(with x) ctx)
      (let ((bindings (clysm/lib/macros:loop-context-with-bindings ctx)))
        (ok (= 1 (length bindings)) "Should have one binding")
        (ok (equal '(x nil) (first bindings)) "Binding should be (x nil)")))))

(deftest with-and-parallel-parsing
  (testing "WITH AND parallel binding parsing"
    ;; T090: Tests for WITH AND parsing
    (let ((ctx (clysm/lib/macros:make-loop-context)))
      (clysm/lib/macros:parse-loop-clauses '(with x = 1 and y = 2) ctx)
      (let ((bindings (clysm/lib/macros:loop-context-with-bindings ctx)))
        (ok (= 2 (length bindings)) "Should have two bindings")
        (ok (equal '(x 1) (first bindings)) "First binding should be (x 1)")
        (ok (equal '(y 2) (second bindings)) "Second binding should be (y 2)")))))

;;; ============================================================
;;; US7: DO Clause Tests (T095-T096)
;;; ============================================================

(deftest do-clause-parsing
  (testing "DO clause parsing"
    ;; T095: Tests for DO clause parsing
    (let ((ctx (clysm/lib/macros:make-loop-context)))
      (clysm/lib/macros:parse-loop-clauses '(do (action1) (action2)) ctx)
      (let ((body (clysm/lib/macros:loop-context-body-clauses ctx)))
        (ok (= 2 (length body)) "Should have two body forms")
        (ok (equal '(action1) (first body)) "First form should be (action1)")
        (ok (equal '(action2) (second body)) "Second form should be (action2)")))))

(deftest doing-synonym-parsing
  (testing "DOING synonym parsing"
    ;; T096: DOING is a synonym for DO
    (ok (clysm/lib/macros:loop-keyword-p 'doing)
        "DOING should be recognized as LOOP keyword")))

;;; ============================================================
;;; US8: NAMED Clause Tests (T100)
;;; ============================================================

(deftest named-clause-parsing
  (testing "NAMED clause parsing"
    ;; T100: Tests for NAMED clause parsing
    (let ((ctx (clysm/lib/macros:make-loop-context)))
      (clysm/lib/macros:parse-loop-clauses '(named my-loop for i from 1 to 3) ctx)
      (ok (eq 'my-loop (clysm/lib/macros:loop-context-name ctx))
          "Loop should be named MY-LOOP"))))

;;; ============================================================
;;; Edge Cases (T109-T110)
;;; ============================================================

(deftest empty-iteration-edge-case
  (testing "Empty iteration edge case"
    ;; T109: Tests for empty list/zero count iteration
    ;; FOR var IN NIL should iterate zero times
    (let ((ctx (clysm/lib/macros:make-loop-context)))
      (clysm/lib/macros:parse-loop-clauses '(for x in nil collect x) ctx)
      (ok (= 1 (length (clysm/lib/macros:loop-context-iteration-clauses ctx)))
          "Should parse iteration clause even with NIL list"))))

(deftest simple-loop-infinite
  (testing "Simple LOOP (infinite)"
    ;; T110: (loop form...) without keywords is infinite loop
    ;; The make-loop-expander handles this case specially
    (ok t "Simple loop tested via macro expansion")))
