;;;; tests/unit/primitives-test.lisp - Primitive Operations Tests

(in-package #:clysm/tests)

;;; ============================================================
;;; Primitive Registry Tests
;;; ============================================================

(defsuite primitives-suite
    "Tests for primitive operation code generators")

(deftest test-primitives-initialized ()
  "Test that primitives are initialized"
  (is (> (hash-table-count clysm:*primitives*) 0)))

(deftest test-find-primitive-exists ()
  "Test finding existing primitives"
  (is (clysm:find-primitive '+))
  (is (clysm:find-primitive 'cons))
  (is (clysm:find-primitive 'car))
  (is (clysm:find-primitive 'eq)))

(deftest test-find-primitive-not-exists ()
  "Test finding non-existent primitives"
  (is-false (clysm:find-primitive 'not-a-primitive))
  (is-false (clysm:find-primitive 'foo-bar-baz)))

(deftest test-primitivep ()
  "Test primitivep predicate"
  (is (clysm:primitivep '+))
  (is (clysm:primitivep 'cons))
  (is-false (clysm:primitivep 'not-a-primitive)))

(deftest test-primitive-arity ()
  "Test primitive arity"
  (is-eql 2 (clysm:primitive-arity (clysm:find-primitive '+)))
  (is-eql 2 (clysm:primitive-arity (clysm:find-primitive 'cons)))
  (is-eql 1 (clysm:primitive-arity (clysm:find-primitive 'car)))
  (is-eql 1 (clysm:primitive-arity (clysm:find-primitive 'consp)))
  (is-eql 2 (clysm:primitive-arity (clysm:find-primitive 'eq))))

(deftest test-primitive-pure-p ()
  "Test primitive purity flags"
  (is (clysm:primitive-pure-p (clysm:find-primitive '+)))
  (is (clysm:primitive-pure-p (clysm:find-primitive 'car)))
  (is (clysm:primitive-pure-p (clysm:find-primitive 'consp)))
  (is-false (clysm:primitive-pure-p (clysm:find-primitive 'rplaca))))

(deftest test-primitive-foldable-p ()
  "Test primitive foldability flags"
  (is (clysm:primitive-foldable-p (clysm:find-primitive '+)))
  (is (clysm:primitive-foldable-p (clysm:find-primitive '<)))
  (is (clysm:primitive-foldable-p (clysm:find-primitive 'eq)))
  ;; cons is pure but not foldable (creates new object)
  (is-false (clysm:primitive-foldable-p (clysm:find-primitive 'cons))))

;;; ============================================================
;;; Fixnum Arithmetic Emitter Tests
;;; ============================================================

(deftest test-emit-fixnum-add ()
  "Test fixnum addition bytecode"
  (let ((bytes (clysm:emit-fixnum-add)))
    (is (listp bytes))
    ;; Should contain i32.add (#x6A) and ref.i31 (#xFB #x1C)
    (is (member #x6A bytes))
    (is (member #xFB bytes))))

(deftest test-emit-fixnum-sub ()
  "Test fixnum subtraction bytecode"
  (let ((bytes (clysm:emit-fixnum-sub)))
    (is (listp bytes))
    ;; Should contain i32.sub (#x6B)
    (is (member #x6B bytes))))

(deftest test-emit-fixnum-mul ()
  "Test fixnum multiplication bytecode"
  (let ((bytes (clysm:emit-fixnum-mul)))
    (is (listp bytes))
    ;; Should contain i32.mul (#x6C)
    (is (member #x6C bytes))))

(deftest test-emit-fixnum-div ()
  "Test fixnum division bytecode"
  (let ((bytes (clysm:emit-fixnum-div)))
    (is (listp bytes))
    ;; Should contain i32.div_s (#x6D)
    (is (member #x6D bytes))))

(deftest test-emit-fixnum-rem ()
  "Test fixnum remainder bytecode"
  (let ((bytes (clysm:emit-fixnum-rem)))
    (is (listp bytes))
    ;; Should contain i32.rem_s (#x6F)
    (is (member #x6F bytes))))

;;; ============================================================
;;; Fixnum Comparison Emitter Tests
;;; ============================================================

(deftest test-emit-fixnum-lt ()
  "Test fixnum less-than bytecode"
  (let ((bytes (clysm:emit-fixnum-lt)))
    (is (listp bytes))
    ;; Should contain i32.lt_s (#x48)
    (is-eql #x48 (first bytes))))

(deftest test-emit-fixnum-le ()
  "Test fixnum less-or-equal bytecode"
  (let ((bytes (clysm:emit-fixnum-le)))
    (is (listp bytes))
    ;; Should contain i32.le_s (#x4C)
    (is-eql #x4C (first bytes))))

(deftest test-emit-fixnum-gt ()
  "Test fixnum greater-than bytecode"
  (let ((bytes (clysm:emit-fixnum-gt)))
    (is (listp bytes))
    ;; Should contain i32.gt_s (#x4A)
    (is-eql #x4A (first bytes))))

(deftest test-emit-fixnum-ge ()
  "Test fixnum greater-or-equal bytecode"
  (let ((bytes (clysm:emit-fixnum-ge)))
    (is (listp bytes))
    ;; Should contain i32.ge_s (#x4E)
    (is-eql #x4E (first bytes))))

(deftest test-emit-fixnum-eq ()
  "Test fixnum equality bytecode"
  (let ((bytes (clysm:emit-fixnum-eq)))
    (is (listp bytes))
    ;; Should contain i32.eq (#x46)
    (is-eql #x46 (first bytes))))

(deftest test-emit-fixnum-neq ()
  "Test fixnum inequality bytecode"
  (let ((bytes (clysm:emit-fixnum-neq)))
    (is (listp bytes))
    ;; Should contain i32.ne (#x47)
    (is-eql #x47 (first bytes))))

(deftest test-emit-fixnum-zerop ()
  "Test fixnum zero-p bytecode"
  (let ((bytes (clysm:emit-fixnum-zerop)))
    (is (listp bytes))
    ;; Should contain i32.eqz (#x45)
    (is-eql #x45 (first bytes))))

;;; ============================================================
;;; Cons Operation Emitter Tests
;;; ============================================================

(deftest test-emit-cons ()
  "Test cons creation bytecode"
  (let* ((module (clysm:make-wasm-module))
         (registry (clysm:register-core-types module))
         (bytes (clysm:emit-cons registry)))
    (is (listp bytes))
    ;; Should contain struct.new (#xFB #x00)
    (is-eql #xFB (first bytes))
    (is-eql #x00 (second bytes))))

(deftest test-emit-car-op ()
  "Test car operation bytecode"
  (let* ((module (clysm:make-wasm-module))
         (registry (clysm:register-core-types module))
         (bytes (clysm:emit-car-op registry)))
    (is (listp bytes))
    ;; Should contain struct.get (#xFB #x02)
    (is-eql #xFB (first bytes))
    (is-eql #x02 (second bytes))))

(deftest test-emit-cdr-op ()
  "Test cdr operation bytecode"
  (let* ((module (clysm:make-wasm-module))
         (registry (clysm:register-core-types module))
         (bytes (clysm:emit-cdr-op registry)))
    (is (listp bytes))
    ;; Should contain struct.get (#xFB #x02)
    (is-eql #xFB (first bytes))
    (is-eql #x02 (second bytes))))

(deftest test-emit-rplaca-op ()
  "Test rplaca operation bytecode"
  (let* ((module (clysm:make-wasm-module))
         (registry (clysm:register-core-types module))
         (bytes (clysm:emit-rplaca-op registry)))
    (is (listp bytes))
    ;; Should contain struct.set (#xFB #x05)
    (is-eql #xFB (first bytes))
    (is-eql #x05 (second bytes))))

(deftest test-emit-rplacd-op ()
  "Test rplacd operation bytecode"
  (let* ((module (clysm:make-wasm-module))
         (registry (clysm:register-core-types module))
         (bytes (clysm:emit-rplacd-op registry)))
    (is (listp bytes))
    ;; Should contain struct.set (#xFB #x05)
    (is-eql #xFB (first bytes))
    (is-eql #x05 (second bytes))))

;;; ============================================================
;;; Type Predicate Emitter Tests
;;; ============================================================

(deftest test-emit-consp-op ()
  "Test consp predicate bytecode"
  (let* ((module (clysm:make-wasm-module))
         (registry (clysm:register-core-types module))
         (bytes (clysm:emit-consp-op registry)))
    (is (listp bytes))
    ;; Should contain ref.test (#xFB #x14)
    (is-eql #xFB (first bytes))
    (is-eql #x14 (second bytes))))

(deftest test-emit-atom-op ()
  "Test atom predicate bytecode"
  (let* ((module (clysm:make-wasm-module))
         (registry (clysm:register-core-types module))
         (bytes (clysm:emit-atom-op registry)))
    (is (listp bytes))
    ;; Should contain ref.test and i32.eqz for NOT
    (is (member #xFB bytes))
    (is (member #x45 bytes))))  ; i32.eqz

(deftest test-emit-symbolp-op ()
  "Test symbolp predicate bytecode"
  (let* ((module (clysm:make-wasm-module))
         (registry (clysm:register-core-types module))
         (bytes (clysm:emit-symbolp-op registry)))
    (is (listp bytes))
    ;; Should contain ref.test (#xFB #x14)
    (is-eql #xFB (first bytes))
    (is-eql #x14 (second bytes))))

(deftest test-emit-numberp-op ()
  "Test numberp predicate bytecode"
  (let ((bytes (clysm:emit-numberp-op)))
    (is (listp bytes))
    ;; Should contain ref.test for i31
    (is-eql #xFB (first bytes))
    (is-eql #x14 (second bytes))
    (is-eql #x6C (third bytes))))  ; i31ref type

(deftest test-emit-functionp-op ()
  "Test functionp predicate bytecode"
  (let* ((module (clysm:make-wasm-module))
         (registry (clysm:register-core-types module))
         (bytes (clysm:emit-functionp-op registry)))
    (is (listp bytes))
    ;; Should contain ref.test (#xFB #x14)
    (is-eql #xFB (first bytes))
    (is-eql #x14 (second bytes))))

;;; ============================================================
;;; Equality Emitter Tests
;;; ============================================================

(deftest test-emit-eq-op ()
  "Test eq operation bytecode"
  (let ((bytes (clysm:emit-eq-op)))
    (is (listp bytes))
    ;; Should contain ref.eq (#xD3)
    (is-eql #xD3 (first bytes))))

(deftest test-emit-eql-op ()
  "Test eql operation bytecode"
  (let ((bytes (clysm:emit-eql-op)))
    (is (listp bytes))
    ;; Currently same as eq
    (is-eql #xD3 (first bytes))))

;;; ============================================================
;;; Symbol Operation Emitter Tests
;;; ============================================================

(deftest test-emit-symbol-name ()
  "Test symbol-name accessor bytecode"
  (let* ((module (clysm:make-wasm-module))
         (registry (clysm:register-core-types module))
         (bytes (clysm:emit-symbol-name registry)))
    (is (listp bytes))
    ;; Should contain struct.get
    (is-eql #xFB (first bytes))
    (is-eql #x02 (second bytes))))

(deftest test-emit-symbol-value ()
  "Test symbol-value accessor bytecode"
  (let* ((module (clysm:make-wasm-module))
         (registry (clysm:register-core-types module))
         (bytes (clysm:emit-symbol-value registry)))
    (is (listp bytes))
    ;; Should contain struct.get
    (is-eql #xFB (first bytes))
    (is-eql #x02 (second bytes))))

(deftest test-emit-set-symbol-value ()
  "Test symbol-value setter bytecode"
  (let* ((module (clysm:make-wasm-module))
         (registry (clysm:register-core-types module))
         (bytes (clysm:emit-set-symbol-value registry)))
    (is (listp bytes))
    ;; Should contain struct.set
    (is-eql #xFB (first bytes))
    (is-eql #x05 (second bytes))))

;;; ============================================================
;;; Vector Operation Emitter Tests
;;; ============================================================

(deftest test-emit-make-vector ()
  "Test vector creation bytecode"
  (let* ((module (clysm:make-wasm-module))
         (registry (clysm:register-core-types module))
         (bytes (clysm:emit-make-vector registry)))
    (is (listp bytes))
    ;; Should contain array.new (#xFB #x06)
    (is-eql #xFB (first bytes))
    (is-eql #x06 (second bytes))))

(deftest test-emit-vector-ref ()
  "Test vector-ref bytecode"
  (let* ((module (clysm:make-wasm-module))
         (registry (clysm:register-core-types module))
         (bytes (clysm:emit-vector-ref registry)))
    (is (listp bytes))
    ;; Should contain array.get (#xFB #x0B)
    (is-eql #xFB (first bytes))
    (is-eql #x0B (second bytes))))

(deftest test-emit-vector-set ()
  "Test vector-set bytecode"
  (let* ((module (clysm:make-wasm-module))
         (registry (clysm:register-core-types module))
         (bytes (clysm:emit-vector-set registry)))
    (is (listp bytes))
    ;; Should contain array.set (#xFB #x0E)
    (is-eql #xFB (first bytes))
    (is-eql #x0E (second bytes))))

(deftest test-emit-vector-length ()
  "Test vector-length bytecode"
  (let ((bytes (clysm:emit-vector-length)))
    (is (listp bytes))
    ;; Should contain array.len (#xFB #x0F)
    (is-eql #xFB (first bytes))
    (is-eql #x0F (second bytes))))

;;; ============================================================
;;; Closure/Environment Emitter Tests
;;; ============================================================

(deftest test-emit-closure-env ()
  "Test closure environment accessor bytecode"
  (let* ((module (clysm:make-wasm-module))
         (registry (clysm:register-core-types module))
         (bytes (clysm:emit-closure-env registry)))
    (is (listp bytes))
    ;; Should contain struct.get
    (is-eql #xFB (first bytes))
    (is-eql #x02 (second bytes))))

(deftest test-emit-make-env ()
  "Test environment creation bytecode"
  (let* ((module (clysm:make-wasm-module))
         (registry (clysm:register-core-types module))
         (bytes (clysm:emit-make-env registry 3)))
    (is (listp bytes))
    ;; Should contain array.new_fixed (#xFB #x08)
    (is-eql #xFB (first bytes))
    (is-eql #x08 (second bytes))))

(deftest test-emit-env-ref ()
  "Test environment variable access bytecode"
  (let* ((module (clysm:make-wasm-module))
         (registry (clysm:register-core-types module))
         (bytes (clysm:emit-env-ref registry)))
    (is (listp bytes))
    ;; Should contain array.get (#xFB #x0B)
    (is-eql #xFB (first bytes))
    (is-eql #x0B (second bytes))))

(deftest test-emit-env-set ()
  "Test environment variable mutation bytecode"
  (let* ((module (clysm:make-wasm-module))
         (registry (clysm:register-core-types module))
         (bytes (clysm:emit-env-set registry)))
    (is (listp bytes))
    ;; Should contain array.set (#xFB #x0E)
    (is-eql #xFB (first bytes))
    (is-eql #x0E (second bytes))))

;;; ============================================================
;;; Primitive Generator Integration Tests
;;; ============================================================

(deftest test-primitive-generator-callable ()
  "Test that primitive generators are callable"
  (let* ((module (clysm:make-wasm-module))
         (registry (clysm:register-core-types module)))
    ;; Test arithmetic primitive
    (let* ((prim (clysm:find-primitive '+))
           (gen (clysm:primitive-generator prim))
           (bytes (funcall gen registry)))
      (is (listp bytes)))
    ;; Test cons primitive
    (let* ((prim (clysm:find-primitive 'cons))
           (gen (clysm:primitive-generator prim))
           (bytes (funcall gen registry)))
      (is (listp bytes)))
    ;; Test type predicate
    (let* ((prim (clysm:find-primitive 'consp))
           (gen (clysm:primitive-generator prim))
           (bytes (funcall gen registry)))
      (is (listp bytes)))))

(deftest test-all-primitives-have-generators ()
  "Test that all registered primitives have generators"
  (maphash (lambda (name prim)
             (declare (ignore name))
             (is (functionp (clysm:primitive-generator prim))))
           clysm:*primitives*))

(deftest test-primitive-count ()
  "Test that expected number of primitives are registered"
  ;; We should have at least 30 primitives
  (is (>= (hash-table-count clysm:*primitives*) 30)))
