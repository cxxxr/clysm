;;;; wat-tests.lisp - WAT text format output tests

(in-package #:clysm/tests)

(in-suite :wasm)

;;; Opcode Name Tests

(test opcode-name-basic
  "Test basic opcode to mnemonic conversion."
  (is (string= "i32.const" (clysm/wasm:opcode-name #x41)))
  (is (string= "i32.add" (clysm/wasm:opcode-name #x6a)))
  (is (string= "local.get" (clysm/wasm:opcode-name #x20)))
  (is (string= "call" (clysm/wasm:opcode-name #x10)))
  (is (string= "if" (clysm/wasm:opcode-name #x04)))
  (is (string= "end" (clysm/wasm:opcode-name #x0b))))

(test opcode-name-control
  "Test control flow opcode names."
  (is (string= "block" (clysm/wasm:opcode-name #x02)))
  (is (string= "loop" (clysm/wasm:opcode-name #x03)))
  (is (string= "br" (clysm/wasm:opcode-name #x0c)))
  (is (string= "br_if" (clysm/wasm:opcode-name #x0d)))
  (is (string= "return" (clysm/wasm:opcode-name #x0f))))

(test opcode-name-comparison
  "Test comparison opcode names."
  (is (string= "i32.eqz" (clysm/wasm:opcode-name #x45)))
  (is (string= "i32.eq" (clysm/wasm:opcode-name #x46)))
  (is (string= "i32.lt_s" (clysm/wasm:opcode-name #x48)))
  (is (string= "i32.gt_s" (clysm/wasm:opcode-name #x4a))))

(test opcode-name-arithmetic
  "Test arithmetic opcode names."
  (is (string= "i32.sub" (clysm/wasm:opcode-name #x6b)))
  (is (string= "i32.mul" (clysm/wasm:opcode-name #x6c)))
  (is (string= "i32.div_s" (clysm/wasm:opcode-name #x6d)))
  (is (string= "i32.and" (clysm/wasm:opcode-name #x71)))
  (is (string= "i32.or" (clysm/wasm:opcode-name #x72))))

(test opcode-name-unknown
  "Test unknown opcode handling."
  (let ((name (clysm/wasm:opcode-name #xff)))
    (is (search "unknown" name))))

(test gc-opcode-name
  "Test GC instruction opcode names."
  (is (string= "struct.new" (clysm/wasm:gc-opcode-name 0)))
  (is (string= "struct.get" (clysm/wasm:gc-opcode-name 2)))
  (is (string= "array.new" (clysm/wasm:gc-opcode-name 6)))
  (is (string= "array.len" (clysm/wasm:gc-opcode-name 15)))
  (is (string= "ref.i31" (clysm/wasm:gc-opcode-name 28))))

;;; Value Type Name Tests

(test value-type-name-basic
  "Test value type to name conversion."
  (is (string= "i32" (clysm/wasm:value-type-name #x7f)))
  (is (string= "i64" (clysm/wasm:value-type-name #x7e)))
  (is (string= "f32" (clysm/wasm:value-type-name #x7d)))
  (is (string= "f64" (clysm/wasm:value-type-name #x7c))))

(test value-type-name-reference
  "Test reference type names."
  (is (string= "funcref" (clysm/wasm:value-type-name #x70)))
  (is (string= "externref" (clysm/wasm:value-type-name #x6f)))
  (is (string= "anyref" (clysm/wasm:value-type-name #x6e))))

;;; Expression Tree Building Tests

(test build-tree-simple-const
  "Test expression tree for simple constant."
  (let ((tree (clysm/wasm:build-expression-tree
               '((#x41 42)))))  ; i32.const 42
    (is (not (null tree)))
    (is (= #x41 (clysm/wasm::wat-node-opcode tree)))
    (is (equal '(42) (clysm/wasm::wat-node-args tree)))))

(test build-tree-binary-op
  "Test expression tree for binary operation."
  (let ((tree (clysm/wasm:build-expression-tree
               '((#x41 1)       ; i32.const 1
                 (#x41 2)       ; i32.const 2
                 #x6a))))       ; i32.add
    (is (not (null tree)))
    ;; Top node should be i32.add
    (is (= #x6a (clysm/wasm::wat-node-opcode tree)))
    ;; Should have 2 children
    (is (= 2 (length (clysm/wasm::wat-node-children tree))))))

(test build-tree-nested
  "Test expression tree for nested operations."
  (let ((tree (clysm/wasm:build-expression-tree
               '((#x41 1)       ; i32.const 1
                 (#x41 2)       ; i32.const 2
                 #x6a           ; i32.add
                 (#x41 3)       ; i32.const 3
                 #x6c))))       ; i32.mul  -> (* (+ 1 2) 3)
    (is (not (null tree)))
    ;; Top node should be i32.mul
    (is (= #x6c (clysm/wasm::wat-node-opcode tree)))
    ;; One child should be the i32.add
    (let ((children (clysm/wasm::wat-node-children tree)))
      (is (= 2 (length children)))
      ;; First child is the add (nested)
      (is (= #x6a (clysm/wasm::wat-node-opcode (first children)))))))

;;; Module to WAT Output Tests

(test module-to-wat-empty
  "Test WAT output for empty module."
  (let* ((module (make-wasm-module))
         (wat (with-output-to-string (s)
                (clysm/wasm:module-to-wat module :stream s))))
    (is (search "(module" wat))
    (is (search ")" wat))))

(test module-to-wat-simple-function
  "Test WAT output for simple function."
  (let ((module (make-wasm-module)))
    ;; Add type: () -> i32
    (add-func-type module nil (list +type-i32+))
    ;; Add function: i32.const 42
    (add-function module 0 nil '((#x41 42)))
    ;; Export
    (add-export module "answer" +export-func+ 0)
    (finalize-module module)
    (let ((wat (with-output-to-string (s)
                 (clysm/wasm:module-to-wat module :stream s))))
      ;; Check structure
      (is (search "(module" wat))
      (is (search "(type" wat))
      (is (search "(func" wat))
      (is (search "(export" wat))
      (is (search "i32.const 42" wat)))))

(test module-to-wat-add-function
  "Test WAT output for add function."
  (let ((module (make-wasm-module)))
    ;; Add type: (i32, i32) -> i32
    (add-func-type module
                   (list +type-i32+ +type-i32+)
                   (list +type-i32+))
    ;; Add function: local.get 0, local.get 1, i32.add
    (add-function module 0 nil
                  '((#x20 0)     ; local.get 0
                    (#x20 1)     ; local.get 1
                    #x6a))       ; i32.add
    ;; Export
    (add-export module "add" +export-func+ 0)
    (finalize-module module)
    (let ((wat (with-output-to-string (s)
                 (clysm/wasm:module-to-wat module :stream s))))
      (is (search "local.get 0" wat))
      (is (search "local.get 1" wat))
      (is (search "i32.add" wat))
      (is (search "\"add\"" wat)))))

(test module-to-wat-with-source-info
  "Test WAT output with source comments."
  (let ((module (make-wasm-module)))
    (add-func-type module nil (list +type-i32+))
    (add-function module 0 nil '((#x41 42)))
    (add-export module "answer" +export-func+ 0)
    (finalize-module module)
    (let ((wat (with-output-to-string (s)
                 (clysm/wasm:module-to-wat module
                                           :stream s
                                           :source-info '((defun answer () 42))))))
      ;; Source comment should appear
      (is (search ";; Lisp:" wat))
      (is (search "ANSWER" wat)))))

(test module-to-wat-flat-style
  "Test flat style WAT output."
  (let ((module (make-wasm-module)))
    (add-func-type module nil (list +type-i32+))
    (add-function module 0 nil
                  '((#x41 1)
                    (#x41 2)
                    #x6a))
    (finalize-module module)
    (let ((wat (with-output-to-string (s)
                 (clysm/wasm:module-to-wat module :stream s :style :flat))))
      ;; Flat style: instructions on separate lines
      (is (search "i32.const 1" wat))
      (is (search "i32.const 2" wat))
      (is (search "i32.add" wat)))))

(test module-to-wat-folded-style
  "Test folded (S-expression) style WAT output."
  (let ((module (make-wasm-module)))
    (add-func-type module nil (list +type-i32+))
    (add-function module 0 nil
                  '((#x41 1)
                    (#x41 2)
                    #x6a))
    (finalize-module module)
    (let ((wat (with-output-to-string (s)
                 (clysm/wasm:module-to-wat module :stream s :style :folded))))
      ;; Folded style: nested S-expressions
      (is (search "(i32.add" wat)))))

;;; Compiler Integration Tests

(test compile-to-wat-simple
  "Test compile-to-wat integration."
  (let ((wat (clysm/compiler:disassemble-to-string '((+ 1 2)))))
    (is (stringp wat))
    (is (search "(module" wat))
    ;; Should contain the compiled expression
    (is (or (search "i32.add" wat)
            (search "i32.const" wat)))))

(test disassemble-form-returns-string
  "Test disassemble-form returns valid output."
  (let ((output (with-output-to-string (*standard-output*)
                  (clysm/compiler:disassemble-form '(+ 1 2)))))
    (is (stringp output))
    (is (> (length output) 0))))
