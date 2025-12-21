/**
 * Eval tests for CLYSM
 */

import { describe, it, before } from 'node:test';
import assert from 'node:assert/strict';
import { loadKernel } from '../js/bridge.js';
import { Reader } from '../js/reader.js';
import { Printer } from '../js/printer.js';

describe('Evaluator', () => {
  let kernel;
  let reader;
  let printer;

  /**
   * Helper to evaluate a string and return the result
   */
  function evalString(input) {
    const expr = reader.readFromString(input);
    return kernel.eval(expr);
  }

  /**
   * Helper to evaluate and print
   */
  function evalPrint(input) {
    return printer.print(evalString(input));
  }

  before(async () => {
    kernel = await loadKernel();
    reader = new Reader(kernel);
    printer = new Printer(kernel);
  });

  describe('Self-evaluating objects', () => {
    it('evaluates numbers', () => {
      assert.strictEqual(evalPrint('42'), '42');
      assert.strictEqual(evalPrint('-17'), '-17');
      assert.strictEqual(evalPrint('0'), '0');
    });

    it('evaluates strings', () => {
      assert.strictEqual(evalPrint('"hello"'), '"hello"');
      assert.strictEqual(evalPrint('""'), '""');
    });

    it('evaluates NIL', () => {
      assert.strictEqual(evalPrint('nil'), 'NIL');
      assert.strictEqual(evalPrint('()'), 'NIL');
    });

    it('evaluates T', () => {
      assert.strictEqual(evalPrint('t'), 'T');
    });

    it('evaluates keywords', () => {
      assert.strictEqual(evalPrint(':test'), ':TEST');
      assert.strictEqual(evalPrint(':foo'), ':FOO');
    });
  });

  describe('Quote', () => {
    it('quotes symbols', () => {
      assert.strictEqual(evalPrint("'foo"), 'FOO');
    });

    it('quotes lists', () => {
      assert.strictEqual(evalPrint("'(1 2 3)"), '(1 2 3)');
    });

    it('quotes nested lists', () => {
      assert.strictEqual(evalPrint("'((a b) (c d))"), '((A B) (C D))');
    });

    it('does not evaluate quoted content', () => {
      // Even though x is unbound, quoting prevents evaluation
      assert.strictEqual(evalPrint("'x"), 'X');
    });
  });

  describe('If', () => {
    it('evaluates then branch when test is true', () => {
      assert.strictEqual(evalPrint('(if t 1 2)'), '1');
    });

    it('evaluates else branch when test is false', () => {
      assert.strictEqual(evalPrint('(if nil 1 2)'), '2');
    });

    it('returns NIL when else is missing and test is false', () => {
      assert.strictEqual(evalPrint('(if nil 1)'), 'NIL');
    });

    it('treats non-NIL as true', () => {
      assert.strictEqual(evalPrint('(if 0 1 2)'), '1');
      assert.strictEqual(evalPrint('(if "hello" 1 2)'), '1');
    });

    it('evaluates test form', () => {
      assert.strictEqual(evalPrint("(if (null '()) 1 2)"), '1');
      assert.strictEqual(evalPrint("(if (null '(a)) 1 2)"), '2');
    });
  });

  describe('Progn', () => {
    it('evaluates empty progn to NIL', () => {
      assert.strictEqual(evalPrint('(progn)'), 'NIL');
    });

    it('returns last value', () => {
      assert.strictEqual(evalPrint('(progn 1 2 3)'), '3');
    });

    it('evaluates all forms in order', () => {
      // Use setq to verify order
      evalString('(defvar *test-order* 0)');
      evalString('(progn (setq *test-order* 1) (setq *test-order* 2))');
      assert.strictEqual(evalPrint('*test-order*'), '2');
    });
  });

  describe('Arithmetic', () => {
    it('adds numbers', () => {
      assert.strictEqual(evalPrint('(+ 1 2)'), '3');
      assert.strictEqual(evalPrint('(+ 1 2 3 4)'), '10');
      assert.strictEqual(evalPrint('(+)'), '0');
    });

    it('subtracts numbers', () => {
      assert.strictEqual(evalPrint('(- 10 3)'), '7');
      assert.strictEqual(evalPrint('(- 10 3 2)'), '5');
      assert.strictEqual(evalPrint('(- 5)'), '-5');
    });

    it('multiplies numbers', () => {
      assert.strictEqual(evalPrint('(* 2 3)'), '6');
      assert.strictEqual(evalPrint('(* 2 3 4)'), '24');
      assert.strictEqual(evalPrint('(*)'), '1');
    });

    it('divides numbers', () => {
      assert.strictEqual(evalPrint('(/ 10 2)'), '5');
      assert.strictEqual(evalPrint('(/ 100 2 5)'), '10');
    });

    it('handles nested arithmetic', () => {
      assert.strictEqual(evalPrint('(+ (* 2 3) (- 10 5))'), '11');
    });
  });

  describe('Comparison', () => {
    it('compares with <', () => {
      assert.strictEqual(evalPrint('(< 1 2)'), 'T');
      assert.strictEqual(evalPrint('(< 2 1)'), 'NIL');
      assert.strictEqual(evalPrint('(< 1 1)'), 'NIL');
    });

    it('compares with >', () => {
      assert.strictEqual(evalPrint('(> 2 1)'), 'T');
      assert.strictEqual(evalPrint('(> 1 2)'), 'NIL');
    });

    it('compares with <=', () => {
      assert.strictEqual(evalPrint('(<= 1 2)'), 'T');
      assert.strictEqual(evalPrint('(<= 1 1)'), 'T');
      assert.strictEqual(evalPrint('(<= 2 1)'), 'NIL');
    });

    it('compares with >=', () => {
      assert.strictEqual(evalPrint('(>= 2 1)'), 'T');
      assert.strictEqual(evalPrint('(>= 1 1)'), 'T');
      assert.strictEqual(evalPrint('(>= 1 2)'), 'NIL');
    });

    it('compares with =', () => {
      assert.strictEqual(evalPrint('(= 1 1)'), 'T');
      assert.strictEqual(evalPrint('(= 1 2)'), 'NIL');
    });
  });

  describe('Type predicates', () => {
    it('null predicate', () => {
      assert.strictEqual(evalPrint('(null nil)'), 'T');
      assert.strictEqual(evalPrint('(null t)'), 'NIL');
      assert.strictEqual(evalPrint("(null '())"), 'T');
      assert.strictEqual(evalPrint("(null '(a))"), 'NIL');
    });

    it('atom predicate', () => {
      assert.strictEqual(evalPrint('(atom 42)'), 'T');
      assert.strictEqual(evalPrint("(atom 'foo)"), 'T');
      assert.strictEqual(evalPrint('(atom nil)'), 'T');
      assert.strictEqual(evalPrint("(atom '(a b))"), 'NIL');
    });

    it('consp predicate', () => {
      assert.strictEqual(evalPrint("(consp '(a b))"), 'T');
      assert.strictEqual(evalPrint('(consp nil)'), 'NIL');
      assert.strictEqual(evalPrint('(consp 42)'), 'NIL');
    });

    it('symbolp predicate', () => {
      assert.strictEqual(evalPrint("(symbolp 'foo)"), 'T');
      assert.strictEqual(evalPrint('(symbolp nil)'), 'T');
      assert.strictEqual(evalPrint('(symbolp 42)'), 'NIL');
    });

    it('numberp predicate', () => {
      assert.strictEqual(evalPrint('(numberp 42)'), 'T');
      assert.strictEqual(evalPrint("(numberp 'foo)"), 'NIL');
    });

    it('stringp predicate', () => {
      assert.strictEqual(evalPrint('(stringp "hello")'), 'T');
      assert.strictEqual(evalPrint('(stringp 42)'), 'NIL');
    });
  });

  describe('Cons operations', () => {
    it('car gets first element', () => {
      assert.strictEqual(evalPrint("(car '(a b c))"), 'A');
    });

    it('cdr gets rest', () => {
      assert.strictEqual(evalPrint("(cdr '(a b c))"), '(B C)');
    });

    it('cons creates pair', () => {
      assert.strictEqual(evalPrint("(cons 'a '(b c))"), '(A B C)');
      assert.strictEqual(evalPrint("(cons 'a 'b)"), '(A . B)');
    });

    it('car of nil is nil', () => {
      assert.strictEqual(evalPrint('(car nil)'), 'NIL');
    });

    it('cdr of nil is nil', () => {
      assert.strictEqual(evalPrint('(cdr nil)'), 'NIL');
    });

    it('car of symbol throws error', () => {
      assert.throws(
        () => evalString("(car '+)"),
        /CAR:.*SYMBOL.*is not a list/
      );
    });

    it('cdr of number throws error', () => {
      assert.throws(
        () => evalString('(cdr 42)'),
        /CDR:.*FIXNUM.*is not a list/
      );
    });

    it('car of string throws error', () => {
      assert.throws(
        () => evalString('(car "hello")'),
        /CAR:.*STRING.*is not a list/
      );
    });
  });

  describe('Equality', () => {
    it('eq compares identity', () => {
      assert.strictEqual(evalPrint("(eq 'a 'a)"), 'T');
      assert.strictEqual(evalPrint("(eq 'a 'b)"), 'NIL');
      assert.strictEqual(evalPrint('(eq nil nil)'), 'T');
    });

    it('equal compares structure', () => {
      assert.strictEqual(evalPrint("(equal '(a b) '(a b))"), 'T');
      assert.strictEqual(evalPrint("(equal '(a b) '(a c))"), 'NIL');
    });
  });

  describe('Let', () => {
    it('binds single variable', () => {
      assert.strictEqual(evalPrint('(let ((x 10)) x)'), '10');
    });

    it('binds multiple variables', () => {
      assert.strictEqual(evalPrint('(let ((x 1) (y 2)) (+ x y))'), '3');
    });

    it('evaluates body forms in order', () => {
      assert.strictEqual(evalPrint('(let ((x 1)) 10 20 x)'), '1');
    });

    it('shadows outer bindings', () => {
      evalString('(defvar *outer* 1)');
      assert.strictEqual(evalPrint('(let ((*outer* 10)) *outer*)'), '10');
    });

    it('restores outer bindings', () => {
      evalString('(defvar *restore* 1)');
      evalString('(let ((*restore* 10)) *restore*)');
      assert.strictEqual(evalPrint('*restore*'), '1');
    });

    it('handles nested let', () => {
      assert.strictEqual(evalPrint('(let ((x 1)) (let ((y 2)) (+ x y)))'), '3');
    });

    it('let bindings are parallel', () => {
      evalString('(defvar *let-test* 1)');
      // In let, all values are evaluated before any binding
      // So y should get the value 1, not 10
      assert.strictEqual(evalPrint('(let ((*let-test* 10) (y *let-test*)) y)'), '1');
    });
  });

  describe('Let*', () => {
    it('binds sequentially', () => {
      // In let*, each binding sees previous bindings
      assert.strictEqual(evalPrint('(let* ((x 1) (y (+ x 1))) y)'), '2');
    });

    it('handles nested references', () => {
      assert.strictEqual(evalPrint('(let* ((a 1) (b (+ a 1)) (c (+ b 1))) c)'), '3');
    });
  });

  describe('Lambda', () => {
    it('creates function', () => {
      const result = evalString('(lambda (x) x)');
      assert.strictEqual(kernel.isInterpretedClosure(result), true);
    });

    it('can be called directly', () => {
      assert.strictEqual(evalPrint('((lambda (x) x) 42)'), '42');
    });

    it('handles multiple parameters', () => {
      assert.strictEqual(evalPrint('((lambda (x y) (+ x y)) 10 20)'), '30');
    });

    it('captures lexical environment', () => {
      assert.strictEqual(evalPrint('(let ((x 10)) ((lambda (y) (+ x y)) 5))'), '15');
    });

    it('handles nested lambdas', () => {
      assert.strictEqual(evalPrint('(((lambda (x) (lambda (y) (+ x y))) 10) 5)'), '15');
    });
  });

  describe('Defun', () => {
    it('defines a function', () => {
      evalString('(defun double (x) (* x 2))');
      assert.strictEqual(evalPrint('(double 21)'), '42');
    });

    it('handles multiple parameters', () => {
      evalString('(defun add3 (a b c) (+ a b c))');
      assert.strictEqual(evalPrint('(add3 1 2 3)'), '6');
    });

    it('handles multiple body forms', () => {
      evalString('(defun multi-body (x) (+ x 1) (+ x 2) (+ x 3))');
      assert.strictEqual(evalPrint('(multi-body 10)'), '13');
    });

    it('can call other functions', () => {
      evalString('(defun sq (x) (* x x))');
      evalString('(defun sq-sum (a b) (+ (sq a) (sq b)))');
      assert.strictEqual(evalPrint('(sq-sum 3 4)'), '25');
    });
  });

  describe('Defvar', () => {
    it('defines a variable', () => {
      evalString('(defvar *test-var* 42)');
      assert.strictEqual(evalPrint('*test-var*'), '42');
    });

    it('can be changed with setq', () => {
      evalString('(defvar *mutable* 1)');
      evalString('(setq *mutable* 2)');
      assert.strictEqual(evalPrint('*mutable*'), '2');
    });
  });

  describe('Function (#\')', () => {
    it('gets function from symbol', () => {
      evalString('(defun my-fn (x) x)');
      const result = evalString("(function my-fn)");
      assert.strictEqual(kernel.isInterpretedClosure(result), true);
    });
  });

  describe('Funcall', () => {
    it('calls a function', () => {
      evalString('(defun addone (x) (+ x 1))');
      assert.strictEqual(evalPrint("(funcall (function addone) 5)"), '6');
    });
  });

  describe('Not', () => {
    it('negates nil to t', () => {
      assert.strictEqual(evalPrint('(not nil)'), 'T');
    });

    it('negates t to nil', () => {
      assert.strictEqual(evalPrint('(not t)'), 'NIL');
    });

    it('negates non-nil to nil', () => {
      assert.strictEqual(evalPrint('(not 42)'), 'NIL');
    });
  });

  describe('List', () => {
    it('creates a list', () => {
      assert.strictEqual(evalPrint('(list 1 2 3)'), '(1 2 3)');
    });

    it('creates empty list', () => {
      assert.strictEqual(evalPrint('(list)'), 'NIL');
    });
  });

  describe('Length', () => {
    it('returns list length', () => {
      assert.strictEqual(evalPrint("(length '(a b c))"), '3');
      assert.strictEqual(evalPrint("(length '())"), '0');
    });
  });

  describe('Complex examples', () => {
    it('factorial function', () => {
      evalString(`
        (defun factorial (n)
          (if (<= n 1)
              1
              (* n (factorial (- n 1)))))
      `);
      assert.strictEqual(evalPrint('(factorial 5)'), '120');
      assert.strictEqual(evalPrint('(factorial 0)'), '1');
      assert.strictEqual(evalPrint('(factorial 1)'), '1');
    });

    it('fibonacci function', () => {
      evalString(`
        (defun fib (n)
          (if (<= n 1)
              n
              (+ (fib (- n 1)) (fib (- n 2)))))
      `);
      assert.strictEqual(evalPrint('(fib 0)'), '0');
      assert.strictEqual(evalPrint('(fib 1)'), '1');
      assert.strictEqual(evalPrint('(fib 10)'), '55');
    });

    it('higher-order function', () => {
      evalString(`
        (defun apply-twice (f x)
          (funcall f (funcall f x)))
      `);
      evalString('(defun addone (x) (+ x 1))');
      assert.strictEqual(evalPrint("(apply-twice (function addone) 0)"), '2');
    });

    it('closure captures environment', () => {
      evalString(`
        (defun make-adder (n)
          (lambda (x) (+ x n)))
      `);
      evalString('(defvar *add5* (make-adder 5))');
      assert.strictEqual(evalPrint('(funcall *add5* 10)'), '15');
    });
  });

  describe('Dynamic scoping', () => {
    it('recognizes *foo* as special variable', () => {
      // Variables with *name* convention are special
      evalString('(defvar *special-var* 10)');
      assert.strictEqual(evalPrint('*special-var*'), '10');
    });

    it('defvar declares variable as special', () => {
      evalString('(defvar *multiplier* 2)');
      evalString(`
        (defun multiply (x)
          (* x *multiplier*))
      `);
      assert.strictEqual(evalPrint('(multiply 10)'), '20');
    });

    it('let dynamically binds special variables', () => {
      evalString('(defvar *dyn-var* 1)');
      evalString(`
        (defun get-dyn-var ()
          *dyn-var*)
      `);
      // Outside let, value is 1
      assert.strictEqual(evalPrint('(get-dyn-var)'), '1');
      // Inside let, value is 100 (dynamic binding)
      assert.strictEqual(evalPrint('(let ((*dyn-var* 100)) (get-dyn-var))'), '100');
      // After let, value is restored to 1
      assert.strictEqual(evalPrint('(get-dyn-var)'), '1');
    });

    it('nested dynamic bindings work correctly', () => {
      evalString('(defvar *nested* 1)');
      evalString('(defun get-nested () *nested*)');
      assert.strictEqual(
        evalPrint(`
          (let ((*nested* 10))
            (let ((*nested* 100))
              (get-nested)))
        `),
        '100'
      );
      assert.strictEqual(evalPrint('(get-nested)'), '1');
    });

    it('let* dynamically binds special variables sequentially', () => {
      evalString('(defvar *seq1* 1)');
      evalString('(defvar *seq2* 2)');
      // In let*, *seq2* should see the new value of *seq1*
      assert.strictEqual(
        evalPrint('(let* ((*seq1* 10) (*seq2* (+ *seq1* 5))) *seq2*)'),
        '15'
      );
    });

    it('dynamic binding is visible in called functions', () => {
      evalString('(defvar *visible* 42)');
      evalString(`
        (defun inner ()
          *visible*)
      `);
      evalString(`
        (defun outer ()
          (let ((*visible* 999))
            (inner)))
      `);
      assert.strictEqual(evalPrint('(outer)'), '999');
    });

    it('lexical and dynamic variables can be mixed in let', () => {
      evalString('(defvar *dyn* 1)');
      assert.strictEqual(
        evalPrint(`
          (let ((lex 10) (*dyn* 20))
            (+ lex *dyn*))
        `),
        '30'
      );
    });
  });

  describe('Non-local exits', () => {
    it('block/return-from exits with value', () => {
      assert.strictEqual(evalPrint('(block foo (return-from foo 42) 1)'), '42');
    });

    it('catch/throw exits with value', () => {
      assert.strictEqual(evalPrint("(catch 'tag (throw 'tag 123) 0)"), '123');
    });

    it('unwind-protect runs cleanup on throw and restores special bindings', () => {
      evalString('(defvar *unwind-x* 1)');
      assert.strictEqual(
        evalPrint("(catch 'tag (let ((*unwind-x* 2)) (unwind-protect (throw 'tag *unwind-x*) (setq *unwind-x* 3))))"),
        '2'
      );
      assert.strictEqual(evalPrint('*unwind-x*'), '1');
    });

    it('unwind-protect runs cleanup on return-from', () => {
      evalString('(defvar *unwind-y* 0)');
      assert.strictEqual(
        evalPrint('(block foo (unwind-protect (return-from foo 10) (setq *unwind-y* 1)) 99)'),
        '10'
      );
      assert.strictEqual(evalPrint('*unwind-y*'), '1');
    });
  });
});
