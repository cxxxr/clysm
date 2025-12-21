/**
 * CLYSM Evaluator - Lisp interpreter
 *
 * Implements eval for S-expressions.
 */

/**
 * JavaScript closure representation for the interpreter
 * Since we can't easily pass JS functions to Wasm, we use this wrapper
 */
class InterpretedClosure {
  constructor(params, body, env, evaluator, name = null) {
    this.params = params;    // parameter list (Lisp list)
    this.body = body;        // body forms (Lisp list)
    this.env = env;          // lexical environment
    this.evaluator = evaluator; // reference to evaluator for applying
    this.name = name;        // optional function name
  }
}

/**
 * Evaluator class - evaluates Lisp expressions
 */
class Evaluator {
  constructor(kernel) {
    this.kernel = kernel;
    this.primitives = new Map();
    this.specialForms = new Map();
    this.specialVariables = new Set(); // track which symbols are special
    this.bindingStack = [];  // stack of {symbol, oldValue} for dynamic binding
    this.initSpecialForms();
    this.initPrimitives();
  }

  /**
   * Check if value is an interpreted closure
   */
  isInterpretedClosure(x) {
    return x instanceof InterpretedClosure;
  }

  /**
   * Check if a symbol is a special (dynamic) variable
   * Uses the *foo* naming convention
   */
  isSpecialVariable(sym) {
    const k = this.kernel;
    if (!k.isSymbol(sym)) return false;
    const name = k.stringToJS(k.symbolName(sym));
    return (name.startsWith('*') && name.endsWith('*') && name.length > 2) ||
           this.specialVariables.has(sym);
  }

  /**
   * Declare a symbol as special
   */
  declareSpecial(sym) {
    this.specialVariables.add(sym);
  }

  /**
   * Bind a special variable (push old value onto stack)
   */
  bindSpecial(sym, value) {
    const k = this.kernel;
    const oldValue = k.symbolValue(sym);
    this.bindingStack.push({ symbol: sym, oldValue: oldValue });
    k.setSymbolValue(sym, value);
  }

  /**
   * Unbind a special variable (restore old value from stack)
   */
  unbindSpecial(sym) {
    const k = this.kernel;
    if (this.bindingStack.length === 0) {
      throw new Error('Binding stack underflow');
    }
    const entry = this.bindingStack.pop();
    if (!k.eq(entry.symbol, sym)) {
      throw new Error('Binding stack mismatch');
    }
    k.setSymbolValue(sym, entry.oldValue);
  }

  /**
   * Unbind multiple special variables in reverse order
   */
  unbindSpecials(count) {
    for (let i = 0; i < count; i++) {
      const entry = this.bindingStack.pop();
      this.kernel.setSymbolValue(entry.symbol, entry.oldValue);
    }
  }

  /**
   * Initialize special forms
   */
  initSpecialForms() {
    this.specialForms.set('QUOTE', (args, env) => this.evalQuote(args, env));
    this.specialForms.set('IF', (args, env) => this.evalIf(args, env));
    this.specialForms.set('PROGN', (args, env) => this.evalProgn(args, env));
    this.specialForms.set('SETQ', (args, env) => this.evalSetq(args, env));
    this.specialForms.set('LAMBDA', (args, env) => this.evalLambda(args, env));
    this.specialForms.set('LET', (args, env) => this.evalLet(args, env));
    this.specialForms.set('LET*', (args, env) => this.evalLetStar(args, env));
    this.specialForms.set('DEFUN', (args, env) => this.evalDefun(args, env));
    this.specialForms.set('DEFVAR', (args, env) => this.evalDefvar(args, env));
    this.specialForms.set('FUNCTION', (args, env) => this.evalFunction(args, env));
  }

  /**
   * Initialize primitive functions
   */
  initPrimitives() {
    const k = this.kernel;

    // Type predicates
    this.primitives.set('NULL', (args) => this.primNull(args));
    this.primitives.set('ATOM', (args) => this.primAtom(args));
    this.primitives.set('CONSP', (args) => k.isCons(this.nth(args, 0)) ? k.T : k.NIL);
    this.primitives.set('SYMBOLP', (args) => k.isSymbol(this.nth(args, 0)) ? k.T : k.NIL);
    this.primitives.set('NUMBERP', (args) => k.isFixnum(this.nth(args, 0)) ? k.T : k.NIL);
    this.primitives.set('STRINGP', (args) => k.isString(this.nth(args, 0)) ? k.T : k.NIL);
    this.primitives.set('FUNCTIONP', (args) => {
      const x = this.nth(args, 0);
      return (k.isFunction(x) || this.isInterpretedClosure(x)) ? k.T : k.NIL;
    });

    // Cons operations
    this.primitives.set('CAR', (args) => k.car(this.nth(args, 0)));
    this.primitives.set('CDR', (args) => k.cdr(this.nth(args, 0)));
    this.primitives.set('CONS', (args) => k.cons(this.nth(args, 0), this.nth(args, 1)));
    this.primitives.set('RPLACA', (args) => k.rplaca(this.nth(args, 0), this.nth(args, 1)));
    this.primitives.set('RPLACD', (args) => k.rplacd(this.nth(args, 0), this.nth(args, 1)));

    // List operations
    this.primitives.set('LIST', (args) => args);
    this.primitives.set('LENGTH', (args) => this.primLength(args));

    // Equality
    this.primitives.set('EQ', (args) => k.eq(this.nth(args, 0), this.nth(args, 1)) ? k.T : k.NIL);
    this.primitives.set('EQL', (args) => this.primEql(args));
    this.primitives.set('EQUAL', (args) => this.primEqual(args));

    // Arithmetic
    this.primitives.set('+', (args) => this.primAdd(args));
    this.primitives.set('-', (args) => this.primSub(args));
    this.primitives.set('*', (args) => this.primMul(args));
    this.primitives.set('/', (args) => this.primDiv(args));

    // Comparison
    this.primitives.set('<', (args) => this.primLt(args));
    this.primitives.set('>', (args) => this.primGt(args));
    this.primitives.set('<=', (args) => this.primLe(args));
    this.primitives.set('>=', (args) => this.primGe(args));
    this.primitives.set('=', (args) => this.primNumEq(args));

    // Logic
    this.primitives.set('NOT', (args) => k.isNull(this.nth(args, 0)) ? k.T : k.NIL);

    // Print
    this.primitives.set('PRINT', (args) => {
      // Printer will be used by REPL, just return the value
      return this.nth(args, 0);
    });

    // Higher-order
    this.primitives.set('FUNCALL', (args) => this.primFuncall(args));
    this.primitives.set('APPLY', (args) => this.primApply(args));
  }

  /**
   * Get nth element from a list
   */
  nth(list, n) {
    let current = list;
    for (let i = 0; i < n; i++) {
      if (this.kernel.isNull(current)) return this.kernel.NIL;
      current = this.kernel.cdr(current);
    }
    if (this.kernel.isNull(current)) return this.kernel.NIL;
    return this.kernel.car(current);
  }

  /**
   * Count list length
   */
  listLength(list) {
    let count = 0;
    let current = list;
    while (this.kernel.isCons(current)) {
      count++;
      current = this.kernel.cdr(current);
    }
    return count;
  }

  /**
   * Main eval function
   */
  eval(expr, env = null) {
    const k = this.kernel;

    // NIL environment means top-level
    if (env === null) {
      env = k.NIL;
    }

    // Self-evaluating objects
    if (k.isFixnum(expr) || k.isString(expr) || k.isCharacter(expr)) {
      return expr;
    }

    // NIL evaluates to itself
    if (k.isNull(expr)) {
      return expr;
    }

    // Symbol evaluation
    if (k.isSymbol(expr)) {
      return this.evalSymbol(expr, env);
    }

    // List (special form or function call)
    if (k.isCons(expr)) {
      return this.evalList(expr, env);
    }

    // Other types (vector, closure, etc.) return themselves
    return expr;
  }

  /**
   * Evaluate a symbol (variable reference)
   */
  evalSymbol(sym, env) {
    const k = this.kernel;

    // T evaluates to itself
    if (k.eq(sym, k.T)) {
      return k.T;
    }

    // Keywords evaluate to themselves
    if (!k.isNull(k.symbolPackage(sym)) &&
        k.eq(k.symbolPackage(sym), k.KEYWORD)) {
      return sym;
    }

    // Look up in lexical environment first
    // (For now, we use a simple approach - the environment is a list of frames)
    if (k.isEnvFrame(env)) {
      // Search through environment chain for the symbol
      // This is a simple linear search; a real implementation would use indices
      let current = env;
      while (k.isEnvFrame(current)) {
        // For simplicity, we store (symbol . value) pairs in the bindings
        // This is less efficient but simpler to implement initially
        const size = k.envSize(current);
        for (let i = 0; i < size; i += 2) {
          const boundSym = k.envRef(current, i);
          if (k.eq(boundSym, sym)) {
            return k.envRef(current, i + 1);
          }
        }
        current = k.envParent(current);
      }
    }

    // Fall back to symbol's value cell (global/special variable)
    const value = k.symbolValue(sym);
    if (k.isUnbound(value)) {
      throw new Error(`Unbound variable: ${k.stringToJS(k.symbolName(sym))}`);
    }
    return value;
  }

  /**
   * Evaluate a list (special form or function call)
   */
  evalList(list, env) {
    const k = this.kernel;
    const head = k.car(list);
    const args = k.cdr(list);

    // Check for special form
    if (k.isSymbol(head)) {
      const name = k.stringToJS(k.symbolName(head));
      const specialForm = this.specialForms.get(name);
      if (specialForm) {
        return specialForm(args, env);
      }
    }

    // Function call
    return this.evalFunctionCall(head, args, env);
  }

  /**
   * Evaluate function call
   */
  evalFunctionCall(head, args, env) {
    const k = this.kernel;

    // Evaluate the function position
    let func;
    if (k.isSymbol(head)) {
      // Get function from symbol's function cell
      func = k.symbolFunction(head);
      if (k.isNull(func) || k.isUnbound(func)) {
        // Check for primitive
        const name = k.stringToJS(k.symbolName(head));
        const prim = this.primitives.get(name);
        if (prim) {
          // Evaluate arguments
          const evaledArgs = this.evalArgs(args, env);
          return prim(evaledArgs);
        }
        throw new Error(`Undefined function: ${name}`);
      }
    } else if (k.isCons(head)) {
      // Lambda expression in function position
      func = this.eval(head, env);
    } else {
      func = head;
    }

    // Evaluate arguments
    const evaledArgs = this.evalArgs(args, env);

    // Apply the function
    return this.apply(func, evaledArgs);
  }

  /**
   * Evaluate argument list
   */
  evalArgs(args, env) {
    const k = this.kernel;
    if (k.isNull(args)) {
      return k.NIL;
    }
    const car = this.eval(k.car(args), env);
    const cdr = this.evalArgs(k.cdr(args), env);
    return k.cons(car, cdr);
  }

  /**
   * Apply a function to arguments
   */
  apply(func, args) {
    const k = this.kernel;

    // Check for interpreted closure (created by this evaluator)
    if (this.isInterpretedClosure(func)) {
      return this.applyInterpretedClosure(func, args);
    }

    // Check for Wasm closure (future)
    if (k.isClosure(func)) {
      return k.applyClosure(func, args);
    }

    throw new Error('Not a function');
  }

  /**
   * Apply an interpreted closure
   */
  applyInterpretedClosure(closure, args) {
    const k = this.kernel;

    // Create new environment frame
    const params = k.toArray(closure.params);
    const argValues = k.toArray(args);

    // Create bindings (pairs of symbol, value)
    const frameSize = params.length * 2;
    const newEnv = k.makeEnvFrame(closure.env, frameSize);

    for (let i = 0; i < params.length; i++) {
      const value = i < argValues.length ? argValues[i] : k.NIL;
      k.envSet(newEnv, i * 2, params[i]);
      k.envSet(newEnv, i * 2 + 1, value);
    }

    // Evaluate body in new environment
    return this.evalProgn(closure.body, newEnv);
  }

  // === Special Forms ===

  /**
   * (quote x) -> x
   */
  evalQuote(args, env) {
    return this.kernel.car(args);
  }

  /**
   * (if test then else?)
   */
  evalIf(args, env) {
    const k = this.kernel;
    const test = this.eval(k.car(args), env);
    if (!k.isNull(test)) {
      // then
      return this.eval(k.car(k.cdr(args)), env);
    } else {
      // else
      const elsePart = k.cdr(k.cdr(args));
      if (k.isNull(elsePart)) {
        return k.NIL;
      }
      return this.eval(k.car(elsePart), env);
    }
  }

  /**
   * (progn form*) -> last form's value
   */
  evalProgn(args, env) {
    const k = this.kernel;
    let result = k.NIL;
    let forms = args;
    while (!k.isNull(forms)) {
      result = this.eval(k.car(forms), env);
      forms = k.cdr(forms);
    }
    return result;
  }

  /**
   * (setq sym val sym val ...)
   */
  evalSetq(args, env) {
    const k = this.kernel;
    let result = k.NIL;
    let pairs = args;
    while (!k.isNull(pairs)) {
      const sym = k.car(pairs);
      pairs = k.cdr(pairs);
      if (k.isNull(pairs)) {
        throw new Error('Odd number of arguments to SETQ');
      }
      const val = this.eval(k.car(pairs), env);
      pairs = k.cdr(pairs);

      // Try to set in lexical environment first
      let found = false;
      if (k.isEnvFrame(env)) {
        let current = env;
        while (k.isEnvFrame(current)) {
          const size = k.envSize(current);
          for (let i = 0; i < size; i += 2) {
            if (k.eq(k.envRef(current, i), sym)) {
              k.envSet(current, i + 1, val);
              found = true;
              break;
            }
          }
          if (found) break;
          current = k.envParent(current);
        }
      }

      // Fall back to symbol value cell
      if (!found) {
        k.setSymbolValue(sym, val);
      }

      result = val;
    }
    return result;
  }

  /**
   * (lambda (params) body*)
   */
  evalLambda(args, env) {
    const k = this.kernel;
    const params = k.car(args);
    const body = k.cdr(args);

    // Create an interpreted closure
    return new InterpretedClosure(params, body, env, this);
  }

  /**
   * (let ((var val)*) body*)
   */
  evalLet(args, env) {
    const k = this.kernel;
    const bindings = k.car(args);
    const body = k.cdr(args);

    // Evaluate all values in the current environment (before any bindings)
    const bindingList = k.toArray(bindings);
    const lexicalSymbols = [];
    const lexicalValues = [];
    const specialBindings = [];  // {symbol, value} for special variables

    for (const binding of bindingList) {
      let sym, val;
      if (k.isSymbol(binding)) {
        sym = binding;
        val = k.NIL;
      } else {
        sym = k.car(binding);
        const valForm = k.car(k.cdr(binding));
        val = this.eval(valForm, env);
      }

      if (this.isSpecialVariable(sym)) {
        specialBindings.push({ symbol: sym, value: val });
      } else {
        lexicalSymbols.push(sym);
        lexicalValues.push(val);
      }
    }

    // Create lexical environment frame for non-special bindings
    let newEnv = env;
    if (lexicalSymbols.length > 0) {
      const frameSize = lexicalSymbols.length * 2;
      newEnv = k.makeEnvFrame(env, frameSize);
      for (let i = 0; i < lexicalSymbols.length; i++) {
        k.envSet(newEnv, i * 2, lexicalSymbols[i]);
        k.envSet(newEnv, i * 2 + 1, lexicalValues[i]);
      }
    }

    // Bind special variables dynamically
    for (const { symbol, value } of specialBindings) {
      this.bindSpecial(symbol, value);
    }

    try {
      // Evaluate body
      return this.evalProgn(body, newEnv);
    } finally {
      // Unbind special variables in reverse order
      this.unbindSpecials(specialBindings.length);
    }
  }

  /**
   * (let* ((var val)*) body*)
   */
  evalLetStar(args, env) {
    const k = this.kernel;
    const bindings = k.car(args);
    const body = k.cdr(args);

    // For let*, we process bindings sequentially
    let currentEnv = env;
    let bindingList = bindings;
    let specialBindingsCount = 0;

    while (!k.isNull(bindingList)) {
      const binding = k.car(bindingList);
      let sym, val;

      if (k.isSymbol(binding)) {
        sym = binding;
        val = k.NIL;
      } else {
        sym = k.car(binding);
        const valForm = k.car(k.cdr(binding));
        val = this.eval(valForm, currentEnv);
      }

      if (this.isSpecialVariable(sym)) {
        // Dynamic binding for special variables
        this.bindSpecial(sym, val);
        specialBindingsCount++;
      } else {
        // Create new lexical frame for this binding
        const newEnv = k.makeEnvFrame(currentEnv, 2);
        k.envSet(newEnv, 0, sym);
        k.envSet(newEnv, 1, val);
        currentEnv = newEnv;
      }

      bindingList = k.cdr(bindingList);
    }

    try {
      return this.evalProgn(body, currentEnv);
    } finally {
      // Unbind special variables in reverse order
      this.unbindSpecials(specialBindingsCount);
    }
  }

  /**
   * (defun name (params) body*)
   */
  evalDefun(args, env) {
    const k = this.kernel;
    const name = k.car(args);
    const params = k.car(k.cdr(args));
    const body = k.cdr(k.cdr(args));

    // Create lambda and set as function
    const lambda = k.cons(params, body);
    const closure = this.evalLambda(lambda, k.NIL);

    // Set symbol's function cell
    k.setSymbolFunction(name, closure);

    return name;
  }

  /**
   * (defvar name value?)
   */
  evalDefvar(args, env) {
    const k = this.kernel;
    const name = k.car(args);

    // Declare as special variable
    this.declareSpecial(name);

    // Only set if currently unbound
    if (k.isUnbound(k.symbolValue(name))) {
      const valForm = k.car(k.cdr(args));
      if (!k.isNull(k.cdr(args))) {
        const val = this.eval(valForm, env);
        k.setSymbolValue(name, val);
      } else {
        k.setSymbolValue(name, k.NIL);
      }
    }

    return name;
  }

  /**
   * (function name) or #'name
   */
  evalFunction(args, env) {
    const k = this.kernel;
    const nameOrLambda = k.car(args);

    if (k.isSymbol(nameOrLambda)) {
      const func = k.symbolFunction(nameOrLambda);
      if (k.isNull(func) || k.isUnbound(func)) {
        throw new Error(`Undefined function: ${k.stringToJS(k.symbolName(nameOrLambda))}`);
      }
      return func;
    }

    // Lambda expression
    if (k.isCons(nameOrLambda)) {
      const head = k.car(nameOrLambda);
      if (k.isSymbol(head) && k.stringToJS(k.symbolName(head)) === 'LAMBDA') {
        return this.evalLambda(k.cdr(nameOrLambda), env);
      }
    }

    throw new Error('Invalid function specifier');
  }

  // === Primitives ===

  primNull(args) {
    const k = this.kernel;
    return k.isNull(this.nth(args, 0)) ? k.T : k.NIL;
  }

  primAtom(args) {
    const k = this.kernel;
    const x = this.nth(args, 0);
    return k.isCons(x) ? k.NIL : k.T;
  }

  primLength(args) {
    const k = this.kernel;
    const list = this.nth(args, 0);
    return k.makeFixnum(this.listLength(list));
  }

  primEql(args) {
    const k = this.kernel;
    const a = this.nth(args, 0);
    const b = this.nth(args, 1);
    // For now, EQL is same as EQ for our types
    return k.eq(a, b) ? k.T : k.NIL;
  }

  primEqual(args) {
    const k = this.kernel;
    const a = this.nth(args, 0);
    const b = this.nth(args, 1);

    // Recursive structural equality
    const equal = (x, y) => {
      if (k.eq(x, y)) return true;
      if (k.isCons(x) && k.isCons(y)) {
        return equal(k.car(x), k.car(y)) && equal(k.cdr(x), k.cdr(y));
      }
      if (k.isString(x) && k.isString(y)) {
        return k.stringEqual(x, y);
      }
      return false;
    };

    return equal(a, b) ? k.T : k.NIL;
  }

  primAdd(args) {
    const k = this.kernel;
    let sum = 0;
    let current = args;
    while (!k.isNull(current)) {
      sum += k.fixnumValue(k.car(current));
      current = k.cdr(current);
    }
    return k.makeFixnum(sum);
  }

  primSub(args) {
    const k = this.kernel;
    if (k.isNull(args)) return k.makeFixnum(0);
    let result = k.fixnumValue(k.car(args));
    let rest = k.cdr(args);
    if (k.isNull(rest)) {
      return k.makeFixnum(-result);
    }
    while (!k.isNull(rest)) {
      result -= k.fixnumValue(k.car(rest));
      rest = k.cdr(rest);
    }
    return k.makeFixnum(result);
  }

  primMul(args) {
    const k = this.kernel;
    let product = 1;
    let current = args;
    while (!k.isNull(current)) {
      product *= k.fixnumValue(k.car(current));
      current = k.cdr(current);
    }
    return k.makeFixnum(product);
  }

  primDiv(args) {
    const k = this.kernel;
    if (k.isNull(args)) throw new Error('/ requires at least one argument');
    let result = k.fixnumValue(k.car(args));
    let rest = k.cdr(args);
    if (k.isNull(rest)) {
      return k.makeFixnum(Math.floor(1 / result));
    }
    while (!k.isNull(rest)) {
      result = Math.floor(result / k.fixnumValue(k.car(rest)));
      rest = k.cdr(rest);
    }
    return k.makeFixnum(result);
  }

  primLt(args) {
    const k = this.kernel;
    const a = k.fixnumValue(this.nth(args, 0));
    const b = k.fixnumValue(this.nth(args, 1));
    return a < b ? k.T : k.NIL;
  }

  primGt(args) {
    const k = this.kernel;
    const a = k.fixnumValue(this.nth(args, 0));
    const b = k.fixnumValue(this.nth(args, 1));
    return a > b ? k.T : k.NIL;
  }

  primLe(args) {
    const k = this.kernel;
    const a = k.fixnumValue(this.nth(args, 0));
    const b = k.fixnumValue(this.nth(args, 1));
    return a <= b ? k.T : k.NIL;
  }

  primGe(args) {
    const k = this.kernel;
    const a = k.fixnumValue(this.nth(args, 0));
    const b = k.fixnumValue(this.nth(args, 1));
    return a >= b ? k.T : k.NIL;
  }

  primNumEq(args) {
    const k = this.kernel;
    const a = k.fixnumValue(this.nth(args, 0));
    const b = k.fixnumValue(this.nth(args, 1));
    return a === b ? k.T : k.NIL;
  }

  primFuncall(args) {
    const k = this.kernel;
    const func = k.car(args);
    const funcArgs = k.cdr(args);
    return this.apply(func, funcArgs);
  }

  primApply(args) {
    const k = this.kernel;
    const func = k.car(args);
    // For (apply f a1 a2 ... list), we need to flatten
    // For simplicity, support (apply f list) for now
    const lastArg = k.car(k.cdr(args));
    return this.apply(func, lastArg);
  }
}

export { Evaluator, InterpretedClosure };
export default Evaluator;
