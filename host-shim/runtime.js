/**
 * runtime.js - Dynamic call runtime support
 * Feature: 001-ffi-import-architecture (T037-T039)
 *
 * Provides runtime function resolution for dynamic funcall/apply patterns.
 * When the compiler detects code like:
 *   (funcall (intern "FOO") x)
 *   (apply fn args)
 *
 * It emits a $dynamic-call import that this module provides.
 *
 * The dynamic call system:
 * 1. Maintains a registry of all callable functions (FFI and Lisp)
 * 2. Resolves function names at runtime
 * 3. Calls the function with provided arguments
 * 4. Returns the result
 */

/**
 * Function registry - maps function names (symbols) to implementations
 * Key: string (uppercase function name, e.g., "IDENTITY", "SIN")
 * Value: function implementation
 */
const functionRegistry = new Map();

/**
 * Register a function in the runtime
 * @param {string} name - Function name (uppercase)
 * @param {Function} fn - Function implementation
 */
export function registerFunction(name, fn) {
    const upperName = typeof name === 'string' ? name.toUpperCase() : String(name).toUpperCase();
    functionRegistry.set(upperName, fn);
}

/**
 * Register multiple functions at once
 * @param {Object} functions - Object mapping names to implementations
 */
export function registerFunctions(functions) {
    for (const [name, fn] of Object.entries(functions)) {
        registerFunction(name, fn);
    }
}

/**
 * Look up a function by name
 * @param {string} name - Function name
 * @returns {Function|undefined} The function or undefined if not found
 */
export function lookupFunction(name) {
    const upperName = typeof name === 'string' ? name.toUpperCase() : String(name).toUpperCase();
    return functionRegistry.get(upperName);
}

/**
 * Check if a function is registered
 * @param {string} name - Function name
 * @returns {boolean} True if function exists
 */
export function hasFunction(name) {
    const upperName = typeof name === 'string' ? name.toUpperCase() : String(name).toUpperCase();
    return functionRegistry.has(upperName);
}

/**
 * Get all registered function names
 * @returns {string[]} Array of function names
 */
export function getAllFunctionNames() {
    return Array.from(functionRegistry.keys());
}

/**
 * Dynamic call error class
 */
export class DynamicCallError extends Error {
    constructor(functionName, message) {
        super(message || `Undefined function: ${functionName}`);
        this.name = 'DynamicCallError';
        this.functionName = functionName;
    }
}

/**
 * Resolve a Clysm symbol to get the function name
 * Clysm symbols are represented as objects with name property
 * @param {Object|string} symbol - Symbol object or name string
 * @returns {string} The function name
 */
function resolveSymbolName(symbol) {
    if (typeof symbol === 'string') {
        return symbol.toUpperCase();
    }
    if (symbol && typeof symbol === 'object') {
        // Clysm symbol object has 'name' field
        if (symbol.name) {
            return typeof symbol.name === 'string'
                ? symbol.name.toUpperCase()
                : String(symbol.name).toUpperCase();
        }
        // Array representation [type, name, ...]
        if (Array.isArray(symbol) && symbol.length >= 2) {
            return String(symbol[1]).toUpperCase();
        }
    }
    // Fallback: stringify and uppercase
    return String(symbol).toUpperCase();
}

/**
 * Dynamic call implementation - the core $dynamic-call function
 * This is imported by Wasm modules that use dynamic funcall/apply
 *
 * @param {Object} symbol - The function to call (Clysm symbol)
 * @param {Array} args - Arguments array
 * @returns {*} The result of calling the function
 * @throws {DynamicCallError} If the function is not found
 */
export function dynamicCall(symbol, args) {
    const name = resolveSymbolName(symbol);
    const fn = lookupFunction(name);

    if (!fn) {
        throw new DynamicCallError(name);
    }

    // Convert Clysm list/array to JavaScript array
    const jsArgs = Array.isArray(args) ? args : [args];

    return fn.apply(null, jsArgs);
}

// T038: Register built-in functions
// These are common Lisp functions that might be called dynamically

// Identity function
registerFunction('IDENTITY', (x) => x);

// List functions
registerFunction('CAR', (cons) => {
    if (Array.isArray(cons) && cons.length > 0) return cons[0];
    if (cons && typeof cons === 'object' && 'car' in cons) return cons.car;
    return null;
});

registerFunction('CDR', (cons) => {
    if (Array.isArray(cons) && cons.length > 1) return cons.slice(1);
    if (cons && typeof cons === 'object' && 'cdr' in cons) return cons.cdr;
    return null;
});

registerFunction('CONS', (car, cdr) => {
    if (Array.isArray(cdr)) return [car, ...cdr];
    return { car, cdr };
});

registerFunction('LIST', (...args) => args);

// Type predicates
registerFunction('NULL', (x) => x === null || x === undefined || (Array.isArray(x) && x.length === 0));
registerFunction('CONSP', (x) => Array.isArray(x) && x.length > 0);
registerFunction('ATOM', (x) => !Array.isArray(x) || x.length === 0);
registerFunction('LISTP', (x) => x === null || Array.isArray(x));
registerFunction('NUMBERP', (x) => typeof x === 'number');
registerFunction('STRINGP', (x) => typeof x === 'string');
registerFunction('SYMBOLP', (x) => x && typeof x === 'object' && x.type === 'symbol');
registerFunction('FUNCTIONP', (x) => typeof x === 'function');

// Arithmetic functions
registerFunction('+', (...args) => args.reduce((a, b) => a + b, 0));
registerFunction('-', (first, ...rest) => rest.length === 0 ? -first : rest.reduce((a, b) => a - b, first));
registerFunction('*', (...args) => args.reduce((a, b) => a * b, 1));
registerFunction('/', (first, ...rest) => rest.reduce((a, b) => a / b, first));

// Comparison functions
registerFunction('=', (...args) => args.every((v, i, arr) => i === 0 || v === arr[i - 1]));
registerFunction('<', (...args) => args.every((v, i, arr) => i === 0 || arr[i - 1] < v));
registerFunction('>', (...args) => args.every((v, i, arr) => i === 0 || arr[i - 1] > v));
registerFunction('<=', (...args) => args.every((v, i, arr) => i === 0 || arr[i - 1] <= v));
registerFunction('>=', (...args) => args.every((v, i, arr) => i === 0 || arr[i - 1] >= v));

// String functions
registerFunction('STRING=', (a, b) => String(a) === String(b));
registerFunction('STRING-UPCASE', (s) => String(s).toUpperCase());
registerFunction('STRING-DOWNCASE', (s) => String(s).toLowerCase());

/**
 * Get imports object for WebAssembly instantiation
 * @returns {Object} Import object with clysm:runtime functions
 */
export function getImports() {
    return {
        'clysm:runtime': {
            '$dynamic-call': dynamicCall
        }
    };
}

/**
 * Merge runtime imports with other import objects
 * @param  {...Object} imports - Other import objects to merge
 * @returns {Object} Combined import object
 */
export function mergeImports(...imports) {
    const result = getImports();
    for (const importObj of imports) {
        for (const [module, funcs] of Object.entries(importObj)) {
            if (result[module]) {
                Object.assign(result[module], funcs);
            } else {
                result[module] = { ...funcs };
            }
        }
    }
    return result;
}

export default {
    registerFunction,
    registerFunctions,
    lookupFunction,
    hasFunction,
    getAllFunctionNames,
    dynamicCall,
    DynamicCallError,
    getImports,
    mergeImports
};
