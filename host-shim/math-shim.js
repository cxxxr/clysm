/**
 * math-shim.js - Host math function implementations
 * ANSI Numeric Functions Extension (001-numeric-functions)
 *
 * Provides transcendental math functions for Clysm-compiled WebAssembly modules.
 * Imports under the "clysm:math" module namespace.
 *
 * Research decision: Import from host via FFI is ~10x faster than
 * implementing Taylor series or Chebyshev polynomials in Wasm.
 */

// Mathematical constants
export const PI = Math.PI;
export const E = Math.E;

// Trigonometric functions (radians)
export function sin(x) {
    return Math.sin(x);
}

export function cos(x) {
    return Math.cos(x);
}

export function tan(x) {
    return Math.tan(x);
}

// Inverse trigonometric functions
export function asin(x) {
    return Math.asin(x);
}

export function acos(x) {
    return Math.acos(x);
}

export function atan(x) {
    return Math.atan(x);
}

export function atan2(y, x) {
    return Math.atan2(y, x);
}

// Hyperbolic functions
export function sinh(x) {
    return Math.sinh(x);
}

export function cosh(x) {
    return Math.cosh(x);
}

export function tanh(x) {
    return Math.tanh(x);
}

// Inverse hyperbolic functions
export function asinh(x) {
    return Math.asinh(x);
}

export function acosh(x) {
    return Math.acosh(x);
}

export function atanh(x) {
    return Math.atanh(x);
}

// Exponential and logarithmic functions
export function exp(x) {
    return Math.exp(x);
}

export function log(x) {
    return Math.log(x);
}

export function log10(x) {
    return Math.log10(x);
}

// Power functions
export function pow(base, exponent) {
    return Math.pow(base, exponent);
}

export function sqrt(x) {
    return Math.sqrt(x);
}

/**
 * Get imports object for WebAssembly instantiation
 * @returns {Object} Import object with clysm:math functions
 */
export function getImports() {
    return {
        'clysm:math': {
            'sin': sin,
            'cos': cos,
            'tan': tan,
            'asin': asin,
            'acos': acos,
            'atan': atan,
            'atan2': atan2,
            'sinh': sinh,
            'cosh': cosh,
            'tanh': tanh,
            'asinh': asinh,
            'acosh': acosh,
            'atanh': atanh,
            'exp': exp,
            'log': log,
            'log10': log10,
            'pow': pow,
            'sqrt': sqrt,
            'PI': PI,
            'E': E
        }
    };
}

export default {
    sin,
    cos,
    tan,
    asin,
    acos,
    atan,
    atan2,
    sinh,
    cosh,
    tanh,
    asinh,
    acosh,
    atanh,
    exp,
    log,
    log10,
    pow,
    sqrt,
    PI,
    E,
    getImports
};
