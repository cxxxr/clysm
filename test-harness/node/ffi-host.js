/**
 * ffi-host.js - JavaScript FFI test harness for Node.js (T026, T065)
 *
 * Provides sample host functions for testing FFI interoperability.
 * This module is loaded alongside compiled Wasm modules to provide
 * the imported functions.
 *
 * T065: Enhanced with complete marshal type support:
 *   - :fixnum  -> i31ref <-> i32 (sign-extended)
 *   - :float   -> (ref $float) <-> f64
 *   - :string  -> (ref $string) <-> externref
 *   - :boolean -> anyref <-> i32 (0/1)
 *   - :anyref  -> anyref <-> anyref (passthrough)
 */

'use strict';

/**
 * Host functions that can be imported by Lisp code.
 *
 * Usage in Lisp:
 *   (ffi:define-foreign-function host-log "host.log" (:string) :void)
 *   (ffi:define-foreign-function host-add "host.add" (:fixnum :fixnum) :fixnum)
 */
const hostFunctions = {
    /**
     * Log a message to console
     * @param {string} message - Message to log
     */
    log: (message) => {
        console.log('[Lisp]', message);
    },

    /**
     * Add two integers
     * @param {number} a - First operand
     * @param {number} b - Second operand
     * @returns {number} Sum of a and b
     */
    add: (a, b) => {
        return a + b;
    },

    /**
     * Subtract two integers
     * @param {number} a - First operand
     * @param {number} b - Second operand
     * @returns {number} Difference (a - b)
     */
    sub: (a, b) => {
        return a - b;
    },

    /**
     * Multiply two integers
     * @param {number} a - First operand
     * @param {number} b - Second operand
     * @returns {number} Product of a and b
     */
    mul: (a, b) => {
        return a * b;
    },

    /**
     * Get current timestamp in milliseconds
     * @returns {number} Current time in milliseconds since epoch
     */
    now: () => {
        return Date.now();
    },

    /**
     * Generate a random float between 0 and 1
     * @returns {number} Random float
     */
    random: () => {
        return Math.random();
    },

    /**
     * Test boolean conversion: returns true if input > 0
     * @param {number} value - Value to test
     * @returns {number} 1 if true, 0 if false
     */
    isPositive: (value) => {
        return value > 0 ? 1 : 0;
    },

    /**
     * String length (for string marshalling tests)
     * @param {string} s - Input string
     * @returns {number} Length of string
     */
    strlen: (s) => {
        return s ? s.length : 0;
    },

    /**
     * Concatenate two strings
     * @param {string} a - First string
     * @param {string} b - Second string
     * @returns {string} Concatenated string
     */
    strcat: (a, b) => {
        return (a || '') + (b || '');
    }
};

/**
 * Environment functions (env.* namespace)
 */
const envFunctions = {
    /**
     * Print to stdout (like console.log but without prefix)
     * @param {string} message - Message to print
     */
    print: (message) => {
        process.stdout.write(String(message) + '\n');
    },

    /**
     * Exit process with code
     * @param {number} code - Exit code
     */
    exit: (code) => {
        process.exit(code);
    },

    /**
     * Get environment variable
     * @param {string} name - Variable name
     * @returns {string} Value or empty string
     */
    getenv: (name) => {
        return process.env[name] || '';
    }
};

/**
 * Create import object for WebAssembly instantiation
 *
 * @returns {Object} Import object with host and env namespaces
 */
function createImportObject() {
    return {
        host: hostFunctions,
        env: envFunctions
    };
}

/**
 * Load and instantiate a Wasm module with FFI support
 *
 * @param {string|Buffer|ArrayBuffer} wasmSource - Path to .wasm file or bytes
 * @returns {Promise<WebAssembly.Instance>} Instantiated module
 */
async function loadWasmModule(wasmSource) {
    let wasmBytes;

    if (typeof wasmSource === 'string') {
        // Path to file
        const fs = require('fs').promises;
        wasmBytes = await fs.readFile(wasmSource);
    } else if (wasmSource instanceof Buffer) {
        wasmBytes = wasmSource;
    } else {
        wasmBytes = new Uint8Array(wasmSource);
    }

    const importObject = createImportObject();
    const { instance } = await WebAssembly.instantiate(wasmBytes, importObject);
    return instance;
}

/**
 * Run the _start export of a Wasm module
 *
 * @param {WebAssembly.Instance} instance - Instantiated module
 * @returns {*} Return value from _start
 */
function runModule(instance) {
    if (instance.exports._start) {
        return instance.exports._start();
    }
    throw new Error('Module has no _start export');
}

/**
 * T036: Call a Lisp exported function from host
 *
 * This helper function calls an exported Lisp function by name.
 * Lisp functions are exported via ffi:export-function.
 *
 * @param {WebAssembly.Instance} instance - Instantiated module
 * @param {string} name - Name of the exported function
 * @param {...*} args - Arguments to pass to the function
 * @returns {*} Return value from the function
 *
 * Example:
 *   // In Lisp:
 *   // (defun my-add (a b) (+ a b))
 *   // (ffi:export-function my-add :as "add" :signature ((:fixnum :fixnum) :fixnum))
 *
 *   // In JavaScript:
 *   const result = callExport(instance, 'add', 10, 20);
 *   console.log(result); // 30
 */
function callExport(instance, name, ...args) {
    const fn = instance.exports[name];
    if (!fn) {
        throw new Error(`Export '${name}' not found in module. Available exports: ${Object.keys(instance.exports).join(', ')}`);
    }
    if (typeof fn !== 'function') {
        throw new Error(`Export '${name}' is not a function (got ${typeof fn})`);
    }
    return fn(...args);
}

/**
 * Get all exported function names from a module
 *
 * @param {WebAssembly.Instance} instance - Instantiated module
 * @returns {string[]} Array of export names
 */
function getExportNames(instance) {
    return Object.keys(instance.exports).filter(
        name => typeof instance.exports[name] === 'function'
    );
}

/**
 * Test helper: verify an exported function exists and is callable
 *
 * @param {WebAssembly.Instance} instance - Instantiated module
 * @param {string} name - Name of the export to verify
 * @returns {boolean} True if export exists and is a function
 */
function hasExport(instance, name) {
    return typeof instance.exports[name] === 'function';
}

/**
 * T065: Marshal type conversion utilities
 *
 * These helpers convert between JavaScript and Wasm types according
 * to the FFI marshalling conventions.
 */
const marshalTypes = {
    /**
     * Convert i31ref (Wasm) to fixnum (JS number)
     * i31ref values are already sign-extended i32 in JS
     */
    i31refToFixnum: (value) => {
        // Sign-extend from 31 bits if needed
        if (value & 0x40000000) {
            return value | ~0x7FFFFFFF;
        }
        return value;
    },

    /**
     * Convert fixnum (JS number) to i31ref (Wasm)
     * Only the low 31 bits are used
     */
    fixnumToI31ref: (value) => {
        return value & 0x7FFFFFFF;
    },

    /**
     * Convert boolean to i32 (0 or 1)
     */
    boolToI32: (value) => {
        return value ? 1 : 0;
    },

    /**
     * Convert i32 to boolean
     */
    i32ToBool: (value) => {
        return value !== 0;
    },

    /**
     * Test if a value is a valid fixnum (fits in 31 bits signed)
     */
    isFixnum: (value) => {
        return Number.isInteger(value) && value >= -1073741824 && value <= 1073741823;
    }
};

/**
 * T065: Test runner for FFI integration tests
 *
 * @param {string} wasmPath - Path to the Wasm file
 * @param {Object} tests - Object mapping test names to test functions
 */
async function runTests(wasmPath, tests) {
    console.log(`\nRunning FFI tests for: ${wasmPath}`);
    console.log('='.repeat(50));

    let passed = 0;
    let failed = 0;

    try {
        const instance = await loadWasmModule(wasmPath);

        for (const [name, testFn] of Object.entries(tests)) {
            try {
                await testFn(instance);
                console.log(`  ✓ ${name}`);
                passed++;
            } catch (error) {
                console.log(`  ✗ ${name}: ${error.message}`);
                failed++;
            }
        }
    } catch (error) {
        console.log(`  ✗ Failed to load module: ${error.message}`);
        failed = Object.keys(tests).length;
    }

    console.log('='.repeat(50));
    console.log(`Results: ${passed} passed, ${failed} failed`);
    return failed === 0;
}

// Export for use in tests
module.exports = {
    hostFunctions,
    envFunctions,
    createImportObject,
    loadWasmModule,
    runModule,
    // T036: Export helpers for calling Lisp exports
    callExport,
    getExportNames,
    hasExport,
    // T065: Marshal type utilities and test runner
    marshalTypes,
    runTests
};

// If run directly, provide usage info
if (require.main === module) {
    console.log('FFI Test Harness');
    console.log('================');
    console.log('');
    console.log('Available host functions:');
    console.log('  host.log(message)      - Log message to console');
    console.log('  host.add(a, b)         - Add two integers');
    console.log('  host.sub(a, b)         - Subtract two integers');
    console.log('  host.mul(a, b)         - Multiply two integers');
    console.log('  host.now()             - Get current timestamp');
    console.log('  host.random()          - Get random float [0, 1)');
    console.log('  host.isPositive(n)     - Test if n > 0');
    console.log('  host.strlen(s)         - Get string length');
    console.log('  host.strcat(a, b)      - Concatenate strings');
    console.log('');
    console.log('Available env functions:');
    console.log('  env.print(message)     - Print to stdout');
    console.log('  env.exit(code)         - Exit with code');
    console.log('  env.getenv(name)       - Get environment variable');
    console.log('');
    console.log('Usage (importing from host):');
    console.log('  const { loadWasmModule, runModule } = require("./ffi-host");');
    console.log('  const instance = await loadWasmModule("program.wasm");');
    console.log('  const result = runModule(instance);');
    console.log('');
    console.log('Usage (calling Lisp exports):');
    console.log('  const { loadWasmModule, callExport } = require("./ffi-host");');
    console.log('  const instance = await loadWasmModule("program.wasm");');
    console.log('  const result = callExport(instance, "add", 10, 20);');
    console.log('  console.log(result); // 30');
}
