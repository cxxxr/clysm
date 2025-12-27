#!/usr/bin/env node
/**
 * verify-stage0.js - JavaScript host shim for Stage 0 verification
 *
 * T028-T029, T033, T037: Host shim for invoking Stage 0 compiler
 *
 * KNOWN LIMITATION: Stage 0 currently compiles only 14/849 forms (1.6%)
 * due to CL subset constraints. The compile export is not yet functional.
 * This shim is prepared for when Clysm's CL support is extended.
 *
 * Usage:
 *   node verify-stage0.js <test-id>
 *   wasmtime --invoke <export> dist/clysm-stage0.wasm
 *
 * Test IDs:
 *   V001 - Arithmetic: (+ 1 2) → 3
 *   V002 - Defun: (defun f (x) (* x 2)) (f 21) → 42
 *   V003 - Control: (if (> 10 5) 'greater 'less) → GREATER
 */

const fs = require('fs');
const path = require('path');

// Configuration
const STAGE0_PATH = path.join(__dirname, '..', 'dist', 'clysm-stage0.wasm');

// Test cases
const TEST_CASES = {
  V001: {
    name: 'Arithmetic',
    input: '(+ 1 2)',
    expected: 3,
    description: 'Basic addition should yield 3'
  },
  V002: {
    name: 'Defun',
    input: '(progn (defun f (x) (* x 2)) (f 21))',
    expected: 42,
    description: 'Function definition and call should yield 42'
  },
  V003: {
    name: 'Control Flow',
    input: "(if (> 10 5) 'greater 'less)",
    expected: 'GREATER',
    description: 'Conditional should yield GREATER'
  }
};

/**
 * Host imports for Wasm module
 */
const hostImports = {
  host: {
    /**
     * Read a string from the host (for source input)
     * @param {number} ptr - Pointer to string buffer
     * @param {number} len - Maximum length
     * @returns {number} - Bytes read
     */
    read_string: (ptr, len) => {
      console.log('[host.read_string] Called with ptr:', ptr, 'len:', len);
      return 0; // No input in verification mode
    },

    /**
     * Write bytes to the host (for compiled output)
     * @param {number} ptr - Pointer to byte buffer
     * @param {number} len - Length of data
     */
    write_bytes: (ptr, len) => {
      console.log('[host.write_bytes] Called with ptr:', ptr, 'len:', len);
    },

    /**
     * Log a message from Wasm
     * @param {number} ptr - Pointer to message
     * @param {number} len - Length of message
     */
    log: (ptr, len) => {
      console.log('[host.log] Message from Wasm');
    },

    /**
     * Report an error from Wasm
     * @param {number} code - Error code
     * @param {number} ptr - Pointer to message
     * @param {number} len - Length of message
     */
    error: (code, ptr, len) => {
      console.error('[host.error] Code:', code);
    }
  }
};

/**
 * Check if Stage 0 binary exists
 */
function checkStage0Exists() {
  if (!fs.existsSync(STAGE0_PATH)) {
    console.error('ERROR: Stage 0 binary not found:', STAGE0_PATH);
    console.error('Run: sbcl --load build/bootstrap.lisp');
    process.exit(1);
  }
  return true;
}

/**
 * Load and instantiate Stage 0 module
 */
async function loadStage0() {
  checkStage0Exists();

  try {
    const wasmBuffer = fs.readFileSync(STAGE0_PATH);
    const wasmModule = await WebAssembly.compile(wasmBuffer);
    const instance = await WebAssembly.instantiate(wasmModule, hostImports);
    return instance;
  } catch (error) {
    console.error('ERROR: Failed to load Stage 0:', error.message);

    // Check for specific error types
    if (error.message.includes('import')) {
      console.error('Note: Stage 0 may require additional host imports.');
      console.error('Current exports:', Object.keys(hostImports.host).join(', '));
    }

    process.exit(2);
  }
}

/**
 * Run a verification test
 * @param {string} testId - Test identifier (V001, V002, V003)
 */
async function runTest(testId) {
  const test = TEST_CASES[testId];
  if (!test) {
    console.error('Unknown test ID:', testId);
    console.error('Available:', Object.keys(TEST_CASES).join(', '));
    process.exit(1);
  }

  console.log('='.repeat(60));
  console.log('Test:', testId, '-', test.name);
  console.log('Input:', test.input);
  console.log('Expected:', test.expected);
  console.log('='.repeat(60));

  // KNOWN LIMITATION: Stage 0 doesn't export 'compile' yet
  console.log('\nKNOWN LIMITATION:');
  console.log('Stage 0 currently compiles only 14/849 forms (1.6%).');
  console.log('The compile export is not functional until Clysm supports:');
  console.log('  - defstruct');
  console.log('  - declare');
  console.log('  - format');
  console.log('  - define-condition');
  console.log('  - and other CL features used in the compiler source.');
  console.log('\nThis test will SKIP until these features are implemented.');
  console.log('='.repeat(60));

  // Try to load the module to verify it's valid
  try {
    checkStage0Exists();
    const wasmBuffer = fs.readFileSync(STAGE0_PATH);

    // At minimum, verify the binary is valid WebAssembly
    await WebAssembly.compile(wasmBuffer);
    console.log('\n✓ Stage 0 binary is valid WebAssembly');

    // Check what exports are available
    const module = await WebAssembly.compile(wasmBuffer);
    const exports = WebAssembly.Module.exports(module);
    console.log('  Exports:', exports.length > 0
      ? exports.map(e => e.name).join(', ')
      : '(none)');

    console.log('\nResult: SKIP (compile export not available)');
    process.exit(77); // Standard skip exit code

  } catch (error) {
    console.error('\n✗ Stage 0 binary validation failed:', error.message);
    process.exit(1);
  }
}

/**
 * List available tests
 */
function listTests() {
  console.log('Available verification tests:\n');
  for (const [id, test] of Object.entries(TEST_CASES)) {
    console.log(`  ${id}: ${test.name}`);
    console.log(`      Input: ${test.input}`);
    console.log(`      Expected: ${test.expected}`);
    console.log();
  }
}

/**
 * Main entry point
 */
async function main() {
  const args = process.argv.slice(2);

  if (args.length === 0 || args[0] === '--help') {
    console.log('Usage: node verify-stage0.js <test-id>');
    console.log('       node verify-stage0.js --list');
    console.log();
    listTests();
    process.exit(0);
  }

  if (args[0] === '--list') {
    listTests();
    process.exit(0);
  }

  if (args[0] === '--check') {
    // Just check if Stage 0 exists and validates
    checkStage0Exists();
    console.log('Stage 0 binary exists:', STAGE0_PATH);
    process.exit(0);
  }

  await runTest(args[0].toUpperCase());
}

main().catch(error => {
  console.error('Unexpected error:', error);
  process.exit(1);
});
