#!/usr/bin/env node
/**
 * stage1-runner.js - Stage 1 Wasm Module Loader and Runner
 * Phase 13D-8: Stage 1 Runtime Environment (001-stage1-runtime)
 *
 * Runs the Stage 1 Clysm compiler with all required FFI shims.
 *
 * Usage:
 *   node stage1-runner.js [options] [wasm-path]
 *
 * Options:
 *   --help, -h       Show this help message
 *   --verbose, -v    Enable verbose logging
 *   --expr <expr>    Compile a Lisp expression (requires compile_form export)
 *   --output <path>  Output path for compiled Wasm (with --expr)
 *
 * Exit Codes:
 *   0  - Success
 *   1  - Partial success (some operations failed)
 *   2  - Failure (operation failed)
 *   3  - Missing dependency (required file not found)
 *   77 - Known limitation (export not available)
 */

import { readFile, access, constants } from 'node:fs/promises';
import * as path from 'node:path';
import { fileURLToPath } from 'node:url';
import { getImports as getIoImports, setStdinContent } from './io-shim.js';
import { getImports as getFsImports } from './fs-shim.js';

// Resolve __dirname for ES modules
const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

// Exit codes per data-model.md
const EXIT_SUCCESS = 0;
const EXIT_PARTIAL = 1;
const EXIT_FAILURE = 2;
const EXIT_MISSING_DEP = 3;
const EXIT_SKIP = 77;

// Default Stage 1 Wasm path
const DEFAULT_WASM_PATH = path.join(__dirname, '..', 'dist', 'clysm-stage1.wasm');

// Verbose logging flag
let verbose = false;

/**
 * Log message if verbose mode is enabled
 * @param {string} message - Message to log
 */
function logVerbose(message) {
    if (verbose) {
        console.error(`[VERBOSE] ${message}`);
    }
}

/**
 * Log error message
 * @param {string} message - Error message
 */
function logError(message) {
    console.error(`[ERROR] ${message}`);
}

/**
 * Log info message
 * @param {string} message - Info message
 */
function logInfo(message) {
    console.error(`[INFO] ${message}`);
}

/**
 * Log known limitation
 * @param {string} message - Limitation description
 */
function logLimitation(message) {
    console.error(`[KNOWN LIMITATION] ${message}`);
}

/**
 * Print usage information
 */
function printHelp() {
    console.log(`
Stage 1 Wasm Runner - Clysm Compiler Bootstrap

Usage:
  node stage1-runner.js [options] [wasm-path]

Arguments:
  wasm-path          Path to Stage 1 Wasm module (default: dist/clysm-stage1.wasm)

Options:
  --help, -h         Show this help message
  --verbose, -v      Enable verbose logging
  --expr <expr>      Compile a Lisp expression (requires compile_form export)
  --output <path>    Output path for compiled Wasm (with --expr)

Exit Codes:
  0  - Success (_start executed without errors)
  1  - Partial success (some operations failed)
  2  - Failure (Wasm trap or execution error)
  3  - Missing dependency (Wasm file not found)
  77 - Known limitation (required export not available)

Examples:
  node stage1-runner.js                           # Run Stage 1 with defaults
  node stage1-runner.js --verbose                 # Run with verbose output
  node stage1-runner.js ./custom-stage1.wasm      # Run custom Wasm module
  node stage1-runner.js --expr "(+ 1 2)"          # Compile expression
`.trim());
}

/**
 * Parse command line arguments
 * @param {string[]} args - Command line arguments
 * @returns {Object} Parsed options
 */
function parseArgs(args) {
    const options = {
        wasmPath: DEFAULT_WASM_PATH,
        help: false,
        verbose: false,
        expr: null,
        output: null
    };

    for (let i = 0; i < args.length; i++) {
        const arg = args[i];

        switch (arg) {
            case '--help':
            case '-h':
                options.help = true;
                break;

            case '--verbose':
            case '-v':
                options.verbose = true;
                break;

            case '--expr':
                if (i + 1 >= args.length) {
                    logError('--expr requires an argument');
                    process.exit(EXIT_FAILURE);
                }
                options.expr = args[++i];
                break;

            case '--output':
                if (i + 1 >= args.length) {
                    logError('--output requires an argument');
                    process.exit(EXIT_FAILURE);
                }
                options.output = args[++i];
                break;

            default:
                if (arg.startsWith('-')) {
                    logError(`Unknown option: ${arg}`);
                    process.exit(EXIT_FAILURE);
                }
                // Positional argument - treat as wasm path
                options.wasmPath = arg;
                break;
        }
    }

    return options;
}

/**
 * Merge all FFI imports into a single imports object
 * @returns {Object} Combined import object for WebAssembly instantiation
 */
function getAllImports() {
    const ioImports = getIoImports();
    const fsImports = getFsImports();

    logVerbose('FFI imports loaded:');
    logVerbose(`  clysm:io: ${Object.keys(ioImports['clysm:io']).join(', ')}`);
    logVerbose(`  clysm:fs: ${Object.keys(fsImports['clysm:fs']).join(', ')}`);

    return {
        ...ioImports,
        ...fsImports
    };
}

/**
 * Check if a file exists and is readable
 * @param {string} filepath - Path to check
 * @returns {Promise<boolean>} True if file exists and is readable
 */
async function fileExists(filepath) {
    try {
        await access(filepath, constants.R_OK);
        return true;
    } catch {
        return false;
    }
}

/**
 * Load and compile a WebAssembly module
 * @param {string} wasmPath - Path to Wasm file
 * @returns {Promise<WebAssembly.Module>} Compiled WebAssembly module
 */
async function loadWasmModule(wasmPath) {
    logVerbose(`Loading Wasm module: ${wasmPath}`);

    if (!await fileExists(wasmPath)) {
        logError(`Stage 1 binary not found: ${wasmPath}`);
        process.exit(EXIT_MISSING_DEP);
    }

    const wasmBuffer = await readFile(wasmPath);
    logVerbose(`Read ${wasmBuffer.length} bytes`);

    // Validate Wasm magic bytes
    if (wasmBuffer.length < 8) {
        logError('Invalid Wasm file: too short');
        process.exit(EXIT_FAILURE);
    }

    const magic = wasmBuffer.slice(0, 4);
    if (magic[0] !== 0x00 || magic[1] !== 0x61 || magic[2] !== 0x73 || magic[3] !== 0x6d) {
        logError('Invalid Wasm file: bad magic bytes');
        process.exit(EXIT_FAILURE);
    }

    logVerbose('Wasm magic bytes validated');

    return WebAssembly.compile(wasmBuffer);
}

/**
 * Instantiate a WebAssembly module with FFI imports
 * @param {WebAssembly.Module} module - Compiled WebAssembly module
 * @returns {Promise<WebAssembly.Instance>} Instantiated module
 */
async function instantiateModule(module) {
    const imports = getAllImports();

    logVerbose('Instantiating WebAssembly module...');

    try {
        const instance = await WebAssembly.instantiate(module, imports);
        logVerbose('Module instantiated successfully');
        return instance;
    } catch (error) {
        if (error instanceof WebAssembly.LinkError) {
            logError(`FFI import mismatch: ${error.message}`);
            logError('Ensure all required host functions are provided');
        } else {
            logError(`Instantiation failed: ${error.message}`);
        }
        process.exit(EXIT_FAILURE);
    }
}

/**
 * Discover and log module exports
 * @param {WebAssembly.Instance} instance - WebAssembly instance
 * @returns {Object} Export information
 */
function discoverExports(instance) {
    const exports = Object.keys(instance.exports);
    logVerbose(`Module exports: ${exports.join(', ')}`);

    const info = {
        hasStart: '_start' in instance.exports,
        hasCompileForm: 'compile_form' in instance.exports,
        hasCompileAll: 'compile_all' in instance.exports,
        exports
    };

    return info;
}

/**
 * Invoke _start function with error handling
 * @param {WebAssembly.Instance} instance - WebAssembly instance
 */
function invokeStart(instance) {
    logVerbose('Invoking _start...');

    try {
        instance.exports._start();
        logVerbose('_start completed successfully');
    } catch (error) {
        if (error instanceof WebAssembly.RuntimeError) {
            logError(`Wasm trap: ${error.message}`);
            if (error.stack) {
                console.error('\nStack trace:');
                console.error(error.stack);
            }
        } else {
            logError(`Runtime error: ${error.message}`);
        }
        process.exit(EXIT_FAILURE);
    }
}

/**
 * Compile a Lisp expression using compile_form export
 * @param {WebAssembly.Instance} instance - WebAssembly instance
 * @param {string} expr - Lisp expression to compile
 * @param {Object} exportInfo - Export information
 * @returns {Uint8Array|null} Compiled Wasm bytes or null
 */
function compileExpression(instance, expr, exportInfo) {
    if (!exportInfo.hasCompileForm) {
        logLimitation('Stage 1 does not export compile_form.');
        logInfo('Current compilation rate is ~14.1%.');
        logInfo('compile_form may not be available until compilation rate improves.');
        return null;
    }

    logVerbose(`Compiling expression: ${expr}`);

    try {
        const result = instance.exports.compile_form(expr);
        logVerbose('compile_form returned successfully');
        return result;
    } catch (error) {
        logError(`Compilation failed: ${error.message}`);
        return null;
    }
}

/**
 * Main entry point
 */
async function main() {
    const args = process.argv.slice(2);
    const options = parseArgs(args);

    // Handle --help
    if (options.help) {
        printHelp();
        process.exit(EXIT_SUCCESS);
    }

    // Set verbose flag
    verbose = options.verbose;

    logVerbose('Stage 1 Runner starting...');
    logVerbose(`Node.js ${process.version}`);

    // Load and compile Wasm module
    const module = await loadWasmModule(options.wasmPath);

    // Instantiate with FFI imports
    const instance = await instantiateModule(module);

    // Discover exports
    const exportInfo = discoverExports(instance);

    // Handle --expr mode
    if (options.expr !== null) {
        const result = compileExpression(instance, options.expr, exportInfo);

        if (result === null) {
            // compile_form not available or failed
            process.exit(EXIT_SKIP);
        }

        // Write output if path specified
        if (options.output) {
            const { writeFile } = await import('node:fs/promises');
            await writeFile(options.output, result);
            logInfo(`Compiled output written to: ${options.output}`);
        } else {
            // Output to stdout as hex (for debugging)
            console.log(Buffer.from(result).toString('hex'));
        }

        process.exit(EXIT_SUCCESS);
    }

    // Default mode: invoke _start
    if (!exportInfo.hasStart) {
        logError('Stage 1 does not export _start function');
        process.exit(EXIT_FAILURE);
    }

    invokeStart(instance);
    process.exit(EXIT_SUCCESS);
}

// Run main
main().catch(error => {
    logError(`Unexpected error: ${error.message}`);
    if (error.stack) {
        console.error(error.stack);
    }
    process.exit(EXIT_FAILURE);
});
