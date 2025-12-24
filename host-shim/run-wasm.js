#!/usr/bin/env node
/**
 * run-wasm.js - Module loader and runner script (T023)
 * FFI-based stream I/O (015-ffi-stream-io)
 *
 * Runs Clysm-compiled WebAssembly modules with host I/O shim.
 *
 * Usage:
 *   node run-wasm.js <path-to-wasm> [--stdin <file>]
 */

import { readFile } from 'node:fs/promises';
import { getImports, setStdinContent } from './io-shim.js';

async function main() {
    const args = process.argv.slice(2);

    if (args.length === 0) {
        console.error('Usage: node run-wasm.js <path-to-wasm> [--stdin <file>]');
        process.exit(1);
    }

    const wasmPath = args[0];
    let stdinFile = null;

    // Parse arguments
    for (let i = 1; i < args.length; i++) {
        if (args[i] === '--stdin' && args[i + 1]) {
            stdinFile = args[i + 1];
            i++;
        }
    }

    try {
        // Load stdin content if provided
        if (stdinFile) {
            const stdinContent = await readFile(stdinFile, 'utf-8');
            setStdinContent(stdinContent);
        }

        // Load and compile WebAssembly module
        const wasmBuffer = await readFile(wasmPath);
        const imports = getImports();

        // Instantiate with WasmGC support
        const { instance } = await WebAssembly.instantiate(wasmBuffer, imports);

        // Run the start function or main export
        if (instance.exports._start) {
            instance.exports._start();
        } else if (instance.exports.main) {
            const result = instance.exports.main();
            if (result !== undefined) {
                console.log(`Result: ${result}`);
            }
        } else {
            console.error('No _start or main function found in module');
            process.exit(1);
        }

    } catch (error) {
        console.error(`Error: ${error.message}`);
        if (error.stack) {
            console.error(error.stack);
        }
        process.exit(1);
    }
}

main();
