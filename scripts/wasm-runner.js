#!/usr/bin/env node
// wasm-runner.js - Execute WASM modules and return results
// Usage: node wasm-runner.js <wasm-file> [function-name]

const fs = require('fs');
const path = require('path');

async function runWasm(wasmPath, funcName = 'test-main') {
    try {
        const wasmBuffer = fs.readFileSync(wasmPath);
        const wasmModule = await WebAssembly.compile(wasmBuffer);

        // Setup imports for memory (if needed)
        const memory = new WebAssembly.Memory({ initial: 16, maximum: 256 });

        const imports = {
            env: {
                memory: memory
            }
        };

        const instance = await WebAssembly.instantiate(wasmModule, imports);

        // Get the exported function
        const exportedFunc = instance.exports[funcName];
        if (!exportedFunc) {
            // Try without hyphen (WASM export name mangling)
            const altName = funcName.replace(/-/g, '_');
            const altFunc = instance.exports[altName];
            if (!altFunc) {
                console.log(JSON.stringify({
                    status: 'error',
                    error: `Function '${funcName}' not found in exports. Available: ${Object.keys(instance.exports).join(', ')}`
                }));
                process.exit(1);
            }
        }

        // Call the function
        const func = instance.exports[funcName] || instance.exports[funcName.replace(/-/g, '_')];
        const result = func();

        // Determine result type and format
        let resultType = 'i32';
        let resultValue = result;

        if (typeof result === 'bigint') {
            resultType = 'i64';
            resultValue = result.toString();
        } else if (typeof result === 'number') {
            if (Number.isInteger(result)) {
                resultType = 'i32';
            } else {
                resultType = 'f64';
            }
        }

        console.log(JSON.stringify({
            status: 'success',
            value: resultValue,
            type: resultType,
            // Include memory if needed for heap inspection
            memorySize: instance.exports.memory ? instance.exports.memory.buffer.byteLength : 0
        }));

    } catch (err) {
        console.log(JSON.stringify({
            status: 'error',
            error: err.message,
            stack: err.stack
        }));
        process.exit(1);
    }
}

// Parse command line arguments
const args = process.argv.slice(2);

if (args.length < 1) {
    console.error('Usage: node wasm-runner.js <wasm-file> [function-name]');
    console.error('');
    console.error('Options:');
    console.error('  wasm-file      Path to the WASM binary file');
    console.error('  function-name  Name of the function to call (default: test-main)');
    process.exit(1);
}

const wasmPath = args[0];
const funcName = args[1] || 'test-main';

if (!fs.existsSync(wasmPath)) {
    console.log(JSON.stringify({
        status: 'error',
        error: `File not found: ${wasmPath}`
    }));
    process.exit(1);
}

runWasm(wasmPath, funcName);
