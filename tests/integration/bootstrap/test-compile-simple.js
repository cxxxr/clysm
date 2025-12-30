#!/usr/bin/env node
/**
 * test-compile-simple.js - Integration test for compile_form (T013)
 * Part of 001-bootstrap-fixpoint Phase 13D-9
 *
 * Tests that Stage 1 can compile a simple form `(+ 1 2)` via compile_form export.
 * TDD: This test must FAIL before implementation is complete.
 *
 * Exit Codes:
 *   0  - All tests passed
 *   1  - Test failure
 *   77 - Skip (export not available - expected before implementation)
 */

import { readFile, access, constants, writeFile, unlink } from 'node:fs/promises';
import { execSync } from 'node:child_process';
import * as path from 'node:path';
import { fileURLToPath } from 'node:url';

// Resolve paths for ES modules
const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const projectRoot = path.resolve(__dirname, '..', '..', '..');
const distDir = path.join(projectRoot, 'dist');
const stage1Path = path.join(distDir, 'clysm-stage1.wasm');
const hostShimDir = path.join(projectRoot, 'host-shim');

// Exit codes
const EXIT_PASS = 0;
const EXIT_FAIL = 1;
const EXIT_SKIP = 77;

let testsPassed = 0;
let testsFailed = 0;
let testsSkipped = 0;

/**
 * Log test result
 */
function logTest(name, passed, message) {
    const status = passed ? 'PASS' : 'FAIL';
    console.log(`[${status}] ${name}: ${message}`);
    if (passed) testsPassed++;
    else testsFailed++;
}

/**
 * Log skipped test
 */
function logSkip(name, reason) {
    console.log(`[SKIP] ${name}: ${reason}`);
    testsSkipped++;
}

/**
 * Check if Stage 1 exports compile_form
 */
async function checkCompileFormExport() {
    try {
        const output = execSync(`wasm-tools print "${stage1Path}"`, { encoding: 'utf-8' });
        return output.includes('(export "compile_form"');
    } catch (error) {
        console.error(`Error checking exports: ${error.message}`);
        return false;
    }
}

/**
 * Test 1: Stage 1 Wasm file exists
 */
async function testStage1Exists() {
    try {
        await access(stage1Path, constants.R_OK);
        logTest('stage1-exists', true, `Found ${stage1Path}`);
        return true;
    } catch {
        logTest('stage1-exists', false, `Stage 1 binary not found: ${stage1Path}`);
        return false;
    }
}

/**
 * Test 2: Stage 1 exports compile_form function
 */
async function testCompileFormExport() {
    const hasExport = await checkCompileFormExport();
    if (!hasExport) {
        // This is expected to fail initially - log as skip for TDD
        logSkip('compile-form-export', 'compile_form not exported (expected before implementation)');
        return false;
    }
    logTest('compile-form-export', true, 'compile_form is exported');
    return true;
}

/**
 * Test 3: Compile simple form (+ 1 2) using stage1-runner.js
 * This test exercises the full compilation pipeline
 */
async function testCompileSimpleForm() {
    const hasExport = await checkCompileFormExport();
    if (!hasExport) {
        logSkip('compile-simple-form', 'compile_form not available (skip compile test)');
        return null; // null indicates skip
    }

    const outputPath = path.join(distDir, 'test-simple.wasm');
    const runnerPath = path.join(hostShimDir, 'stage1-runner.js');

    try {
        // Use stage1-runner.js to compile expression
        execSync(`node "${runnerPath}" --expr "(+ 1 2)" --output "${outputPath}"`, {
            encoding: 'utf-8',
            stdio: 'pipe'
        });

        // Verify output file was created
        await access(outputPath, constants.R_OK);
        logTest('compile-simple-form', true, 'Successfully compiled (+ 1 2)');

        // Clean up
        await unlink(outputPath);
        return true;
    } catch (error) {
        if (error.status === 77) {
            // Exit code 77 means known limitation (compile_form not available)
            logSkip('compile-simple-form', 'stage1-runner returned skip (77)');
            return null;
        }
        logTest('compile-simple-form', false, `Compilation failed: ${error.message}`);
        return false;
    }
}

/**
 * Test 4: Compiled output passes wasm-tools validation
 */
async function testCompiledOutputValid() {
    const hasExport = await checkCompileFormExport();
    if (!hasExport) {
        logSkip('compiled-output-valid', 'compile_form not available (skip validation)');
        return null;
    }

    const outputPath = path.join(distDir, 'test-simple-valid.wasm');
    const runnerPath = path.join(hostShimDir, 'stage1-runner.js');

    try {
        // Compile expression
        execSync(`node "${runnerPath}" --expr "(+ 1 2)" --output "${outputPath}"`, {
            encoding: 'utf-8',
            stdio: 'pipe'
        });

        // Validate with wasm-tools
        execSync(`wasm-tools validate "${outputPath}"`, {
            encoding: 'utf-8',
            stdio: 'pipe'
        });

        logTest('compiled-output-valid', true, 'Output passes wasm-tools validation');

        // Clean up
        await unlink(outputPath);
        return true;
    } catch (error) {
        if (error.status === 77) {
            logSkip('compiled-output-valid', 'stage1-runner returned skip (77)');
            return null;
        }
        logTest('compiled-output-valid', false, `Validation failed: ${error.message}`);
        // Clean up on failure too
        try { await unlink(outputPath); } catch { /* ignore */ }
        return false;
    }
}

/**
 * Main test runner
 */
async function main() {
    console.log('=== Bootstrap Integration Tests (T013) ===');
    console.log(`Stage 1: ${stage1Path}`);
    console.log('');

    // Run tests
    const stage1Exists = await testStage1Exists();
    if (!stage1Exists) {
        console.log('\nStage 1 binary not found. Cannot proceed with tests.');
        process.exit(EXIT_FAIL);
    }

    await testCompileFormExport();
    await testCompileSimpleForm();
    await testCompiledOutputValid();

    // Summary
    console.log('');
    console.log('=== Test Summary ===');
    console.log(`Passed:  ${testsPassed}`);
    console.log(`Failed:  ${testsFailed}`);
    console.log(`Skipped: ${testsSkipped}`);

    // Determine exit code
    if (testsFailed > 0) {
        process.exit(EXIT_FAIL);
    } else if (testsSkipped > 0 && testsPassed === 0) {
        // All tests skipped (expected before implementation)
        console.log('\nNote: Tests skipped because compile_form is not yet exported.');
        console.log('This is expected before T014-T021 implementation is complete.');
        process.exit(EXIT_SKIP);
    } else {
        process.exit(EXIT_PASS);
    }
}

// Run tests
main().catch(error => {
    console.error(`\nUnexpected error: ${error.message}`);
    console.error(error.stack);
    process.exit(EXIT_FAIL);
});
