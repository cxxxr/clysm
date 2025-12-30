/**
 * test-fixpoint-exit.js - Contract test for fixpoint verification exit codes (T041)
 *
 * Part of 001-bootstrap-fixpoint Phase 13D-9, User Story 3
 *
 * Tests FR-006: Fixpoint verification script exit codes
 *   - Exit 0: Stage 1 and Stage 2 are byte-identical (ACHIEVED)
 *   - Exit 1: Binaries differ (NOT_ACHIEVED)
 *   - Exit 2: Compilation error (Stage 2 generation failed)
 *   - Exit 3: Missing dependency (wasmtime/wasm-tools/Stage 1 missing)
 */

const { spawn, execSync } = require('child_process');
const fs = require('fs');
const path = require('path');

const PROJECT_ROOT = path.resolve(__dirname, '../../..');
const SCRIPT_PATH = path.join(PROJECT_ROOT, 'scripts/verify-fixpoint.sh');
const DIST_DIR = path.join(PROJECT_ROOT, 'dist');

// Test utilities
function runScript(args = []) {
    return new Promise((resolve) => {
        const proc = spawn('bash', [SCRIPT_PATH, ...args], {
            cwd: PROJECT_ROOT,
            stdio: ['pipe', 'pipe', 'pipe']
        });

        let stdout = '';
        let stderr = '';

        proc.stdout.on('data', (data) => { stdout += data.toString(); });
        proc.stderr.on('data', (data) => { stderr += data.toString(); });

        proc.on('close', (code) => {
            resolve({ code, stdout, stderr });
        });
    });
}

function createTestWasm(outputPath, content) {
    // Minimal valid Wasm module
    const minimalWasm = Buffer.from([
        0x00, 0x61, 0x73, 0x6d, // magic
        0x01, 0x00, 0x00, 0x00, // version
        0x01, 0x05, 0x01, 0x60, 0x00, 0x01, 0x7f, // type section
        0x03, 0x02, 0x01, 0x00, // function section
        0x07, 0x0a, 0x01, 0x06, 0x5f, 0x73, 0x74, 0x61, 0x72, 0x74, 0x00, 0x00, // export section
        0x0a, 0x06, 0x01, 0x04, 0x00, 0x41, 0x00, 0x0b  // code section
    ]);

    if (content) {
        fs.writeFileSync(outputPath, content);
    } else {
        fs.writeFileSync(outputPath, minimalWasm);
    }
}

let testsRun = 0;
let testsPassed = 0;

async function test(name, fn) {
    testsRun++;
    try {
        await fn();
        testsPassed++;
        console.log(`  PASS: ${name}`);
    } catch (err) {
        console.log(`  FAIL: ${name}`);
        console.log(`        ${err.message}`);
    }
}

function assertEqual(actual, expected, msg = '') {
    if (actual !== expected) {
        throw new Error(`Expected ${expected}, got ${actual}${msg ? ': ' + msg : ''}`);
    }
}

function assertIncludes(arr, value, msg = '') {
    if (!arr.includes(value)) {
        throw new Error(`Expected array to include ${value}${msg ? ': ' + msg : ''}`);
    }
}

// Tests
async function runTests() {
    console.log('=== T041: Fixpoint Script Exit Codes ===\n');

    // Backup original files
    const stage1Path = path.join(DIST_DIR, 'clysm-stage1.wasm');
    const stage2Path = path.join(DIST_DIR, 'clysm-stage2.wasm');
    const stage1Backup = path.join(DIST_DIR, 'clysm-stage1.wasm.bak');
    const stage2Backup = path.join(DIST_DIR, 'clysm-stage2.wasm.bak');

    let stage1Existed = false;
    let stage2Existed = false;

    if (fs.existsSync(stage1Path)) {
        fs.copyFileSync(stage1Path, stage1Backup);
        stage1Existed = true;
    }
    if (fs.existsSync(stage2Path)) {
        fs.copyFileSync(stage2Path, stage2Backup);
        stage2Existed = true;
    }

    try {
        // Test 1: Exit 0 when binaries are identical
        await test('Exit 0 when Stage 1 and Stage 2 are identical', async () => {
            // Create identical test binaries
            createTestWasm(stage1Path);
            createTestWasm(stage2Path);

            const result = await runScript(['--skip-generate']);
            assertEqual(result.code, 0, 'Should exit 0 for identical binaries');
        });

        // Test 2: Exit 1 when binaries differ
        await test('Exit 1 when Stage 1 and Stage 2 differ', async () => {
            // Create different test binaries
            createTestWasm(stage1Path);
            // Create a slightly different Wasm
            const differentWasm = Buffer.from([
                0x00, 0x61, 0x73, 0x6d,
                0x01, 0x00, 0x00, 0x00,
                0x01, 0x05, 0x01, 0x60, 0x00, 0x01, 0x7f,
                0x03, 0x02, 0x01, 0x00,
                0x07, 0x0a, 0x01, 0x06, 0x5f, 0x73, 0x74, 0x61, 0x72, 0x74, 0x00, 0x00,
                0x0a, 0x06, 0x01, 0x04, 0x00, 0x41, 0x2a, 0x0b  // Different constant (42 instead of 0)
            ]);
            fs.writeFileSync(stage2Path, differentWasm);

            const result = await runScript(['--skip-generate']);
            assertEqual(result.code, 1, 'Should exit 1 for different binaries');
        });

        // Test 3: Exit 3 when Stage 1 is missing
        await test('Exit 3 when Stage 1 is missing', async () => {
            if (fs.existsSync(stage1Path)) {
                fs.unlinkSync(stage1Path);
            }

            const result = await runScript(['--skip-generate']);
            assertEqual(result.code, 3, 'Should exit 3 for missing Stage 1');
        });

        // Test 4: JSON output includes correct status for identical
        await test('JSON output has status ACHIEVED for identical binaries', async () => {
            createTestWasm(stage1Path);
            createTestWasm(stage2Path);

            const result = await runScript(['--skip-generate', '--json']);
            assertEqual(result.code, 0, 'Should exit 0');

            const json = JSON.parse(result.stdout);
            assertEqual(json.status, 'ACHIEVED', 'Status should be ACHIEVED');
            assertEqual(json.comparison.identical, true, 'Should report identical');
        });

        // Test 5: JSON output includes correct status for different
        await test('JSON output has status NOT_ACHIEVED for different binaries', async () => {
            createTestWasm(stage1Path);
            const differentWasm = Buffer.from([
                0x00, 0x61, 0x73, 0x6d,
                0x01, 0x00, 0x00, 0x00,
                0x01, 0x05, 0x01, 0x60, 0x00, 0x01, 0x7f,
                0x03, 0x02, 0x01, 0x00,
                0x07, 0x0a, 0x01, 0x06, 0x5f, 0x73, 0x74, 0x61, 0x72, 0x74, 0x00, 0x00,
                0x0a, 0x06, 0x01, 0x04, 0x00, 0x41, 0x2a, 0x0b
            ]);
            fs.writeFileSync(stage2Path, differentWasm);

            const result = await runScript(['--skip-generate', '--json']);
            assertEqual(result.code, 1, 'Should exit 1');

            const json = JSON.parse(result.stdout);
            assertEqual(json.status, 'NOT_ACHIEVED', 'Status should be NOT_ACHIEVED');
            assertEqual(json.comparison.identical, false, 'Should report not identical');
        });

    } finally {
        // Restore original files
        if (stage1Existed && fs.existsSync(stage1Backup)) {
            fs.copyFileSync(stage1Backup, stage1Path);
            fs.unlinkSync(stage1Backup);
        }
        if (stage2Existed && fs.existsSync(stage2Backup)) {
            fs.copyFileSync(stage2Backup, stage2Path);
            fs.unlinkSync(stage2Backup);
        }
    }

    console.log(`\n=== Results: ${testsPassed}/${testsRun} passed ===`);

    if (testsPassed < testsRun) {
        process.exit(1);
    }
}

runTests().catch(err => {
    console.error('Test suite error:', err);
    process.exit(1);
});
