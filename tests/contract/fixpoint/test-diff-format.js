/**
 * test-diff-format.js - Contract test for diff output format validation (T042)
 *
 * Part of 001-bootstrap-fixpoint Phase 13D-9, User Story 3
 *
 * Tests FR-006: When Stage 1 and Stage 2 differ, the verification script must provide:
 *   - First difference offset (byte position)
 *   - Size difference
 *   - Total differing bytes count
 *   - Section-level diff information (when using wasm-tools)
 */

const { spawn } = require('child_process');
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

function createTestWasm(outputPath, variant = 'base') {
    // Base minimal valid Wasm module
    const baseWasm = Buffer.from([
        0x00, 0x61, 0x73, 0x6d, // magic
        0x01, 0x00, 0x00, 0x00, // version
        0x01, 0x05, 0x01, 0x60, 0x00, 0x01, 0x7f, // type section
        0x03, 0x02, 0x01, 0x00, // function section
        0x07, 0x0a, 0x01, 0x06, 0x5f, 0x73, 0x74, 0x61, 0x72, 0x74, 0x00, 0x00, // export section
        0x0a, 0x06, 0x01, 0x04, 0x00, 0x41, 0x00, 0x0b  // code section
    ]);

    if (variant === 'base') {
        fs.writeFileSync(outputPath, baseWasm);
    } else if (variant === 'different-constant') {
        // Change i32.const 0 to i32.const 42 (at byte offset 35)
        const modified = Buffer.from(baseWasm);
        modified[35] = 0x2a; // 42
        fs.writeFileSync(outputPath, modified);
    } else if (variant === 'larger') {
        // Add extra bytes to make it larger
        const larger = Buffer.concat([baseWasm, Buffer.from([0x00, 0x00, 0x00, 0x00])]);
        fs.writeFileSync(outputPath, larger);
    } else if (variant === 'multiple-diffs') {
        // Multiple differences at different offsets
        const modified = Buffer.from(baseWasm);
        modified[8] = 0x06;  // Change byte at offset 8
        modified[20] = 0xff; // Change byte at offset 20
        modified[35] = 0x2a; // Change byte at offset 35
        fs.writeFileSync(outputPath, modified);
    }

    return baseWasm.length;
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

function assertTrue(value, msg = '') {
    if (!value) {
        throw new Error(`Expected true${msg ? ': ' + msg : ''}`);
    }
}

function assertHasProperty(obj, prop, msg = '') {
    if (!(prop in obj)) {
        throw new Error(`Object missing property '${prop}'${msg ? ': ' + msg : ''}`);
    }
}

// Tests
async function runTests() {
    console.log('=== T042: Diff Output Format Validation ===\n');

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
        // Test 1: JSON output contains first_diff_offset
        await test('JSON output contains first_diff_offset', async () => {
            createTestWasm(stage1Path, 'base');
            createTestWasm(stage2Path, 'different-constant');

            const result = await runScript(['--skip-generate', '--json']);
            const json = JSON.parse(result.stdout);

            assertHasProperty(json, 'comparison');
            assertHasProperty(json.comparison, 'first_diff_offset');
            assertTrue(json.comparison.first_diff_offset > 0, 'Should have positive offset');
        });

        // Test 2: JSON output contains diff_bytes count
        await test('JSON output contains diff_bytes count', async () => {
            createTestWasm(stage1Path, 'base');
            createTestWasm(stage2Path, 'different-constant');

            const result = await runScript(['--skip-generate', '--json']);
            const json = JSON.parse(result.stdout);

            assertHasProperty(json.comparison, 'diff_bytes');
            assertTrue(json.comparison.diff_bytes >= 1, 'Should have at least 1 diff byte');
        });

        // Test 3: JSON output contains size information
        await test('JSON output contains binary sizes', async () => {
            const baseSize = createTestWasm(stage1Path, 'base');
            createTestWasm(stage2Path, 'larger');

            const result = await runScript(['--skip-generate', '--json']);
            const json = JSON.parse(result.stdout);

            assertHasProperty(json, 'stage1');
            assertHasProperty(json.stage1, 'size_bytes');
            assertHasProperty(json, 'stage2');
            assertHasProperty(json.stage2, 'size_bytes');

            assertEqual(json.stage1.size_bytes, baseSize, 'Stage 1 size should match');
            assertTrue(json.stage2.size_bytes > json.stage1.size_bytes, 'Stage 2 should be larger');
        });

        // Test 4: JSON output contains timing information
        await test('JSON output contains timing information', async () => {
            createTestWasm(stage1Path, 'base');
            createTestWasm(stage2Path, 'base');

            const result = await runScript(['--skip-generate', '--json']);
            const json = JSON.parse(result.stdout);

            assertHasProperty(json, 'timing');
            assertHasProperty(json.timing, 'comparison_ms');
            assertTrue(typeof json.timing.comparison_ms === 'number', 'comparison_ms should be a number');
        });

        // Test 5: JSON output has required schema for FixpointResult
        await test('JSON output matches FixpointResult schema', async () => {
            createTestWasm(stage1Path, 'base');
            createTestWasm(stage2Path, 'different-constant');

            const result = await runScript(['--skip-generate', '--json']);
            const json = JSON.parse(result.stdout);

            // Required top-level fields
            assertHasProperty(json, 'status');
            assertHasProperty(json, 'timestamp');
            assertHasProperty(json, 'stage1');
            assertHasProperty(json, 'stage2');
            assertHasProperty(json, 'comparison');
            assertHasProperty(json, 'timing');

            // Stage info fields
            assertHasProperty(json.stage1, 'path');
            assertHasProperty(json.stage1, 'size_bytes');
            assertHasProperty(json.stage1, 'valid');

            // Comparison fields
            assertHasProperty(json.comparison, 'identical');
            assertHasProperty(json.comparison, 'first_diff_offset');
            assertHasProperty(json.comparison, 'diff_bytes');
        });

        // Test 6: Text output includes size difference
        await test('Text output includes size difference', async () => {
            createTestWasm(stage1Path, 'base');
            createTestWasm(stage2Path, 'larger');

            const result = await runScript(['--skip-generate']);
            assertTrue(result.stdout.includes('Size difference') ||
                      result.stdout.includes('size') ||
                      result.stderr.includes('size'),
                      'Should mention size in output');
        });

        // Test 7: Text output includes first difference offset
        await test('Text output includes first difference offset', async () => {
            createTestWasm(stage1Path, 'base');
            createTestWasm(stage2Path, 'different-constant');

            const result = await runScript(['--skip-generate']);
            const output = result.stdout + result.stderr;
            assertTrue(output.includes('offset') || output.includes('byte') || output.includes('difference'),
                      'Should mention difference location in output');
        });

        // Test 8: Multiple differences are counted
        await test('Multiple differences are counted correctly', async () => {
            createTestWasm(stage1Path, 'base');
            createTestWasm(stage2Path, 'multiple-diffs');

            const result = await runScript(['--skip-generate', '--json']);
            const json = JSON.parse(result.stdout);

            assertTrue(json.comparison.diff_bytes >= 3, 'Should report at least 3 differing bytes');
        });

    } finally {
        // Restore original files
        if (stage1Existed && fs.existsSync(stage1Backup)) {
            fs.copyFileSync(stage1Backup, stage1Path);
            fs.unlinkSync(stage1Backup);
        } else if (fs.existsSync(stage1Path) && !stage1Existed) {
            fs.unlinkSync(stage1Path);
        }

        if (stage2Existed && fs.existsSync(stage2Backup)) {
            fs.copyFileSync(stage2Backup, stage2Path);
            fs.unlinkSync(stage2Backup);
        } else if (fs.existsSync(stage2Path) && !stage2Existed) {
            fs.unlinkSync(stage2Path);
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
