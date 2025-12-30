/**
 * test-stage2-exit.js - Contract test for Stage 2 generation exit codes (T029)
 * Part of 001-bootstrap-fixpoint Phase 13D-9
 *
 * Tests that the Stage 2 generation script exits with appropriate codes:
 * - 0: Success (Stage 2 generated and validates)
 * - 1: Partial success (Stage 2 generated with some compilation failures)
 * - 2: Failure (Stage 2 generation failed completely)
 *
 * TDD: This test should FAIL before stage2-gen.js is implemented.
 */

const { execSync, spawnSync } = require('child_process');
const path = require('path');
const fs = require('fs');

const STAGE2_GEN_SCRIPT = path.join(__dirname, '../../../host-shim/stage2-gen.js');
const STAGE2_OUTPUT = path.join(__dirname, '../../../dist/clysm-stage2.wasm');

describe('Stage 2 Generation Exit Codes (T029)', () => {
  beforeAll(() => {
    // Check prerequisites
    const stage1Path = path.join(__dirname, '../../../dist/clysm-stage1.wasm');
    if (!fs.existsSync(stage1Path)) {
      console.warn('Stage 1 not found, tests will be skipped');
    }
  });

  test('stage2-gen.js script should exist', () => {
    // This test will fail until stage2-gen.js is created
    if (!fs.existsSync(STAGE2_GEN_SCRIPT)) {
      throw new Error(`Stage 2 generation script not found: ${STAGE2_GEN_SCRIPT}`);
    }
    expect(fs.existsSync(STAGE2_GEN_SCRIPT)).toBe(true);
  });

  test('stage2-gen.js should exit with code 0, 1, or 2', () => {
    // Skip if script doesn't exist
    if (!fs.existsSync(STAGE2_GEN_SCRIPT)) {
      console.log('SKIP: stage2-gen.js not implemented yet');
      return;
    }

    const result = spawnSync('node', [STAGE2_GEN_SCRIPT], {
      cwd: path.join(__dirname, '../../../'),
      timeout: 300000, // 5 minutes
      encoding: 'utf8'
    });

    // Should exit with 0 (success), 1 (partial), or 2 (failure)
    expect([0, 1, 2]).toContain(result.status);
  });

  test('exit code 0 should produce valid Stage 2 wasm', () => {
    if (!fs.existsSync(STAGE2_GEN_SCRIPT)) {
      console.log('SKIP: stage2-gen.js not implemented yet');
      return;
    }

    const result = spawnSync('node', [STAGE2_GEN_SCRIPT], {
      cwd: path.join(__dirname, '../../../'),
      timeout: 300000,
      encoding: 'utf8'
    });

    if (result.status === 0) {
      // Should have produced a valid Stage 2 file
      expect(fs.existsSync(STAGE2_OUTPUT)).toBe(true);

      // Validate with wasm-tools
      const validateResult = spawnSync('wasm-tools', ['validate', STAGE2_OUTPUT]);
      expect(validateResult.status).toBe(0);
    }
  });

  test('exit code 1 should still produce output with report', () => {
    if (!fs.existsSync(STAGE2_GEN_SCRIPT)) {
      console.log('SKIP: stage2-gen.js not implemented yet');
      return;
    }

    const result = spawnSync('node', [STAGE2_GEN_SCRIPT], {
      cwd: path.join(__dirname, '../../../'),
      timeout: 300000,
      encoding: 'utf8'
    });

    if (result.status === 1) {
      // Partial success should still produce a report
      const reportPath = path.join(__dirname, '../../../dist/stage2-report.json');
      expect(fs.existsSync(reportPath)).toBe(true);
    }
  });
});
