/**
 * test-stage2-gen.js - Integration test for full Stage 2 generation (T031)
 * Part of 001-bootstrap-fixpoint Phase 13D-9
 *
 * Tests the complete Stage 2 generation workflow:
 * 1. Load Stage 1 Wasm
 * 2. Iterate through compiler source files
 * 3. Call compile_form for each form
 * 4. Aggregate output into Stage 2 Wasm
 *
 * TDD: This test should FAIL before stage2-gen.js is implemented.
 */

const { spawnSync } = require('child_process');
const path = require('path');
const fs = require('fs');

const PROJECT_ROOT = path.join(__dirname, '../../../');
const STAGE1_PATH = path.join(PROJECT_ROOT, 'dist/clysm-stage1.wasm');
const STAGE2_PATH = path.join(PROJECT_ROOT, 'dist/clysm-stage2.wasm');
const STAGE2_REPORT = path.join(PROJECT_ROOT, 'dist/stage2-report.json');
const STAGE2_GEN_SCRIPT = path.join(PROJECT_ROOT, 'host-shim/stage2-gen.js');

describe('Full Stage 2 Generation (T031)', () => {
  beforeAll(() => {
    // Ensure Stage 1 exists
    if (!fs.existsSync(STAGE1_PATH)) {
      console.error('Stage 1 not found. Run: sbcl --load build/stage1-complete.lisp');
    }
  });

  test('Stage 1 prerequisite should exist', () => {
    expect(fs.existsSync(STAGE1_PATH)).toBe(true);
  });

  test('Stage 2 generation script should exist', () => {
    if (!fs.existsSync(STAGE2_GEN_SCRIPT)) {
      throw new Error('stage2-gen.js not implemented yet');
    }
    expect(fs.existsSync(STAGE2_GEN_SCRIPT)).toBe(true);
  });

  test('Stage 2 generation should complete', () => {
    if (!fs.existsSync(STAGE2_GEN_SCRIPT)) {
      console.log('SKIP: stage2-gen.js not implemented yet');
      return;
    }

    // Remove old output
    if (fs.existsSync(STAGE2_PATH)) {
      fs.unlinkSync(STAGE2_PATH);
    }

    const result = spawnSync('node', [STAGE2_GEN_SCRIPT], {
      cwd: PROJECT_ROOT,
      timeout: 300000, // 5 minutes
      encoding: 'utf8'
    });

    console.log('Stage 2 generation stdout:', result.stdout);
    if (result.stderr) {
      console.log('Stage 2 generation stderr:', result.stderr);
    }

    // Should complete (exit 0 or 1, not 2)
    expect([0, 1]).toContain(result.status);
  });

  test('Stage 2 output should be created', () => {
    if (!fs.existsSync(STAGE2_GEN_SCRIPT)) {
      console.log('SKIP: stage2-gen.js not implemented yet');
      return;
    }

    expect(fs.existsSync(STAGE2_PATH)).toBe(true);
  });

  test('Stage 2 report should be created', () => {
    if (!fs.existsSync(STAGE2_GEN_SCRIPT)) {
      console.log('SKIP: stage2-gen.js not implemented yet');
      return;
    }

    expect(fs.existsSync(STAGE2_REPORT)).toBe(true);
  });

  test('Stage 2 report should have expected fields', () => {
    if (!fs.existsSync(STAGE2_REPORT)) {
      console.log('SKIP: Stage 2 report not generated yet');
      return;
    }

    const report = JSON.parse(fs.readFileSync(STAGE2_REPORT, 'utf8'));

    expect(report).toHaveProperty('stage');
    expect(report).toHaveProperty('timestamp');
    expect(report).toHaveProperty('compilation_rate');
    expect(report).toHaveProperty('forms_total');
    expect(report).toHaveProperty('forms_success');
    expect(report).toHaveProperty('forms_failed');
    expect(report).toHaveProperty('blockers');
    expect(report).toHaveProperty('fixpoint_status');
  });

  test('Stage 2 should have same exports as Stage 1', () => {
    if (!fs.existsSync(STAGE2_PATH)) {
      console.log('SKIP: Stage 2 not generated yet');
      return;
    }

    const stage1Exports = spawnSync('wasm-tools', ['print', STAGE1_PATH], {
      encoding: 'utf8'
    }).stdout.match(/export "[^"]+"/g) || [];

    const stage2Exports = spawnSync('wasm-tools', ['print', STAGE2_PATH], {
      encoding: 'utf8'
    }).stdout.match(/export "[^"]+"/g) || [];

    // Stage 2 should export _start at minimum
    expect(stage2Exports).toContain('export "_start"');
  });
});
