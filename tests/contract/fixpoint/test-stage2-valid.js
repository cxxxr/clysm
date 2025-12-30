/**
 * test-stage2-valid.js - Contract test for Stage 2 Wasm validation (T030)
 * Part of 001-bootstrap-fixpoint Phase 13D-9
 *
 * Tests that Stage 2 output passes wasm-tools validate.
 * TDD: This test should FAIL before Stage 2 generation is implemented.
 */

const { spawnSync } = require('child_process');
const path = require('path');
const fs = require('fs');

const STAGE2_OUTPUT = path.join(__dirname, '../../../dist/clysm-stage2.wasm');

describe('Stage 2 Wasm Validation (T030)', () => {
  test('dist/clysm-stage2.wasm should exist', () => {
    // This will fail until Stage 2 is generated
    if (!fs.existsSync(STAGE2_OUTPUT)) {
      throw new Error(`Stage 2 not found: ${STAGE2_OUTPUT}. Run stage2-gen.js first.`);
    }
    expect(fs.existsSync(STAGE2_OUTPUT)).toBe(true);
  });

  test('Stage 2 should pass wasm-tools validate', () => {
    if (!fs.existsSync(STAGE2_OUTPUT)) {
      console.log('SKIP: Stage 2 not generated yet');
      return;
    }

    const result = spawnSync('wasm-tools', ['validate', STAGE2_OUTPUT], {
      encoding: 'utf8'
    });

    if (result.status !== 0) {
      console.error('Validation error:', result.stderr);
    }
    expect(result.status).toBe(0);
  });

  test('Stage 2 should have non-zero size', () => {
    if (!fs.existsSync(STAGE2_OUTPUT)) {
      console.log('SKIP: Stage 2 not generated yet');
      return;
    }

    const stats = fs.statSync(STAGE2_OUTPUT);
    expect(stats.size).toBeGreaterThan(0);
  });

  test('Stage 2 should have Wasm magic bytes', () => {
    if (!fs.existsSync(STAGE2_OUTPUT)) {
      console.log('SKIP: Stage 2 not generated yet');
      return;
    }

    const buffer = fs.readFileSync(STAGE2_OUTPUT);
    // Wasm magic: \0asm
    expect(buffer[0]).toBe(0x00);
    expect(buffer[1]).toBe(0x61); // 'a'
    expect(buffer[2]).toBe(0x73); // 's'
    expect(buffer[3]).toBe(0x6d); // 'm'
  });

  test('Stage 2 should export _start function', () => {
    if (!fs.existsSync(STAGE2_OUTPUT)) {
      console.log('SKIP: Stage 2 not generated yet');
      return;
    }

    const result = spawnSync('wasm-tools', ['print', STAGE2_OUTPUT], {
      encoding: 'utf8'
    });

    expect(result.stdout).toContain('export "_start"');
  });
});
