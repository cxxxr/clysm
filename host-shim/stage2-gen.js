#!/usr/bin/env node
/**
 * stage2-gen.js - Stage 2 Wasm binary generator (T032-T040)
 * Part of 001-bootstrap-fixpoint Phase 13D-9
 *
 * Loads Stage 1 Wasm and uses compile_form to compile Clysm source into Stage 2.
 *
 * Exit codes:
 *   0 - Success (Stage 2 generated and validates)
 *   1 - Partial success (Stage 2 generated with compilation failures)
 *   2 - Failure (Stage 2 generation failed completely)
 */

const fs = require('fs');
const path = require('path');
const { execSync, spawnSync } = require('child_process');

// Configuration
const PROJECT_ROOT = path.join(__dirname, '..');
const STAGE1_PATH = path.join(PROJECT_ROOT, 'dist/clysm-stage1.wasm');
const STAGE2_PATH = path.join(PROJECT_ROOT, 'dist/clysm-stage2.wasm');
const STAGE2_REPORT_PATH = path.join(PROJECT_ROOT, 'dist/stage2-report.json');
const SRC_DIR = path.join(PROJECT_ROOT, 'src/clysm');

// Statistics tracking (T037)
const stats = {
  formsTotal: 0,
  formsSuccess: 0,
  formsFailed: 0,
  categories: {},
  startTime: Date.now()
};

/**
 * T033: Discover source files in dependency order
 */
function discoverSourceFiles() {
  const files = [];

  // Define module loading order based on dependencies
  const moduleOrder = [
    'lib/package.lisp',
    'lib/macros.lisp',
    'lib/setf-expanders.lisp',
    'compiler/package.lisp',
    'compiler/ast.lisp',
    'compiler/gc-types.lisp',
    'compiler/codegen/wasm-ir.lisp',
    'compiler/codegen/func-section.lisp',
    'compiler/codegen/expr-compiler.lisp',
    'compiler/transform/macro.lisp',
    'compiler/analyzer/io-usage.lisp',
    'compiler/compiler.lisp',
    'eval/package.lisp',
    'eval/interpreter.lisp',
    'eval/jit.lisp',
    'clos/package.lisp',
    'clos/mop.lisp',
    'clos/combination.lisp',
    'ffi/package.lisp',
    'ffi/types.lisp',
    'ffi/registry.lisp',
    'stage0/package.lisp',
    'stage0/exports.lisp',
    'stage1/package.lisp',
    'stage1/types.lisp',
    'stage1/conditions.lisp',
    'stage1/reader.lisp',
    'stage1/runner.lisp',
    'stage1/progress.lisp',
    'stage1/blocker.lisp',
    'stage1/diff.lisp',
    'stage1/fixpoint.lisp',
    'stage1/generator.lisp'
  ];

  for (const relPath of moduleOrder) {
    const fullPath = path.join(SRC_DIR, relPath);
    if (fs.existsSync(fullPath)) {
      files.push(fullPath);
    }
  }

  return files;
}

/**
 * T034: Parse forms from a Lisp source file
 */
function parseFormsFromFile(filePath) {
  const forms = [];
  const content = fs.readFileSync(filePath, 'utf8');

  // Simple form extraction - looks for top-level definitions
  // This is a simplified parser; real implementation would use proper s-expr parser
  const defPatterns = [
    /\(defun\s+[\w-]+/g,
    /\(defmacro\s+[\w-]+/g,
    /\(defclass\s+[\w-]+/g,
    /\(defstruct\s+[\w-]+/g,
    /\(defconstant\s+[\w*-]+/g,
    /\(defvar\s+[\w*-]+/g,
    /\(defparameter\s+[\w*-]+/g
  ];

  for (const pattern of defPatterns) {
    let match;
    while ((match = pattern.exec(content)) !== null) {
      // Extract form name
      const formMatch = match[0].match(/\(def\w+\s+([\w*-]+)/);
      if (formMatch) {
        forms.push({
          type: match[0].split(/\s+/)[0].slice(1), // 'defun', 'defmacro', etc.
          name: formMatch[1],
          file: filePath,
          offset: match.index
        });
      }
    }
  }

  return forms;
}

/**
 * T035: Invoke compile_form for a single form
 */
async function compileForm(wasmInstance, formSexp) {
  try {
    // The compile_form export should accept an S-expression and return Wasm bytes
    const compileFormFn = wasmInstance.exports.compile_form;
    if (!compileFormFn) {
      throw new Error('compile_form export not found in Stage 1');
    }

    // Note: The current stub implementation returns null
    // Once real compiler is in Stage 1, this will return actual Wasm bytes
    const result = compileFormFn(formSexp);
    return result;
  } catch (error) {
    return { error: error.message };
  }
}

/**
 * Record a compilation result
 */
function recordResult(formInfo, success, errorMessage = null) {
  stats.formsTotal++;

  if (success) {
    stats.formsSuccess++;
  } else {
    stats.formsFailed++;

    // Categorize error
    let category = 'unknown-error';
    if (errorMessage) {
      if (errorMessage.includes('unsupported')) {
        category = 'unsupported-feature';
      } else if (errorMessage.includes('type')) {
        category = 'type-error';
      } else if (errorMessage.includes('undefined')) {
        category = 'missing-definition';
      }
    } else {
      category = `unsupported-${formInfo.type}`;
    }

    if (!stats.categories[category]) {
      stats.categories[category] = { count: 0, examples: [] };
    }
    stats.categories[category].count++;
    if (stats.categories[category].examples.length < 5) {
      stats.categories[category].examples.push(formInfo.name);
    }
  }
}

/**
 * T036: Aggregate compiled forms into a Wasm module
 */
function aggregateWasmModule(compiledForms) {
  // For now, since compile_form stubs return null, we'll create a minimal valid module
  // Once Stage 1 has real compiler implementation, this will aggregate actual bytecode

  // Minimal valid Wasm module with _start
  const header = Buffer.from([0x00, 0x61, 0x73, 0x6D, 0x01, 0x00, 0x00, 0x00]);

  // Type section: () -> i32
  const typeSection = Buffer.from([
    0x01, // section id
    0x05, // section size
    0x01, // num types
    0x60, 0x00, 0x01, 0x7F // () -> i32
  ]);

  // Function section: 1 function of type 0
  const funcSection = Buffer.from([
    0x03, // section id
    0x02, // section size
    0x01, // num functions
    0x00  // type index
  ]);

  // Export section: export _start
  const exportSection = Buffer.from([
    0x07, // section id
    0x0A, // section size
    0x01, // num exports
    0x06, // name length
    0x5F, 0x73, 0x74, 0x61, 0x72, 0x74, // "_start"
    0x00, // export kind (func)
    0x00  // func index
  ]);

  // Code section: _start returns 0
  const codeSection = Buffer.from([
    0x0A, // section id
    0x06, // section size
    0x01, // num functions
    0x04, // body size
    0x00, // num locals
    0x41, 0x00, // i32.const 0
    0x0B  // end
  ]);

  return Buffer.concat([header, typeSection, funcSection, exportSection, codeSection]);
}

/**
 * T038: Generate Stage 2 report JSON
 */
function generateReport() {
  const duration = Date.now() - stats.startTime;
  const compilationRate = stats.formsTotal > 0
    ? (stats.formsSuccess / stats.formsTotal) * 100
    : 0;

  // Build blockers array
  const blockers = Object.entries(stats.categories)
    .map(([name, data]) => ({
      category: name,
      count: data.count,
      examples: data.examples,
      remediation: getRemediation(name)
    }))
    .sort((a, b) => b.count - a.count);

  const top5Blockers = blockers.slice(0, 5).map(b => b.category);

  const report = {
    stage: 2,
    timestamp: new Date().toISOString(),
    compilation_rate: Math.round(compilationRate * 100) / 100,
    forms_total: stats.formsTotal,
    forms_success: stats.formsSuccess,
    forms_failed: stats.formsFailed,
    blockers: blockers,
    top_5_blockers: top5Blockers,
    fixpoint_status: 'pending',
    generation_time_ms: duration
  };

  return report;
}

/**
 * Get remediation suggestion for a category
 */
function getRemediation(category) {
  const remediations = {
    'unsupported-macro': 'Implement macro expansion for this form',
    'unsupported-defstruct': 'Add DEFSTRUCT compilation or expand to DEFCLASS',
    'unsupported-feature': 'Implement missing language feature',
    'type-error': 'Fix type mismatch in form compilation',
    'missing-definition': 'Define missing function or variable',
    'unknown-error': 'Investigate error and add specific handling'
  };
  return remediations[category] || `Implement support for ${category}`;
}

/**
 * T040: Validate output with wasm-tools
 */
function validateOutput(wasmPath) {
  const result = spawnSync('wasm-tools', ['validate', wasmPath], {
    encoding: 'utf8'
  });
  return result.status === 0;
}

/**
 * Main entry point
 */
async function main() {
  console.log('=== Stage 2 Generation ===\n');

  // Check prerequisites
  if (!fs.existsSync(STAGE1_PATH)) {
    console.error(`ERROR: Stage 1 not found at ${STAGE1_PATH}`);
    console.error('Run: sbcl --load build/stage1-complete.lisp');
    process.exit(2);
  }

  console.log(`Stage 1: ${STAGE1_PATH}`);
  console.log(`Output: ${STAGE2_PATH}`);
  console.log(`Report: ${STAGE2_REPORT_PATH}\n`);

  // T032: Load Stage 1
  console.log('Loading Stage 1 Wasm...');
  let wasmInstance;
  try {
    const wasmBuffer = fs.readFileSync(STAGE1_PATH);
    const wasmModule = await WebAssembly.compile(wasmBuffer);

    // Create imports (minimal shim)
    const imports = {
      env: {
        // Add any required imports here
      }
    };

    wasmInstance = await WebAssembly.instantiate(wasmModule, imports);
    console.log('Stage 1 loaded successfully.\n');

    // Check for compile_form export
    if (wasmInstance.exports.compile_form) {
      console.log('compile_form export found.');
    } else {
      console.warn('WARNING: compile_form export not found (stub implementation)');
    }
  } catch (error) {
    console.error(`ERROR loading Stage 1: ${error.message}`);
    // Continue with stub implementation
    wasmInstance = { exports: {} };
  }

  // T033: Discover source files
  console.log('\nDiscovering source files...');
  const sourceFiles = discoverSourceFiles();
  console.log(`Found ${sourceFiles.length} source files.\n`);

  // T034-T035: Parse and compile forms
  console.log('Compiling forms...');
  const compiledForms = [];

  for (const file of sourceFiles) {
    const relativePath = path.relative(PROJECT_ROOT, file);
    const forms = parseFormsFromFile(file);

    for (const form of forms) {
      // Since compile_form is a stub, we record as failed for now
      // Once real implementation exists, this will compile actual forms
      const success = false; // Stub always fails
      recordResult(form, success, 'Stub implementation - not yet functional');

      if (success) {
        compiledForms.push({ form, bytes: null });
      }
    }

    if (forms.length > 0) {
      console.log(`  ${relativePath}: ${forms.length} forms`);
    }
  }

  console.log(`\nCompilation complete: ${stats.formsSuccess}/${stats.formsTotal} forms succeeded`);

  // T036: Aggregate into Stage 2 module
  console.log('\nAggregating Stage 2 module...');
  const stage2Bytes = aggregateWasmModule(compiledForms);

  // Write Stage 2 binary
  fs.writeFileSync(STAGE2_PATH, stage2Bytes);
  console.log(`Stage 2 written: ${stage2Bytes.length} bytes`);

  // T040: Validate output
  console.log('\nValidating Stage 2...');
  const isValid = validateOutput(STAGE2_PATH);
  if (isValid) {
    console.log('Validation PASSED');
  } else {
    console.error('Validation FAILED');
  }

  // T038: Generate report
  const report = generateReport();
  fs.writeFileSync(STAGE2_REPORT_PATH, JSON.stringify(report, null, 2));
  console.log(`\nReport written: ${STAGE2_REPORT_PATH}`);

  // Summary
  console.log('\n=== Summary ===');
  console.log(`Forms: ${stats.formsTotal} total, ${stats.formsSuccess} success, ${stats.formsFailed} failed`);
  console.log(`Compilation rate: ${report.compilation_rate}%`);
  console.log(`Top blockers: ${report.top_5_blockers.join(', ') || 'none'}`);

  // Determine exit code
  if (isValid && stats.formsFailed === 0) {
    console.log('\nResult: SUCCESS');
    process.exit(0);
  } else if (isValid) {
    console.log('\nResult: PARTIAL SUCCESS (some forms failed)');
    process.exit(1);
  } else {
    console.log('\nResult: FAILURE (invalid output)');
    process.exit(2);
  }
}

main().catch(error => {
  console.error('Unhandled error:', error);
  process.exit(2);
});
