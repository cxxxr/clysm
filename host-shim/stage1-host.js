#!/usr/bin/env node
/**
 * stage1-host.js - JavaScript host shim for Stage 1 compiler generation
 *
 * Part of Feature 039: Stage 1 Compiler Generation
 *
 * This host shim provides FFI functions for Stage 0 to:
 * - Read source files from the filesystem
 * - Track compilation progress
 * - Write Stage 1 binary output
 *
 * Usage:
 *   node stage1-host.js --stage0 <path> --output <path> --report <path>
 *
 * Exit codes:
 *   0 - Success (Stage 1 generated)
 *   1 - Partial success (Stage 1 generated with errors)
 *   2 - Failure (Stage 1 could not be generated)
 */

const fs = require('fs');
const path = require('path');

// Configuration from command line
const args = parseArgs();
const STAGE0_PATH = args.stage0 || path.join(__dirname, '..', 'dist', 'clysm-stage0.wasm');
const STAGE1_PATH = args.output || path.join(__dirname, '..', 'dist', 'clysm-stage1.wasm');
const REPORT_PATH = args.report || path.join(__dirname, '..', 'dist', 'stage1-report.json');
const SOURCE_ROOT = args.root || path.join(__dirname, '..');

// Module paths in compilation order (from compiler-order.lisp)
const MODULE_PATHS = [
  "src/clysm/backend/leb128.lisp",
  "src/clysm/backend/sections.lisp",
  "src/clysm/backend/wasm-emit.lisp",
  "src/clysm/backend/wat-print.lisp",
  "src/clysm/reader/tokenizer.lisp",
  "src/clysm/reader/parser.lisp",
  "src/clysm/reader/package.lisp",
  "src/clysm/reader/reader.lisp",
  "src/clysm/compiler/ast.lisp",
  "src/clysm/compiler/env.lisp",
  "src/clysm/compiler/analyzer/free-vars.lisp",
  "src/clysm/compiler/analyzer/tail-call.lisp",
  "src/clysm/compiler/analyzer/type-infer.lisp",
  "src/clysm/compiler/analyzer/io-usage.lisp",
  "src/clysm/compiler/transform/closure.lisp",
  "src/clysm/compiler/transform/macro.lisp",
  "src/clysm/compiler/codegen/wasm-ir.lisp",
  "src/clysm/compiler/codegen/gc-types.lisp",
  "src/clysm/compiler/codegen/type-section.lisp",
  "src/clysm/compiler/codegen/func-section.lisp",
  "src/clysm/compiler/compiler.lisp",
  "src/clysm/runtime/objects.lisp",
  "src/clysm/runtime/special-vars.lisp",
  "src/clysm/runtime/multi-value.lisp",
  "src/clysm/runtime/printer.lisp",
  "src/clysm/runtime/condition-runtime.lisp",
  "src/clysm/clos/mop.lisp",
  "src/clysm/clos/defclass.lisp",
  "src/clysm/clos/instance.lisp",
  "src/clysm/clos/slot-access.lisp",
  "src/clysm/clos/generic.lisp",
  "src/clysm/clos/defmethod.lisp",
  "src/clysm/clos/combination.lisp",
  "src/clysm/clos/dispatch.lisp",
  "src/clysm/clos/method-combination.lisp",
  "src/clysm/conditions/package.lisp",
  "src/clysm/conditions/types.lisp",
  "src/clysm/conditions/handlers.lisp",
  "src/clysm/conditions/restarts.lisp",
  "src/clysm/conditions/signaling.lisp",
  "src/clysm/conditions/standard.lisp",
  "src/clysm/lib/utf8.lisp",
  "src/clysm/lib/setf-expanders.lisp",
  "src/clysm/lib/destructuring.lisp",
  "src/clysm/lib/macros.lisp"
];

// Progress tracking
const progressReport = {
  timestamp: new Date().toISOString(),
  stage0_version: 'stage0-v1.0',
  modules: [],
  summary: {
    total_forms: 0,
    compiled: 0,
    failed: 0,
    skipped: 0,
    coverage_pct: 0
  }
};

let currentModule = null;
let compiledBytes = [];

/**
 * Parse command line arguments
 */
function parseArgs() {
  const args = {};
  for (let i = 2; i < process.argv.length; i++) {
    const arg = process.argv[i];
    if (arg.startsWith('--')) {
      const key = arg.slice(2);
      const value = process.argv[++i];
      args[key] = value;
    }
  }
  return args;
}

/**
 * FFI imports for clysm.fs namespace
 */
const fsImports = {
  /**
   * Read a source file and return contents
   * @param {string} pathRef - externref to path string
   * @returns {string} - File contents as externref
   */
  'read-source': (pathRef) => {
    try {
      const fullPath = path.join(SOURCE_ROOT, pathRef);
      if (!fs.existsSync(fullPath)) {
        console.error(`[fs.read-source] File not found: ${fullPath}`);
        return null;
      }
      const contents = fs.readFileSync(fullPath, 'utf-8');
      console.log(`[fs.read-source] Read ${contents.length} bytes from ${pathRef}`);
      return contents;
    } catch (error) {
      console.error(`[fs.read-source] Error: ${error.message}`);
      return null;
    }
  },

  /**
   * Return list of module paths in compilation order
   * @returns {string[]} - Array of path strings
   */
  'list-modules': () => {
    console.log(`[fs.list-modules] Returning ${MODULE_PATHS.length} modules`);
    return MODULE_PATHS;
  }
};

/**
 * FFI imports for clysm.compile namespace
 */
const compileImports = {
  /**
   * Signal start of compilation for a module
   * @param {string} modulePath - Module path
   * @param {number} formCount - Number of forms to compile
   */
  'report-start': (modulePath, formCount) => {
    console.log(`[compile.report-start] ${modulePath}: ${formCount} forms`);
    currentModule = {
      path: modulePath,
      total_forms: formCount,
      compiled: 0,
      failed: 0,
      skipped: 0,
      failures: []
    };
  },

  /**
   * Report result of compiling a single form
   * @param {string} formId - Form identifier
   * @param {number} success - 1 for success, 0 for failure
   * @param {string} errorType - Error category if failed
   * @param {string} errorMsg - Error message if failed
   */
  'report-form-result': (formId, success, errorType, errorMsg) => {
    if (!currentModule) return;

    if (success === 1) {
      currentModule.compiled++;
      progressReport.summary.compiled++;
    } else {
      currentModule.failed++;
      progressReport.summary.failed++;

      // Track failure
      let failureGroup = currentModule.failures.find(f => f.operator === errorType);
      if (!failureGroup) {
        failureGroup = { operator: errorType, count: 0, example: errorMsg };
        currentModule.failures.push(failureGroup);
      }
      failureGroup.count++;
    }
  },

  /**
   * Signal completion of module compilation
   * @param {string} modulePath - Module path
   * @param {number} compiled - Forms successfully compiled
   * @param {number} failed - Forms that failed
   */
  'report-module-complete': (modulePath, compiled, failed) => {
    if (!currentModule) return;

    console.log(`[compile.report-module-complete] ${modulePath}: ${compiled} compiled, ${failed} failed`);
    progressReport.modules.push(currentModule);
    progressReport.summary.total_forms += currentModule.total_forms;
    currentModule = null;
  }
};

/**
 * FFI imports for clysm.output namespace
 */
const outputImports = {
  /**
   * Write compiled Wasm bytes to output
   * @param {Uint8Array} bytes - Byte array to write
   * @param {string} pathRef - Output file path
   * @returns {number} - Bytes written
   */
  'write-bytes': (bytes, pathRef) => {
    try {
      const outputPath = pathRef || STAGE1_PATH;
      fs.writeFileSync(outputPath, bytes);
      console.log(`[output.write-bytes] Wrote ${bytes.length} bytes to ${outputPath}`);
      return bytes.length;
    } catch (error) {
      console.error(`[output.write-bytes] Error: ${error.message}`);
      return 0;
    }
  }
};

/**
 * Host imports combining all namespaces
 */
const hostImports = {
  'clysm.fs': fsImports,
  'clysm.compile': compileImports,
  'clysm.output': outputImports,
  // Legacy imports for compatibility
  host: {
    read_string: () => 0,
    write_bytes: () => 0,
    log: () => {},
    error: (code) => console.error(`[host.error] Code: ${code}`)
  }
};

/**
 * Calculate and update coverage percentage
 */
function updateCoverage() {
  const { total_forms, compiled } = progressReport.summary;
  progressReport.summary.coverage_pct = total_forms > 0
    ? (compiled / total_forms * 100).toFixed(2)
    : 0;
}

/**
 * Write progress report to file
 */
function writeProgressReport() {
  updateCoverage();
  fs.writeFileSync(REPORT_PATH, JSON.stringify(progressReport, null, 2));
  console.log(`[report] Written to ${REPORT_PATH}`);
}

/**
 * Load and run Stage 0 for compilation
 */
async function runStage1Generation() {
  console.log('='.repeat(60));
  console.log('Stage 1 Compiler Generation');
  console.log('='.repeat(60));
  console.log(`Stage 0: ${STAGE0_PATH}`);
  console.log(`Output:  ${STAGE1_PATH}`);
  console.log(`Report:  ${REPORT_PATH}`);
  console.log('='.repeat(60));

  // Check Stage 0 exists
  if (!fs.existsSync(STAGE0_PATH)) {
    console.error('ERROR: Stage 0 binary not found');
    console.error('Run: sbcl --load build/bootstrap.lisp');
    process.exit(2);
  }

  try {
    // Load Stage 0
    const wasmBuffer = fs.readFileSync(STAGE0_PATH);
    console.log(`[load] Stage 0 binary: ${wasmBuffer.length} bytes`);

    // Compile module
    const wasmModule = await WebAssembly.compile(wasmBuffer);
    console.log('[load] WebAssembly module compiled');

    // Check exports
    const exports = WebAssembly.Module.exports(wasmModule);
    console.log(`[load] Exports: ${exports.map(e => e.name).join(', ') || '(none)'}`);

    // Check for required exports
    const hasCompileAll = exports.some(e => e.name === 'compile_all');
    const hasCompileModule = exports.some(e => e.name === 'compile_module');

    if (!hasCompileAll && !hasCompileModule) {
      console.log('\n[KNOWN LIMITATION]');
      console.log('Stage 0 does not export compile_all or compile_module.');
      console.log('This is expected until Clysm supports more CL features.');
      console.log('\nGenerating placeholder Stage 1 binary...');

      // Generate minimal valid Wasm binary as placeholder
      generatePlaceholderStage1();

      writeProgressReport();
      console.log('\nResult: PARTIAL (placeholder Stage 1 generated)');
      process.exit(1);
    }

    // Instantiate with FFI imports
    const instance = await WebAssembly.instantiate(wasmModule, hostImports);
    console.log('[load] Instance created');

    // Run compile_all if available
    if (hasCompileAll) {
      console.log('\n[compile] Running compile_all...');
      const result = instance.exports.compile_all();
      console.log(`[compile] Result: ${result}`);
    }

    writeProgressReport();

    // Check if Stage 1 was generated
    if (fs.existsSync(STAGE1_PATH)) {
      const stage1Size = fs.statSync(STAGE1_PATH).size;
      console.log(`\n[success] Stage 1 generated: ${stage1Size} bytes`);
      process.exit(0);
    } else {
      console.log('\n[warning] Stage 1 not generated');
      process.exit(1);
    }

  } catch (error) {
    console.error('\n[error]', error.message);

    if (error.message.includes('import')) {
      console.error('\nMissing imports. Stage 0 may require additional FFI functions.');
    }

    writeProgressReport();
    process.exit(2);
  }
}

/**
 * Generate a minimal valid Wasm binary as placeholder Stage 1
 */
function generatePlaceholderStage1() {
  // Minimal valid Wasm module:
  // magic + version + empty type section + empty function section + empty code section
  const wasmBytes = new Uint8Array([
    0x00, 0x61, 0x73, 0x6d,  // magic: \0asm
    0x01, 0x00, 0x00, 0x00,  // version: 1

    // Type section (id=1) - empty
    0x01,        // section id
    0x01,        // section size (1 byte)
    0x00,        // 0 types

    // Function section (id=3) - empty
    0x03,        // section id
    0x01,        // section size
    0x00,        // 0 functions

    // Code section (id=10) - empty
    0x0a,        // section id
    0x01,        // section size
    0x00         // 0 code entries
  ]);

  fs.writeFileSync(STAGE1_PATH, wasmBytes);
  console.log(`[placeholder] Generated ${wasmBytes.length} bytes`);

  // Update progress report
  progressReport.summary.total_forms = MODULE_PATHS.length * 20; // Estimate
  progressReport.summary.compiled = 0;
  progressReport.summary.failed = progressReport.summary.total_forms;
}

/**
 * Main entry point
 */
async function main() {
  if (args.help) {
    console.log('Usage: node stage1-host.js [options]');
    console.log('');
    console.log('Options:');
    console.log('  --stage0 <path>   Path to Stage 0 binary (default: dist/clysm-stage0.wasm)');
    console.log('  --output <path>   Path for Stage 1 output (default: dist/clysm-stage1.wasm)');
    console.log('  --report <path>   Path for progress report (default: dist/stage1-report.json)');
    console.log('  --root <path>     Source root directory (default: project root)');
    console.log('  --help            Show this help');
    process.exit(0);
  }

  await runStage1Generation();
}

main().catch(error => {
  console.error('Unexpected error:', error);
  process.exit(2);
});
