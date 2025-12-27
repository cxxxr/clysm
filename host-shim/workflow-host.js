/**
 * workflow-host.js - Node.js host shim for Clysm workflow
 *
 * Part of Feature 041: Development Workflow Establishment
 * T074-T078: FFI host functions for Stage 1+ execution
 *
 * Provides filesystem and process FFI for wasmtime execution:
 * - fs.glob: Expand glob patterns
 * - fs.mtime: Get file modification time
 * - fs.read: Read file contents
 * - fs.exists: Check if file exists
 * - process.args: Get command-line arguments
 * - process.exit: Exit with code
 * - console.log/error: Output to stdout/stderr
 */

const fs = require('fs');
const path = require('path');
const { glob } = require('glob');

// FFI function implementations
const ffiExports = {
  /**
   * Expand glob pattern to matching file paths
   * @param {string} pattern - Glob pattern
   * @param {string} directory - Base directory
   * @returns {string[]} - Matching absolute paths
   */
  'clysm:fs.glob': async (pattern, directory) => {
    try {
      const cwd = directory || process.cwd();
      const matches = await glob(pattern, { cwd, absolute: true });
      return matches.sort();
    } catch (e) {
      console.error(`glob error: ${e.message}`);
      return [];
    }
  },

  /**
   * Get file modification time as Unix timestamp
   * @param {string} pathname - File path
   * @returns {number} - Unix timestamp, or 0 if file doesn't exist
   */
  'clysm:fs.mtime': (pathname) => {
    try {
      const stats = fs.statSync(pathname);
      return Math.floor(stats.mtimeMs / 1000);
    } catch (e) {
      return 0;
    }
  },

  /**
   * Read file contents as UTF-8 string
   * @param {string} pathname - File path
   * @returns {string} - File contents
   * @throws {Error} - If file cannot be read
   */
  'clysm:fs.read': (pathname) => {
    return fs.readFileSync(pathname, 'utf-8');
  },

  /**
   * Write contents to file
   * @param {string} pathname - File path
   * @param {string} contents - Contents to write
   */
  'clysm:fs.write': (pathname, contents) => {
    const dir = path.dirname(pathname);
    if (!fs.existsSync(dir)) {
      fs.mkdirSync(dir, { recursive: true });
    }
    fs.writeFileSync(pathname, contents, 'utf-8');
  },

  /**
   * Write binary contents to file
   * @param {string} pathname - File path
   * @param {Uint8Array} contents - Binary contents
   */
  'clysm:fs.write-binary': (pathname, contents) => {
    const dir = path.dirname(pathname);
    if (!fs.existsSync(dir)) {
      fs.mkdirSync(dir, { recursive: true });
    }
    fs.writeFileSync(pathname, Buffer.from(contents));
  },

  /**
   * Check if file exists
   * @param {string} pathname - File path
   * @returns {number} - 1 if exists, 0 otherwise
   */
  'clysm:fs.exists': (pathname) => {
    return fs.existsSync(pathname) ? 1 : 0;
  },

  /**
   * Create directory
   * @param {string} pathname - Directory path
   */
  'clysm:fs.mkdir': (pathname) => {
    fs.mkdirSync(pathname, { recursive: true });
  },

  /**
   * Get command-line arguments
   * @returns {string[]} - Arguments
   */
  'clysm:process.args': () => {
    // Skip node and script path
    return process.argv.slice(2);
  },

  /**
   * Exit with code
   * @param {number} code - Exit code
   */
  'clysm:process.exit': (code) => {
    process.exit(code);
  },

  /**
   * Print to stdout
   * @param {string} msg - Message
   */
  'clysm:console.log': (msg) => {
    console.log(msg);
  },

  /**
   * Print to stderr
   * @param {string} msg - Message
   */
  'clysm:console.error': (msg) => {
    console.error(msg);
  },

  /**
   * Get current working directory
   * @returns {string} - Current directory
   */
  'clysm:process.cwd': () => {
    return process.cwd();
  },

  /**
   * Get environment variable
   * @param {string} name - Variable name
   * @returns {string|null} - Value or null
   */
  'clysm:process.env': (name) => {
    return process.env[name] || null;
  }
};

// Export for use by wasmtime or other runtimes
module.exports = { ffiExports };

// If run directly, provide info
if (require.main === module) {
  console.log('Clysm Workflow Host Shim');
  console.log('Available FFI functions:');
  Object.keys(ffiExports).forEach(name => {
    console.log(`  - ${name}`);
  });
}
