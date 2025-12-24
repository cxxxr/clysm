/**
 * io-shim.js - Host I/O function implementations
 * FFI-based stream I/O (015-ffi-stream-io)
 *
 * Provides host functions for Clysm-compiled WebAssembly modules.
 * Imports under the "clysm:io" module namespace.
 */

// Standard file descriptors
const FD_STDIN = 0;
const FD_STDOUT = 1;
const FD_STDERR = 2;

// Input buffer for stdin simulation (for testing)
let stdinBuffer = '';
let stdinPosition = 0;

/**
 * Set stdin content for testing
 * @param {string} content - Content to use as stdin
 */
export function setStdinContent(content) {
    stdinBuffer = content;
    stdinPosition = 0;
}

/**
 * Write a Unicode codepoint to file descriptor (T019)
 * @param {number} fd - File descriptor (0, 1, or 2)
 * @param {number} codepoint - Unicode codepoint to write
 */
export function writeChar(fd, codepoint) {
    const char = String.fromCodePoint(codepoint);
    switch (fd) {
        case FD_STDOUT:
            process.stdout.write(char);
            break;
        case FD_STDERR:
            process.stderr.write(char);
            break;
        default:
            throw new Error(`Invalid file descriptor for write: ${fd}`);
    }
}

/**
 * Write a string to file descriptor (T020)
 * @param {number} fd - File descriptor (0, 1, or 2)
 * @param {string} str - String to write (WasmGC externref)
 */
export function writeString(fd, str) {
    switch (fd) {
        case FD_STDOUT:
            process.stdout.write(str);
            break;
        case FD_STDERR:
            process.stderr.write(str);
            break;
        default:
            throw new Error(`Invalid file descriptor for write: ${fd}`);
    }
}

/**
 * Read a Unicode codepoint from file descriptor (T021)
 * @param {number} fd - File descriptor (must be 0 for stdin)
 * @returns {number} Unicode codepoint, or -1 on EOF
 */
export function readChar(fd) {
    if (fd !== FD_STDIN) {
        throw new Error(`Invalid file descriptor for read: ${fd}`);
    }

    if (stdinPosition >= stdinBuffer.length) {
        return -1; // EOF
    }

    const codepoint = stdinBuffer.codePointAt(stdinPosition);
    // Handle surrogate pairs
    stdinPosition += codepoint > 0xFFFF ? 2 : 1;
    return codepoint;
}

/**
 * Read a line from file descriptor (T022)
 * @param {number} fd - File descriptor (must be 0 for stdin)
 * @returns {string|null} Line without newline, or null on EOF
 */
export function readLine(fd) {
    if (fd !== FD_STDIN) {
        throw new Error(`Invalid file descriptor for read: ${fd}`);
    }

    if (stdinPosition >= stdinBuffer.length) {
        return null; // EOF
    }

    const newlinePos = stdinBuffer.indexOf('\n', stdinPosition);
    let line;

    if (newlinePos === -1) {
        // No more newlines - return rest of buffer
        line = stdinBuffer.slice(stdinPosition);
        stdinPosition = stdinBuffer.length;
    } else {
        line = stdinBuffer.slice(stdinPosition, newlinePos);
        stdinPosition = newlinePos + 1;
    }

    return line;
}

/**
 * Get imports object for WebAssembly instantiation
 * @returns {Object} Import object with clysm:io functions
 */
export function getImports() {
    return {
        'clysm:io': {
            'write-char': writeChar,
            'write-string': writeString,
            'read-char': readChar,
            'read-line': readLine
        }
    };
}

export default {
    writeChar,
    writeString,
    readChar,
    readLine,
    setStdinContent,
    getImports,
    FD_STDIN,
    FD_STDOUT,
    FD_STDERR
};
