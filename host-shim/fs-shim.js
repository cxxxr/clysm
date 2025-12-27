/**
 * fs-shim.js - Host filesystem function implementations
 * FFI Filesystem Access (035-ffi-filesystem)
 *
 * Provides host functions for Clysm-compiled WebAssembly modules.
 * Imports under the "clysm:fs" module namespace.
 *
 * This shim supports two backends:
 * - Node.js/wasmtime: Real filesystem via Node.js 'fs' module
 * - Browser: Virtual filesystem (in-memory or IndexedDB)
 *
 * Task: T010 - Create base host shim structure with getImports() skeleton
 */

import * as fs from 'fs';
import * as path from 'path';

// File handle registry - maps handle IDs to file descriptors
let nextHandleId = 1;
const openHandles = new Map();

/**
 * Internal: Create a new file handle
 * @param {number} fd - Node.js file descriptor
 * @param {string} pathname - Original pathname
 * @param {string} direction - 'input' or 'output'
 * @returns {object} Handle object
 */
function createHandle(fd, pathname, direction) {
    const handle = {
        id: nextHandleId++,
        fd,
        pathname,
        direction,
        closed: false
    };
    openHandles.set(handle.id, handle);
    return handle;
}

/**
 * Open a file and return an opaque handle
 * FR-001: System MUST provide open-file function to obtain a file stream handle.
 *
 * @param {string} pathname - File path to open
 * @param {string} direction - "input" or "output"
 * @param {string} ifExists - "supersede" or "error"
 * @param {string} ifDoesNotExist - "error" or "create"
 * @returns {object} Opaque file handle
 * @throws {Error} If file operation fails
 */
export function open(pathname, direction, ifExists, ifDoesNotExist) {
    let flags;

    if (direction === 'input') {
        // Input: read-only
        if (!fs.existsSync(pathname)) {
            if (ifDoesNotExist === 'error') {
                throw new Error(`File not found: ${pathname}`);
            }
            // ifDoesNotExist === 'create' - create empty file first
            fs.writeFileSync(pathname, '');
        }
        flags = 'r';
    } else if (direction === 'output') {
        // Output: write mode
        if (fs.existsSync(pathname)) {
            if (ifExists === 'error') {
                throw new Error(`File already exists: ${pathname}`);
            }
            // ifExists === 'supersede' - truncate existing file
        } else {
            if (ifDoesNotExist === 'error') {
                throw new Error(`File not found: ${pathname}`);
            }
            // ifDoesNotExist === 'create' - create is implicit with 'w'
        }
        flags = 'w';
    } else {
        throw new Error(`Invalid direction: ${direction}`);
    }

    const fd = fs.openSync(pathname, flags);
    return createHandle(fd, pathname, direction);
}

/**
 * Close a file handle
 * FR-002: System MUST provide close-file function to release a file stream handle.
 * FR-011: System MUST reject operations on closed file streams.
 *
 * @param {object} handle - Opaque file handle from open()
 * @throws {Error} If handle is invalid or already closed
 */
export function close(handle) {
    if (!handle || typeof handle.id !== 'number') {
        throw new Error('Invalid file handle');
    }

    const h = openHandles.get(handle.id);
    if (!h) {
        throw new Error('Unknown file handle');
    }

    if (h.closed) {
        throw new Error(`File already closed: ${h.pathname}`);
    }

    fs.closeSync(h.fd);
    h.closed = true;
    openHandles.delete(handle.id);
}

/**
 * Read entire file contents as UTF-8 string
 * FR-003: System MUST provide read-file-contents function.
 * FR-010: System MUST properly encode/decode UTF-8.
 *
 * @param {object} handle - Opaque file handle from open()
 * @returns {string} File contents as UTF-8 string
 * @throws {Error} If handle is invalid or read fails
 */
export function readAll(handle) {
    if (!handle || typeof handle.id !== 'number') {
        throw new Error('Invalid file handle');
    }

    const h = openHandles.get(handle.id);
    if (!h) {
        throw new Error('Unknown file handle');
    }

    if (h.closed) {
        throw new Error(`File is closed: ${h.pathname}`);
    }

    if (h.direction !== 'input') {
        throw new Error(`Cannot read from output stream: ${h.pathname}`);
    }

    // Read entire file contents
    // Note: For large files, this could be optimized with streaming
    const buffer = fs.readFileSync(h.pathname, { encoding: 'utf8' });
    return buffer;
}

/**
 * Write UTF-8 string to file, replacing contents
 * FR-004: System MUST provide write-file-contents function.
 * FR-010: System MUST properly encode/decode UTF-8.
 *
 * @param {object} handle - Opaque file handle from open()
 * @param {string} contents - String to write
 * @throws {Error} If handle is invalid or write fails
 */
export function writeAll(handle, contents) {
    if (!handle || typeof handle.id !== 'number') {
        throw new Error('Invalid file handle');
    }

    const h = openHandles.get(handle.id);
    if (!h) {
        throw new Error('Unknown file handle');
    }

    if (h.closed) {
        throw new Error(`File is closed: ${h.pathname}`);
    }

    if (h.direction !== 'output') {
        throw new Error(`Cannot write to input stream: ${h.pathname}`);
    }

    // Write entire contents
    fs.writeFileSync(h.pathname, contents, { encoding: 'utf8' });
}

/**
 * Get imports object for WebAssembly instantiation
 * FR-008: System MUST implement file operations via FFI to host environment.
 *
 * @returns {Object} Import object with clysm:fs functions
 */
export function getImports() {
    return {
        'clysm:fs': {
            'open': open,
            'close': close,
            'read-all': readAll,
            'write-all': writeAll
        }
    };
}

// Utility functions for testing

/**
 * Reset all open handles (for test cleanup)
 */
export function resetHandles() {
    for (const [id, handle] of openHandles) {
        if (!handle.closed) {
            try {
                fs.closeSync(handle.fd);
            } catch (e) {
                // Ignore errors during cleanup
            }
        }
    }
    openHandles.clear();
    nextHandleId = 1;
}

/**
 * Get count of open handles (for testing)
 * @returns {number} Number of currently open handles
 */
export function getOpenHandleCount() {
    return openHandles.size;
}

export default {
    open,
    close,
    readAll,
    writeAll,
    getImports,
    resetHandles,
    getOpenHandleCount
};
