/**
 * package-shim.js - Host package operation implementations
 * Feature: 001-compiler-internal-consolidation (Phase 4, US3)
 *
 * Provides host functions for Clysm-compiled WebAssembly modules.
 * Imports under the "clysm:pkg" module namespace.
 *
 * These functions simulate Common Lisp package operations in JavaScript.
 * For Stage 1 runtime, packages are represented as JavaScript objects
 * with name and symbol table properties.
 */

// Package registry - simulates CL package system
const packages = new Map();

// Initialize standard packages
function initPackages() {
    // Create standard packages
    createPackage('COMMON-LISP', ['CL']);
    createPackage('COMMON-LISP-USER', ['CL-USER']);
    createPackage('KEYWORD', []);
    createPackage('CLYSM', []);
}

/**
 * Create a new package with aliases
 * @param {string} name - Primary package name
 * @param {string[]} nicknames - Package nicknames
 * @returns {Object} Package object
 */
function createPackage(name, nicknames = []) {
    const pkg = {
        name: name,
        nicknames: nicknames,
        symbols: new Map(),
        exports: new Set(),
        uses: []
    };

    // Register by primary name
    packages.set(name.toUpperCase(), pkg);

    // Register by nicknames
    for (const nick of nicknames) {
        packages.set(nick.toUpperCase(), pkg);
    }

    return pkg;
}

/**
 * Find a package by name or nickname (T030)
 * @param {string} name - Package name or nickname
 * @returns {Object|null} Package object or null
 */
export function findPackage(name) {
    if (!name || typeof name !== 'string') {
        return null;
    }
    return packages.get(name.toUpperCase()) || null;
}

/**
 * Intern a symbol in a package (T032)
 * @param {string} name - Symbol name
 * @param {Object|string} pkg - Package object or name
 * @returns {Object} Symbol object (or creates new one)
 */
export function intern(name, pkg) {
    // Resolve package if given as string
    let targetPkg = pkg;
    if (typeof pkg === 'string') {
        targetPkg = findPackage(pkg);
        if (!targetPkg) {
            throw new Error(`Package not found: ${pkg}`);
        }
    }

    if (!targetPkg || typeof targetPkg !== 'object') {
        throw new Error('Invalid package');
    }

    const upperName = name.toUpperCase();

    // Check if symbol exists
    if (targetPkg.symbols.has(upperName)) {
        return targetPkg.symbols.get(upperName);
    }

    // Create new symbol
    const sym = {
        name: upperName,
        package: targetPkg,
        value: undefined,
        function: undefined,
        plist: null
    };

    // Handle KEYWORD package specially - keywords are self-evaluating
    if (targetPkg.name === 'KEYWORD') {
        sym.value = sym;  // Self-evaluating
    }

    targetPkg.symbols.set(upperName, sym);
    return sym;
}

/**
 * Test if object is a package (T034)
 * @param {*} obj - Object to test
 * @returns {boolean} True if package, false otherwise
 */
export function packagep(obj) {
    if (!obj || typeof obj !== 'object') {
        return false;
    }
    // Duck-type check for package structure
    return typeof obj.name === 'string' &&
           obj.symbols instanceof Map &&
           Array.isArray(obj.nicknames);
}

/**
 * Get package name
 * @param {Object} pkg - Package object
 * @returns {string} Package name
 */
export function packageName(pkg) {
    if (!packagep(pkg)) {
        throw new Error('Not a package');
    }
    return pkg.name;
}

/**
 * Get symbol's home package
 * @param {Object} sym - Symbol object
 * @returns {Object|null} Home package or null
 */
export function symbolPackage(sym) {
    if (!sym || typeof sym !== 'object') {
        return null;
    }
    return sym.package || null;
}

/**
 * Get imports object for WebAssembly instantiation
 * @returns {Object} Import object with clysm:pkg functions
 */
export function getImports() {
    return {
        'clysm:pkg': {
            'find-package': findPackage,
            'intern': intern,
            'packagep': packagep,
            'package-name': packageName,
            'symbol-package': symbolPackage
        }
    };
}

// Initialize packages on module load
initPackages();

export default {
    findPackage,
    intern,
    packagep,
    packageName,
    symbolPackage,
    createPackage,
    getImports,
    packages
};
