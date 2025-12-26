// FFI Test Host - Mock functions for testing FFI functionality
// Feature: 027-complete-ffi

/**
 * Creates the host imports object for FFI testing.
 * These functions simulate host environment capabilities.
 */
function createFFITestHost(wasmInstance) {
  const logs = [];

  return {
    host: {
      // Basic logging - captures to array for testing
      log: (msg) => {
        logs.push(String(msg));
        console.log('[host.log]', msg);
      },

      // Arithmetic - for testing fixnum marshalling
      add: (a, b) => {
        if (typeof a !== 'number' || typeof b !== 'number') {
          throw new Error(`host.add: expected numbers, got ${typeof a} and ${typeof b}`);
        }
        return a + b;
      },

      // Subtract - additional arithmetic
      sub: (a, b) => a - b,

      // Multiply - for testing larger values
      mul: (a, b) => a * b,

      // Division - for testing float returns
      div: (a, b) => {
        if (b === 0) {
          throw new Error('host.div: division by zero');
        }
        return a / b;
      },

      // Float operations - for testing float marshalling
      sqrt: (x) => Math.sqrt(x),
      sin: (x) => Math.sin(x),
      cos: (x) => Math.cos(x),

      // Random - returns float between 0 and 1
      random: () => Math.random(),

      // String operations - for testing string marshalling
      concat: (a, b) => String(a) + String(b),
      length: (s) => String(s).length,
      uppercase: (s) => String(s).toUpperCase(),

      // Boolean operations - for testing boolean marshalling
      negate: (b) => !b,
      and: (a, b) => Boolean(a) && Boolean(b),
      or: (a, b) => Boolean(a) || Boolean(b),

      // Error testing - intentionally throws
      throw_error: (msg) => {
        throw new Error(msg || 'Intentional test error');
      },

      // Identity - returns input unchanged (for anyref testing)
      identity: (x) => x,

      // Type checking
      type_of: (x) => typeof x,

      // Null testing
      is_null: (x) => x === null || x === undefined
    },

    ffi: {
      // Dynamic host function dispatch
      // Called by (ffi:call-host "module.function" args...)
      call_host_dynamic: function(funcName, argsArray) {
        // Decode function name from externref
        const name = funcName ? String(funcName) : '';
        const parts = name.split('.');

        if (parts.length !== 2) {
          throw new Error(`Invalid function name format: "${name}" (expected "module.function")`);
        }

        const [moduleName, fieldName] = parts;

        // Look up module in the host object
        // This function is bound to the host object via closure
        const hostObj = {
          host: {
            log: (msg) => {
              logs.push(String(msg));
              console.log('[host.log]', msg);
            },
            add: (a, b) => Number(a) + Number(b),
            sub: (a, b) => Number(a) - Number(b),
            mul: (a, b) => Number(a) * Number(b),
            div: (a, b) => {
              if (Number(b) === 0) throw new Error('host.div: division by zero');
              return Number(a) / Number(b);
            },
            sqrt: (x) => Math.sqrt(Number(x)),
            sin: (x) => Math.sin(Number(x)),
            cos: (x) => Math.cos(Number(x)),
            random: () => Math.random(),
            concat: (a, b) => String(a) + String(b),
            length: (s) => String(s).length,
            uppercase: (s) => String(s).toUpperCase(),
            negate: (b) => !b,
            and: (a, b) => Boolean(a) && Boolean(b),
            or: (a, b) => Boolean(a) || Boolean(b),
            throw_error: (msg) => { throw new Error(msg || 'Intentional test error'); },
            identity: (x) => x,
            type_of: (x) => typeof x,
            is_null: (x) => x === null || x === undefined,
            check: () => true  // Simple boolean return for testing
          }
        };

        const module = hostObj[moduleName];
        if (!module) {
          throw new Error(`ffi.call_host_dynamic: Unknown module "${moduleName}"`);
        }

        const fn = module[fieldName];
        if (typeof fn !== 'function') {
          throw new Error(`ffi.call_host_dynamic: Unknown function "${name}"`);
        }

        // Convert argsArray from Wasm externref array to JS array
        // argsArray may be null (no args) or an array-like externref
        let args = [];
        if (argsArray !== null && argsArray !== undefined) {
          // Try to iterate over the array
          if (typeof argsArray[Symbol.iterator] === 'function') {
            args = Array.from(argsArray);
          } else if (typeof argsArray.length === 'number') {
            for (let i = 0; i < argsArray.length; i++) {
              args.push(argsArray[i]);
            }
          }
        }

        return fn.apply(module, args);
      }
    },

    // Test utilities (not exposed to Wasm)
    _test: {
      getLogs: () => [...logs],
      clearLogs: () => { logs.length = 0; },
      getLogCount: () => logs.length
    }
  };
}

// Callback test functions - for testing re-entrant calls
function createCallbackHost(wasmExports) {
  const baseHost = createFFITestHost(null);

  return {
    ...baseHost,

    callback: {
      // Calls an exported Lisp function
      invoke: (exportName, ...args) => {
        const fn = wasmExports[exportName];
        if (typeof fn !== 'function') {
          throw new Error(`No export named: ${exportName}`);
        }
        return fn.apply(null, args);
      },

      // Calls a Lisp function, then calls another host function
      // Tests nested callbacks: Lisp -> Host -> Lisp -> Host
      nested_call: (exportName, hostFunc, ...args) => {
        const lispFn = wasmExports[exportName];
        if (typeof lispFn !== 'function') {
          throw new Error(`No export named: ${exportName}`);
        }

        const lispResult = lispFn.apply(null, args);

        // Now call a host function with the result
        const hostModule = baseHost.host;
        if (typeof hostModule[hostFunc] !== 'function') {
          throw new Error(`No host function: ${hostFunc}`);
        }

        return hostModule[hostFunc](lispResult);
      },

      // Depth-3 callback test
      depth3: (export1, export2, value) => {
        // Host calls export1 -> Lisp calls Host -> Host calls export2
        const fn1 = wasmExports[export1];
        const fn2 = wasmExports[export2];

        if (typeof fn1 !== 'function' || typeof fn2 !== 'function') {
          throw new Error(`Missing exports: ${export1} or ${export2}`);
        }

        // First callback
        const result1 = fn1(value);
        // Second callback (simulating host calling back into Lisp again)
        const result2 = fn2(result1);

        return result2;
      }
    }
  };
}

// Export for Node.js
if (typeof module !== 'undefined' && module.exports) {
  module.exports = { createFFITestHost, createCallbackHost };
}

// Export for ES modules
if (typeof globalThis !== 'undefined') {
  globalThis.createFFITestHost = createFFITestHost;
  globalThis.createCallbackHost = createCallbackHost;
}
