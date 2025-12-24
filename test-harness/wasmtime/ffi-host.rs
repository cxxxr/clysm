//! ffi-host.rs - Rust FFI test harness for wasmtime (T063)
//!
//! Provides host functions for testing FFI interoperability with wasmtime.
//! Requires wasmtime with WasmGC support enabled.

use anyhow::Result;
use wasmtime::*;
use std::env;
use std::fs;

/// Host functions that can be imported by Lisp code.
/// These are registered under the "host" namespace.
mod host {
    use wasmtime::*;

    /// Log a message to console
    pub fn log(message: &str) {
        println!("[Lisp] {}", message);
    }

    /// Add two integers
    pub fn add(a: i32, b: i32) -> i32 {
        a + b
    }

    /// Subtract two integers
    pub fn sub(a: i32, b: i32) -> i32 {
        a - b
    }

    /// Multiply two integers
    pub fn mul(a: i32, b: i32) -> i32 {
        a * b
    }

    /// Get current timestamp in milliseconds
    pub fn now() -> i64 {
        use std::time::{SystemTime, UNIX_EPOCH};
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_millis() as i64
    }

    /// Generate a random float between 0 and 1
    pub fn random() -> f64 {
        use rand::Rng;
        rand::thread_rng().gen()
    }

    /// Test boolean conversion: returns 1 if input > 0, else 0
    pub fn is_positive(value: i32) -> i32 {
        if value > 0 { 1 } else { 0 }
    }

    /// String length
    pub fn strlen(s: &str) -> i32 {
        s.len() as i32
    }
}

/// Environment functions registered under the "env" namespace.
mod env_funcs {
    /// Print to stdout
    pub fn print(message: &str) {
        println!("{}", message);
    }

    /// Exit with code
    pub fn exit(code: i32) {
        std::process::exit(code);
    }

    /// Get environment variable
    pub fn getenv(name: &str) -> String {
        std::env::var(name).unwrap_or_default()
    }
}

/// Create a wasmtime Engine with WasmGC support enabled.
fn create_engine() -> Result<Engine> {
    let mut config = Config::new();
    // Enable WasmGC features
    config.wasm_gc(true);
    config.wasm_function_references(true);
    config.wasm_exceptions(true);
    Engine::new(&config)
}

/// Create a linker with all host functions registered.
fn create_linker(engine: &Engine) -> Result<Linker<()>> {
    let mut linker = Linker::new(engine);

    // Register host functions
    linker.func_wrap("host", "log", |caller: Caller<'_, ()>, ptr: i32, len: i32| {
        // Note: For full string support, we'd need linear memory access
        // For now, log the raw i31ref value as a placeholder
        println!("[Lisp] (i31ref value: {})", ptr);
    })?;

    linker.func_wrap("host", "add", |a: i32, b: i32| -> i32 {
        host::add(a, b)
    })?;

    linker.func_wrap("host", "sub", |a: i32, b: i32| -> i32 {
        host::sub(a, b)
    })?;

    linker.func_wrap("host", "mul", |a: i32, b: i32| -> i32 {
        host::mul(a, b)
    })?;

    linker.func_wrap("host", "now", || -> i64 {
        host::now()
    })?;

    linker.func_wrap("host", "random", || -> f64 {
        host::random()
    })?;

    linker.func_wrap("host", "isPositive", |value: i32| -> i32 {
        host::is_positive(value)
    })?;

    // Register env functions
    linker.func_wrap("env", "print", |caller: Caller<'_, ()>, ptr: i32, len: i32| {
        println!("(i31ref value: {})", ptr);
    })?;

    linker.func_wrap("env", "exit", |code: i32| {
        env_funcs::exit(code);
    })?;

    Ok(linker)
}

/// Load and instantiate a Wasm module with FFI support.
fn load_wasm_module(engine: &Engine, linker: &Linker<()>, wasm_bytes: &[u8]) -> Result<Instance> {
    let mut store = Store::new(engine, ());
    let module = Module::new(engine, wasm_bytes)?;
    let instance = linker.instantiate(&mut store, &module)?;
    Ok(instance)
}

/// Run the _start export of a Wasm module.
fn run_module(store: &mut Store<()>, instance: &Instance) -> Result<Option<i32>> {
    match instance.get_func(&mut *store, "_start") {
        Some(func) => {
            let mut results = vec![Val::I32(0)];
            func.call(&mut *store, &[], &mut results)?;
            match results.first() {
                Some(Val::I32(v)) => Ok(Some(*v)),
                _ => Ok(None),
            }
        }
        None => {
            eprintln!("Warning: Module has no _start export");
            Ok(None)
        }
    }
}

/// Call an exported function by name with i32 arguments.
fn call_export(store: &mut Store<()>, instance: &Instance, name: &str, args: &[i32]) -> Result<i32> {
    let func = instance
        .get_func(&mut *store, name)
        .ok_or_else(|| anyhow::anyhow!("Export '{}' not found", name))?;

    let args: Vec<Val> = args.iter().map(|&v| Val::I32(v)).collect();
    let mut results = vec![Val::I32(0)];
    func.call(&mut *store, &args, &mut results)?;

    match results.first() {
        Some(Val::I32(v)) => Ok(*v),
        _ => Err(anyhow::anyhow!("Expected i32 result")),
    }
}

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        println!("FFI Test Harness (wasmtime)");
        println!("===========================");
        println!();
        println!("Usage: {} <wasm-file> [function] [args...]", args[0]);
        println!();
        println!("Examples:");
        println!("  {} program.wasm              # Run _start", args[0]);
        println!("  {} program.wasm add 10 20    # Call add(10, 20)", args[0]);
        println!();
        println!("Available host functions (host.*):");
        println!("  log(message)      - Log message to console");
        println!("  add(a, b)         - Add two integers");
        println!("  sub(a, b)         - Subtract two integers");
        println!("  mul(a, b)         - Multiply two integers");
        println!("  now()             - Get current timestamp (ms)");
        println!("  random()          - Get random float [0, 1)");
        println!("  isPositive(n)     - Test if n > 0");
        println!();
        println!("Available env functions (env.*):");
        println!("  print(message)    - Print to stdout");
        println!("  exit(code)        - Exit with code");
        println!("  getenv(name)      - Get environment variable");
        return Ok(());
    }

    let wasm_file = &args[1];
    let wasm_bytes = fs::read(wasm_file)?;

    let engine = create_engine()?;
    let linker = create_linker(&engine)?;
    let mut store = Store::new(&engine, ());
    let module = Module::new(&engine, &wasm_bytes)?;
    let instance = linker.instantiate(&mut store, &module)?;

    if args.len() > 2 {
        // Call named export
        let func_name = &args[2];
        let int_args: Vec<i32> = args[3..]
            .iter()
            .filter_map(|s| s.parse().ok())
            .collect();

        let result = call_export(&mut store, &instance, func_name, &int_args)?;
        println!("{}", result);
    } else {
        // Run _start
        match run_module(&mut store, &instance)? {
            Some(result) => println!("{}", result),
            None => {}
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_host_add() {
        assert_eq!(host::add(10, 20), 30);
        assert_eq!(host::add(-5, 5), 0);
        assert_eq!(host::add(0, 0), 0);
    }

    #[test]
    fn test_host_sub() {
        assert_eq!(host::sub(20, 10), 10);
        assert_eq!(host::sub(5, 5), 0);
        assert_eq!(host::sub(0, 5), -5);
    }

    #[test]
    fn test_host_mul() {
        assert_eq!(host::mul(3, 4), 12);
        assert_eq!(host::mul(-2, 3), -6);
        assert_eq!(host::mul(0, 100), 0);
    }

    #[test]
    fn test_host_is_positive() {
        assert_eq!(host::is_positive(1), 1);
        assert_eq!(host::is_positive(100), 1);
        assert_eq!(host::is_positive(0), 0);
        assert_eq!(host::is_positive(-1), 0);
    }

    #[test]
    fn test_engine_creation() {
        let engine = create_engine();
        assert!(engine.is_ok(), "Engine creation should succeed with WasmGC");
    }

    #[test]
    fn test_linker_creation() {
        let engine = create_engine().unwrap();
        let linker = create_linker(&engine);
        assert!(linker.is_ok(), "Linker creation should succeed");
    }
}
