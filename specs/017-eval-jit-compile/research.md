# Research: Eval/JIT Compile System

**Branch**: `017-eval-jit-compile` | **Date**: 2025-12-24

## Research Topics

### 1. Tiered JIT Compilation Strategy

**Decision**: Two-tier architecture with Tier 1 (interpreter) and Tier 2 (Wasm JIT)

**Rationale**:
- Tier 1 provides immediate execution with zero compilation overhead
- Tier 2 provides optimized execution for hot code paths
- Matches the constitution's "段階的動的コンパイル" principle (Principle VI)
- Similar to V8's Ignition (interpreter) → TurboFan (optimizing) model

**Alternatives Considered**:
- Single-tier interpreter only: Rejected - misses optimization opportunity for hot code
- Three-tier (interpreter → baseline JIT → optimizing JIT): Rejected - unnecessary complexity for initial implementation
- Direct Wasm compilation only: Rejected - high startup latency for simple expressions

### 2. Hot Spot Detection Mechanism

**Decision**: Invocation counter per function with configurable threshold (default: 10)

**Rationale**:
- Simple and predictable behavior
- Low overhead (single increment per call)
- Configurable threshold allows tuning for different workloads
- Existing infrastructure in `compile.lisp` already has `*invocation-counts*` hash table

**Alternatives Considered**:
- Sample-based profiling: Rejected - adds complexity, less predictable
- Loop iteration counting: Rejected - requires bytecode analysis, not applicable to interpreter
- Time-based sampling: Rejected - platform-dependent, non-deterministic

### 3. Tier 2 Compilation Failure Handling

**Decision**: Graceful degradation - remain in Tier 1 silently

**Rationale**:
- Maintains correctness (Tier 1 execution is always correct)
- No disruption to user code
- Standard practice in production JITs (V8, HotSpot)
- Future enhancement can add optional warning mode

**Alternatives Considered**:
- Error on promotion failure: Rejected - disrupts otherwise working code
- Warning + Tier 1: Possible future enhancement, but verbose for normal operation
- Mark "never promote" + log: Possible optimization to avoid repeated attempts

### 4. Anonymous Function Tracking

**Decision**: Use identity-based tracking (eq hash table on function object)

**Rationale**:
- Anonymous functions have no symbol name, but have stable object identity
- Wrapper functions maintain reference to original lambda for potential re-compilation
- Hash table lookup by `eq` is O(1)

**Alternatives Considered**:
- No tracking for anonymous functions: Rejected - misses optimization for anonymous closures passed to map/reduce
- Lambda expression hashing: Rejected - complex, potential for collisions

### 5. Hot-Patching During Execution Safety

**Decision**: Direct slot replacement with single-threaded guarantee

**Rationale**:
- Constitution specifies single-threaded runtime (Assumptions section)
- No concurrent execution means no race conditions
- Function slot update is atomic from Lisp perspective
- In-flight calls complete with old function; new calls use new function

**Alternatives Considered**:
- On-stack replacement (OSR): Rejected - extremely complex, not needed for single-threaded
- Trampoline-based dispatch: Rejected - adds indirection overhead
- Deferred patching at safe points: Rejected - unnecessary for single-threaded

### 6. Closure Environment Handling in Tier 2

**Decision**: Capture environment at compile time, embed in generated Wasm

**Rationale**:
- Closures already capture environment in `interpreted-closure` struct
- Tier 2 compilation can serialize captured values into Wasm globals
- Maintains semantic equivalence with Tier 1 interpretation

**Alternatives Considered**:
- Re-capture at promotion time: Rejected - environment may have changed
- Environment as import parameter: Possible alternative, but more complex

### 7. Runtime Import Table Design

**Decision**: Use existing `*runtime-imports*` hash table with string keys

**Rationale**:
- Already implemented in `jit.lisp`
- Matches Wasm import naming conventions
- Extensible for new runtime functions

**Alternatives Considered**:
- Numeric indices: Rejected - less maintainable
- Symbol-based keys: Possible, but Wasm uses string names

## Existing Infrastructure Analysis

### eval/interpreter.lisp (Tier 1)
- **Status**: Fully functional
- **Special forms supported**: quote, if, progn, let, let*, lambda, setq, block, return-from, tagbody, go, flet, labels, funcall
- **Environment**: `interpreter-env` struct with parent chain
- **Closures**: `interpreted-closure` struct captures params, body, env
- **Built-ins**: Arithmetic, comparisons, list operations, type predicates

### eval/jit.lisp (Tier 2)
- **Status**: Foundation exists, needs integration
- **`generate-wasm`**: Delegates to `compile-to-wasm`
- **`validate-wasm`**: Checks magic bytes and version
- **`instantiate-wasm`**: Creates mock instance (needs real implementation)
- **`jit-compile`**: Main entry point, compiles and instantiates
- **`*runtime-imports*`**: Hash table for runtime functions
- **`*function-slots*`**: Symbol → function mapping for hot-patching

### eval/compile.lisp
- **Status**: Skeleton exists, needs completion
- **`compile*`**: Currently just calls interpreter
- **`*compilation-threshold*`**: 10 (default)
- **`*invocation-counts*`**: Hash table for tracking
- **`should-promote-to-tier-2-p`**: Check if count exceeds threshold
- **`record-invocation`**: Increment counter
- **`promote-to-tier-2`**: JIT compile and hot-patch
- **`compile-with-tier`**: Explicit tier selection

## Implementation Gaps

1. **Wrapper function generation**: `compile*` needs to return a wrapper that:
   - Tracks invocations
   - Triggers promotion at threshold
   - Delegates to current implementation (Tier 1 or Tier 2)

2. **Named function registration**: When name is provided, register in function slot

3. **Wasm instantiation**: Current `instantiate-wasm` returns mock function; needs to:
   - Extract actual function from compiled Wasm module
   - In host mode (SBCL), simulate by interpreting the lambda

4. **Integration testing**: Expand `jit-test.lisp` to cover tier promotion scenarios
