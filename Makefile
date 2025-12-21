.PHONY: build test clean validate

WAT_SRC := src/kernel/kernel.wat
WASM_OUT := build/kernel.wasm

# Build the WebAssembly module using wasm-tools
build: $(WASM_OUT)

$(WASM_OUT): $(WAT_SRC)
	@mkdir -p build
	wasm-tools parse $(WAT_SRC) -o $(WASM_OUT)

# Run tests
test: build
	node --test test/*.test.js

# Clean build artifacts
clean:
	rm -rf build/

# Validate WAT syntax without building
validate:
	wasm-tools parse $(WAT_SRC) --output=/dev/null

# Print wasm module info
info: build
	wasm-tools print $(WASM_OUT) | head -50
