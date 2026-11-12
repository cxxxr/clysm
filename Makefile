# Clysm — Common Lisp on WebAssembly GC
.PHONY: all test test-all test-asdf repl selfhost fixpoint web ansi clean

all: build/clysm-native.wasm

build/clysm-native.wasm:
	sbcl --script tools/build-repl.lisp $@

repl: all
	node host/run.mjs build/clysm-native.wasm

test:
	tools/run-tests.sh

# load a small system with the bundled ASDF (third_party/asdf)
test-asdf:
	bin/clysm script tests/asdf/run.lisp > build/asdf-test.out 2>&1; \
	if cmp -s build/asdf-test.out tests/asdf/run.expected; then echo "asdf: ok"; \
	else diff tests/asdf/run.expected build/asdf-test.out; exit 1; fi

selfhost:
	tools/build-selfhost.sh

SRCS = src/package.lisp src/wasm.lisp src/reader.lisp src/layout.lisp src/frontend.lisp \
       src/codegen.lisp src/prims.lisp src/driver.lisp src/hostenv.lisp src/selfhost.lisp

# stage 2 and stage 3 compilers (built by the Wasm-hosted compiler) must be identical
fixpoint: selfhost
	node host/run.mjs build/clysmc.wasm build/clysmc2.wasm $(SRCS)
	node host/run.mjs build/clysmc2.wasm build/clysmc3.wasm $(SRCS)
	cmp build/clysmc2.wasm build/clysmc3.wasm && echo "fixed point reached"

test-all: test test-asdf selfhost
	INTERP=1 tools/run-tests.sh
	NATIVE=1 tools/run-tests.sh
	SELFHOST=1 tools/run-tests.sh

web:
	tools/build-web.sh

# the ANSI test suite (third_party/ansi-test); results in build/ansi/
ansi:
	git submodule update --init third_party/ansi-test
	tests/ansi/run.sh

clean:
	rm -rf build
