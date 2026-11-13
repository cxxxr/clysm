# Clysm — Common Lisp on WebAssembly GC
.PHONY: all test test-all test-lib test-ui test-lem test-asdf test-quicklisp test-mcclim repl selfhost fixpoint web ansi clean

all: build/clysm-native.wasm

build/clysm-native.wasm:
	sbcl --script tools/build-repl.lisp $@

repl: all
	node host/run.mjs build/clysm-native.wasm

test:
	tools/run-tests.sh

# load a small system with the bundled ASDF (third_party/asdf)
# install Quicklisp from scratch into build/quicklisp-test and load libraries
# (downloads from quicklisp.org)
test-quicklisp:
	rm -rf build/quicklisp-test && mkdir -p build/quicklisp-test
	curl -sfL -o build/quicklisp-test/quicklisp.lisp https://beta.quicklisp.org/quicklisp.lisp
	bin/clysm script tests/quicklisp/install.lisp build/quicklisp-test/quicklisp.lisp build/quicklisp-test/home/ \
	  | tee build/quicklisp-test/log.txt | tail -2
	grep -q 'split-sequence: ("a" "b" "" "c")' build/quicklisp-test/log.txt && \
	grep -q 'str: ("x" "y")' build/quicklisp-test/log.txt && echo "quicklisp: ok"

# McCLIM (from an existing Quicklisp installation) on a virtual X server
test-mcclim:
	tests/mcclim/run.sh

test-ui:
	tools/test-ui.sh

# a program compiled as a library (bin/clysm library) and called from JavaScript
test-lib:
	tests/lib/run.sh

# Lem (contrib/lem) in Electron, typed into without a display
test-lem:
	tests/lem/run.sh

test-asdf:
	bin/clysm script tests/asdf/run.lisp > build/asdf-test.out 2> build/asdf-test.err; \
	if cmp -s build/asdf-test.out tests/asdf/run.expected; then echo "asdf: ok"; \
	else diff tests/asdf/run.expected build/asdf-test.out; exit 1; fi

selfhost:
	tools/build-selfhost.sh

SRCS = src/package.lisp src/wasm.lisp src/reader.lisp src/layout.lisp src/frontend.lisp \
       src/types.lisp src/inline.lisp src/codegen.lisp src/prims.lisp src/typed.lisp src/driver.lisp src/hostenv.lisp src/selfhost.lisp

# stage 2 and stage 3 compilers (built by the Wasm-hosted compiler) must be identical
fixpoint: selfhost
	node host/run.mjs build/clysmc.wasm build/clysmc2.wasm $(SRCS)
	node host/run.mjs build/clysmc2.wasm build/clysmc3.wasm $(SRCS)
	cmp build/clysmc2.wasm build/clysmc3.wasm && echo "fixed point reached"

test-all: test test-lib test-asdf selfhost
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
