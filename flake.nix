{
  description = "Clysm - WebAssembly GC Common Lisp Compiler";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    # ANSI Common Lisp test suite (pinned to specific commit)
    ansi-test = {
      url = "github:pfdietz/ansi-test/6e3f70002559d56d3e4a6f0b8ddcc083d202f066";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, ansi-test }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in {
        devShells.default = pkgs.mkShell {
          buildInputs = [
            pkgs.sbcl
            pkgs.wasmtime
            pkgs.wasm-tools
            pkgs.wabt
            pkgs.nodejs  # For host-shim ANSI test execution
          ];

          shellHook = ''
            echo "Clysm Development Environment"
            echo "=============================="
            echo "SBCL: $(sbcl --version)"
            echo "wasmtime: $(wasmtime --version)"
            echo "wasm-tools: $(wasm-tools --version)"
            echo ""
            echo "Setup Quicklisp (first time only):"
            echo "  sbcl --load ~/quicklisp/setup.lisp"
            echo ""
            echo "Load project:"
            echo "  sbcl --eval '(ql:quickload :clysm)'"
            echo ""
            echo "Run tests:"
            echo "  sbcl --eval '(asdf:test-system :clysm)' --quit"

            # Add current directory to ASDF search path
            export CL_SOURCE_REGISTRY="(:source-registry (:directory \"$PWD\") :inherit-configuration)"
          '';
        };

        checks.default = pkgs.runCommand "clysm-check" {
          buildInputs = [ pkgs.sbcl pkgs.wasm-tools pkgs.wasmtime pkgs.wabt pkgs.nodejs ];
        } ''
          cd ${self}

          # Phase 1: Verify source files exist
          echo "==> Checking source files..."
          test -f clysm.asd
          test -f src/clysm/package.lisp
          test -f src/clysm/backend/leb128.lisp
          test -f src/clysm/backend/wasm-emit.lisp
          test -f src/clysm/backend/sections.lisp
          echo "    Source files OK"

          # Phase 2: Verify interpreter bootstrap files exist (Feature 044)
          echo "==> Checking interpreter bootstrap files..."
          test -f src/clysm/eval/interpreter.lisp
          test -f src/clysm/eval/interpreter-macros.lisp
          test -f src/clysm/eval/interpreter-builtins.lisp
          test -f src/clysm/eval/interpreter-file.lisp
          test -f src/clysm/bootstrap/package.lisp
          test -f src/clysm/bootstrap/interpreter-stage0.lisp
          test -f src/clysm/bootstrap/fixpoint.lisp
          echo "    Interpreter bootstrap files OK"

          # Phase 3: Verify tools are available
          echo "==> Checking tool availability..."
          sbcl --version
          wasm-tools --version
          wasmtime --version
          wat2wasm --version
          node --version
          echo "    Tools OK"

          # Phase 4: Validate empty Wasm module format
          echo "==> Validating Wasm module format..."
          printf '\x00\x61\x73\x6d\x01\x00\x00\x00' > /tmp/empty.wasm
          wasm-tools validate /tmp/empty.wasm
          echo "    Wasm validation OK"

          # Phase 5: Verify interpreter test files exist
          echo "==> Checking interpreter test files..."
          test -f tests/unit/interpreter/defun-test.lisp
          test -f tests/unit/interpreter/defmacro-test.lisp
          test -f tests/unit/interpreter/defstruct-test.lisp
          test -f tests/unit/interpreter/builtins-test.lisp
          test -f tests/contract/interpreter-compile-test.lisp
          test -f tests/integration/bootstrap-fixpoint-test.lisp
          test -f tests/integration/sbcl-free-test.lisp
          echo "    Interpreter test files OK"

          # Phase 6: Verify scripts exist
          echo "==> Checking interpreter scripts..."
          test -f scripts/verify-fixpoint-interp.sh
          test -f scripts/bootstrap-without-sbcl.sh
          test -f scripts/run-tests-via-interpreter.sh
          test -f scripts/gen-stage0-interp.sh
          echo "    Interpreter scripts OK"

          touch $out
        '';

        packages.default = pkgs.stdenv.mkDerivation {
          pname = "clysm";
          version = "0.1.0";
          src = self;

          buildInputs = [ pkgs.sbcl ];

          installPhase = ''
            mkdir -p $out/lib/clysm
            cp -r src/clysm/* $out/lib/clysm/
            cp clysm.asd $out/lib/clysm/
          '';
        };
      });
}
