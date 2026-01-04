{
  description = "Clysm - Common Lisp to WebAssembly GC Compiler";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShells.default = pkgs.mkShell {
          name = "clysm-dev";

          buildInputs = with pkgs; [
            # Common Lisp
            sbcl

            # WebAssembly tools
            wasmtime      # Wasm runtime for testing
            wabt          # wat2wasm, wasm2wat, wasm-validate
            binaryen      # wasm-opt, wasm-as, wasm-dis

            # Development utilities
            rlwrap        # For REPL readline support
          ];

          shellHook = ''
            echo "Clysm Development Environment"
            echo "=============================="
            echo "SBCL:     $(sbcl --version)"
            echo "Wasmtime: $(wasmtime --version)"
            echo "WABT:     $(wat2wasm --version 2>&1 | head -1)"
            echo ""
            echo "Commands:"
            echo "  sbcl              - Start SBCL REPL"
            echo "  rlwrap sbcl       - SBCL with readline"
            echo "  wat2wasm          - Convert WAT to WASM"
            echo "  wasm-validate     - Validate WASM binary"
            echo "  wasmtime          - Run WASM module"
            echo ""
          '';
        };

        # Package for distribution (future)
        packages.default = pkgs.stdenv.mkDerivation {
          pname = "clysm";
          version = "0.1.0";
          src = ./.;

          buildInputs = [ pkgs.sbcl ];

          buildPhase = ''
            # Build will be implemented later
            echo "Building Clysm..."
          '';

          installPhase = ''
            mkdir -p $out
            cp -r . $out/
          '';
        };
      });
}
