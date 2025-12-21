{
  description = "Clysm - WebAssembly GC Common Lisp Compiler";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # Common Lisp dependencies for ASDF
        clDeps = with pkgs.lispPackages_new.sbclPackages; [
          alexandria
          babel
          rove
          trivial-gray-streams
        ];

        sbclWithDeps = pkgs.sbcl.withPackages (ps: clDeps);
      in {
        devShells.default = pkgs.mkShell {
          buildInputs = [
            sbclWithDeps
            pkgs.wasmtime
            pkgs.wasm-tools
            pkgs.wabt
          ];

          shellHook = ''
            echo "Clysm Development Environment"
            echo "=============================="
            echo "SBCL: $(sbcl --version)"
            echo "wasmtime: $(wasmtime --version)"
            echo "wasm-tools: $(wasm-tools --version)"
            echo ""
            echo "Load project: sbcl --load clysm.asd"
            echo "Run tests: sbcl --eval '(asdf:test-system :clysm)' --quit"
          '';
        };

        checks.default = pkgs.runCommand "clysm-check" {
          buildInputs = [ sbclWithDeps pkgs.wasm-tools ];
        } ''
          cd ${self}
          sbcl --non-interactive \
               --load clysm.asd \
               --eval "(asdf:load-system :clysm)" \
               --eval "(asdf:test-system :clysm)" \
               --eval "(uiop:quit 0)"
          touch $out
        '';

        packages.default = pkgs.stdenv.mkDerivation {
          pname = "clysm";
          version = "0.1.0";
          src = self;

          buildInputs = [ sbclWithDeps ];

          buildPhase = ''
            sbcl --non-interactive \
                 --load clysm.asd \
                 --eval "(asdf:load-system :clysm)"
          '';

          installPhase = ''
            mkdir -p $out/lib/clysm
            cp -r src/clysm/* $out/lib/clysm/
            cp clysm.asd $out/lib/clysm/
          '';
        };
      });
}
