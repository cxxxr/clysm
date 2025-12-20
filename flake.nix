{
  description = "clysm - Common Lisp to WebAssembly compiler";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # Quicklisp setup script
        quicklispSetup = pkgs.writeShellScript "quicklisp-setup" ''
          QUICKLISP_DIR="$HOME/.quicklisp"
          if [ ! -f "$QUICKLISP_DIR/setup.lisp" ]; then
            echo "Installing Quicklisp..."
            mkdir -p "$QUICKLISP_DIR"
            ${pkgs.curl}/bin/curl -o /tmp/quicklisp.lisp https://beta.quicklisp.org/quicklisp.lisp
            ${pkgs.sbcl}/bin/sbcl --non-interactive \
              --load /tmp/quicklisp.lisp \
              --eval "(quicklisp-quickstart:install :path \"$QUICKLISP_DIR\")" \
              --eval "(ql:add-to-init-file)"
            rm /tmp/quicklisp.lisp
            echo "Quicklisp installed to $QUICKLISP_DIR"
          fi
        '';

        # Load clysm script
        loadScript = pkgs.writeShellScript "clysm-load" ''
          ${quicklispSetup}
          exec ${pkgs.sbcl}/bin/sbcl \
            --load "$HOME/.quicklisp/setup.lisp" \
            --eval "(push #p\"$PWD/\" asdf:*central-registry*)" \
            --eval "(ql:quickload :clysm)" \
            "$@"
        '';

        # Run tests script
        testScript = pkgs.writeShellScript "clysm-test" ''
          ${quicklispSetup}
          exec ${pkgs.sbcl}/bin/sbcl --non-interactive \
            --load "$HOME/.quicklisp/setup.lisp" \
            --eval "(push #p\"$PWD/\" asdf:*central-registry*)" \
            --eval "(ql:quickload :clysm/tests)" \
            --eval "(let ((results (fiveam:run :clysm))) (if (fiveam:results-status results) (sb-ext:exit :code 0) (sb-ext:exit :code 1)))"
        '';

        # Compile to WASM script
        compileScript = pkgs.writeShellScript "clysm-compile" ''
          ${quicklispSetup}
          INPUT="$1"
          OUTPUT="''${2:-output.wasm}"
          if [ -z "$INPUT" ]; then
            echo "Usage: clysm-compile <input.lisp> [output.wasm]"
            exit 1
          fi
          ${pkgs.sbcl}/bin/sbcl --non-interactive \
            --load "$HOME/.quicklisp/setup.lisp" \
            --eval "(push #p\"$PWD/\" asdf:*central-registry*)" \
            --eval "(ql:quickload :clysm)" \
            --eval "(let* ((forms (clysm/reader:read-file \"$INPUT\"))
                          (module (clysm/compiler:compile-module forms))
                          (bytes (clysm/wasm:encode-module module)))
                     (with-open-file (out \"$OUTPUT\" :direction :output
                                          :if-exists :supersede
                                          :element-type '(unsigned-byte 8))
                       (write-sequence bytes out))
                     (format t \"Compiled ~A -> ~A (~A bytes)~%\"
                             \"$INPUT\" \"$OUTPUT\" (length bytes)))"
        '';

      in {
        devShells.default = pkgs.mkShell {
          buildInputs = [
            pkgs.sbcl
            pkgs.curl
            pkgs.nodejs  # For testing WASM output
          ];

          shellHook = ''
            echo "clysm development environment"
            echo ""
            echo "Commands:"
            echo "  clysm-load    - Start SBCL with clysm loaded"
            echo "  clysm-test    - Run test suite"
            echo "  clysm-compile - Compile Lisp to WASM"
            echo ""

            # Ensure Quicklisp is installed
            ${quicklispSetup}

            export PATH="${pkgs.lib.makeBinPath [ pkgs.sbcl pkgs.nodejs ]}:$PATH"

            # Create wrapper scripts in current directory
            mkdir -p .bin
            ln -sf ${loadScript} .bin/clysm-load
            ln -sf ${testScript} .bin/clysm-test
            ln -sf ${compileScript} .bin/clysm-compile
            export PATH="$PWD/.bin:$PATH"
          '';
        };

        packages.default = pkgs.stdenv.mkDerivation {
          pname = "clysm";
          version = "0.1.0";
          src = ./.;

          buildInputs = [ pkgs.sbcl ];

          # This is a source-only package for now
          dontBuild = true;

          installPhase = ''
            mkdir -p $out/share/common-lisp/source/clysm
            cp -r . $out/share/common-lisp/source/clysm/
          '';
        };
      }
    );
}
