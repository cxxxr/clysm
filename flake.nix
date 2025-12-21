{
  description = "CLYSM - Common Lisp for WebAssembly";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      devShells.${system}.default = pkgs.mkShell {
        packages = with pkgs; [
          # WebAssembly tools
          wabt        # wat2wasm, wasm2wat, wasm-validate
          wasm-tools  # Modern Wasm toolchain with GC support
          binaryen    # wasm-opt

          # JavaScript runtime
          nodejs_22   # Wasm GC support

          # Alternative runtime
          deno
        ];

        shellHook = ''
          echo "CLYSM Development Environment"
          echo "  wasm-tools: $(wasm-tools --version)"
          echo "  node: $(node --version)"
        '';
      };
    };
}
