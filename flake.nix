{
  description = "CLYSM - Common Lisp for WebAssembly";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    llm-agents = {
      url = "github:numtide/llm-agents.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  nixConfig = {
    extra-substituters = [ "https://numtide.cachix.org" ];
    extra-trusted-public-keys = [ "numtide.cachix.org-1:2ps1kLBUWjxIneOy1Ber+6zc8G6uOybBIRXNz6mG8+E=" ];
  };

  outputs = { self, nixpkgs, llm-agents }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      devShells.${system}.default = pkgs.mkShell {
        packages = [
          # WebAssembly tools
          pkgs.wabt        # wat2wasm, wasm2wat, wasm-validate
          pkgs.wasm-tools  # Modern Wasm toolchain with GC support
          pkgs.binaryen    # wasm-opt

          # JavaScript runtime
          pkgs.nodejs_22   # Wasm GC support

          # Alternative runtime
          pkgs.deno

          # Spec-Driven Development
          llm-agents.packages.${system}.spec-kit
        ];

        shellHook = ''
          echo "CLYSM Development Environment"
          echo "  wasm-tools: $(wasm-tools --version)"
          echo "  node: $(node --version)"
          echo "  specify: available (run 'specify --help' for usage)"
        '';
      };
    };
}
