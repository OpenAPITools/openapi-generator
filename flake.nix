{
  description = "OpenAPI generator nix flake";

  inputs.nixpkgs.url = "github:nixos/nixpkgs";  
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
      {
        devShells.default = pkgs.mkShell
          {
            buildInputs = with pkgs;[
              jdk11
              maven
            ];
          };
      }
    );
}

