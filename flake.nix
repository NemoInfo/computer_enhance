{
  description = "Computer Enhance Rust Flake";

  inputs = { nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable"; };

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      devShells.${system}.default = pkgs.mkShell {
        buildInputs = with pkgs; [ nasm cargo-expand ];

        shellHook = ''
          THEME="af-magic" exec $SHELL
        '';
      };
    };
}
