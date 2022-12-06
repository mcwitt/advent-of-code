{
  description = "Haskell project";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        haskellPackages = pkgs.haskell.packages.ghc92;
        packageName = "my-package";
      in
      {
        packages = {
          ${packageName} = haskellPackages.callCabal2nix packageName ./. { };
          default = self.outputs.packages.${system}.${packageName};
        };

        devShell = haskellPackages.shellFor {
          packages = _: [ self.outputs.packages.${system}.${packageName} ];
          withHoogle = true;
          buildInputs = with pkgs; [
            cabal-install
            ghcid
            haskellPackages.haskell-language-server
            haskellPackages.cabal-fmt
            ormolu
          ];
        };
      });
}
