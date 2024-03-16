{
  description = "A Rofi plugin for searching Hoogle";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {self, nixpkgs, flake-utils }@inputs:
    flake-utils.lib.eachDefaultSystem(system:
      let
        pkgs = import nixpkgs {inherit system; };
        hs-hoogle-query = pkgs.haskellPackages.callPackage ./haskell {};
        rofi-hoogle = pkgs.callPackage ./rofi-hoogle-plugin/package.nix { inherit hs-hoogle-query; };
      in {
        packages.hs-hoogle-query = hs-hoogle-query;
        packages.rofi-hoogle = rofi-hoogle;
        defaultPackage = self.packages.${system}.rofi-hoogle;
      });
}
