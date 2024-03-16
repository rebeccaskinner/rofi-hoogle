{
  description = "A Rofi plugin for searching Hoogle";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {self, nixpkgs, flake-utils }@inputs:
    flake-utils.lib.eachDefaultSystem(system:
      let
        hs-hoogle-query = nixpkgs.haskellPackages.callPackage ./haskell {};
        rofi-hoogle = nixpkgs.callPackage ./rofi-hoogle-plugin/package.nix { inherit hs-hoogle-query; };
      in {
        packages.hs-hoogle-query = hs-hoogle-query;
        packages.rofi-hoogle = rofi-hoogle;
        defaultPackage = self.apps.${system}.rofi-hoogle;
      });
}
