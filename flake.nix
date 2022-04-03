{
  description = "A very basic flake";

  outputs = { self, nixpkgs }:  let
    system = "x86_64-linux";
    pkgs = import nixpkgs { overlays = [ self.overlay ]; inherit system; };
  in {

    packages.x86_64-linux = {
      inherit (pkgs) hs-hoogle-query rofi-hoogle;
    };

    devShell.x86_64-linux = self.packages.${system}.rofi-hoogle;
    overlay = final: prev: {
      hs-hoogle-query = final.haskellPackages.callPackage ./haskell {};
      rofi-hoogle =  final.callPackage ./rofi-hoogle-plugin/package.nix {};
    };
  } ;
}
