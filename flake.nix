{
  description = "A Rofi plugin for searching Hoogle";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    let
      overlay = final: prev: {
        hs-hoogle-query = final.haskellPackages.callPackage ./haskell/package.nix { };
        rofi-hoogle = final.callPackage ./rofi-hoogle-plugin/package.nix {
          inherit (final) hs-hoogle-query;
        };
      };
    in
    {
      overlays.default = overlay;
    }
    //
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ overlay ];
        };
      in
      {
        packages = {
          inherit (pkgs) hs-hoogle-query rofi-hoogle;
          default = pkgs.rofi-hoogle;
        };

        devShells.default = pkgs.mkShell {
          inputsFrom = [ pkgs.rofi-hoogle ];
          packages = with pkgs; [
            gcc
            gdb
            valgrind
            binutils
            strace
            ltrace
            xxd
            pkg-config
            rofi
            rofi-unwrapped
            cabal-install
            ghc
          ];
        };
      });
}
