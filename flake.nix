{
  description = "A Rofi plugin for searching Hoogle";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs =
    { self, nixpkgs }:
    let
      overlay = final: prev: {
        hs-hoogle-query = final.haskellPackages.callPackage ./haskell/package.nix { };
        rofi-hoogle = final.callPackage ./rofi-hoogle-plugin/package.nix { };
      };

      # rofi is Linux-only
      systems = [
        "x86_64-linux"
        "aarch64-linux"
      ];

      forAllSystems =
        f:
        nixpkgs.lib.genAttrs systems (
          system:
          f (
            import nixpkgs {
              inherit system;
              overlays = [ overlay ];
            }
          )
        );
    in
    {
      overlays.default = overlay;

      packages = forAllSystems (pkgs: {
        inherit (pkgs) hs-hoogle-query rofi-hoogle;
        default = pkgs.rofi-hoogle;
      });

      checks = forAllSystems (pkgs: {
        inherit (pkgs) hs-hoogle-query rofi-hoogle;
      });

      devShells = forAllSystems (pkgs: {
        default = pkgs.haskellPackages.shellFor {
          packages = _: [ pkgs.hs-hoogle-query ];
          # Leave out the built Haskell library so a compile error in it
          # doesn't prevent entering the shell to fix it.
          buildInputs = pkgs.lib.remove pkgs.hs-hoogle-query pkgs.rofi-hoogle.buildInputs;
          nativeBuildInputs =
            pkgs.rofi-hoogle.nativeBuildInputs
            ++ (with pkgs; [
              cabal-install
              haskellPackages.haskell-language-server
              gdb
              valgrind
              strace
              ltrace
              xxd
              rofi
            ]);
        };
      });

      formatter = forAllSystems (pkgs: pkgs.nixfmt);
    };
}
