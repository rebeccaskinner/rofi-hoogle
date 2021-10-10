let
  hs-hoogle-overlay = self: super: {
    hs-hoogle-query = super.haskellPackages.callPackage ./haskell {};
  };

  pkgs = import <nixpkgs> { overlays = [hs-hoogle-overlay]; };

  hs-hoogle-query = pkgs.haskellPackages.callPackage ./haskell {};
  rofi-hoogle = pkgs.callPackage ./rofi-hoogle-plugin/package.nix {};

  cDevelopmentTools = with pkgs; [
    gcc
    gdb
    valgrind
    binutils
    strace
    ltrace
    xxd
    pkg-config
  ];

  testTools = with pkgs; [
    rofi
    rofi-unwrapped
  ];

  devShell = pkgs.mkShell {
    buildInputs = [ rofi-hoogle.buildInputs rofi-hoogle.nativeBuildInputs cDevelopmentTools testTools];
  };

in { inherit rofi-hoogle; }
