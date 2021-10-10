{ pkgs ? import <nixpkgs> {} }:

let
  rofi-hoogle = pkgs.callPackage ./package.nix {};
in

pkgs.mkShell {
  inputsFrom = [ rofi-hoogle.env ];
}
