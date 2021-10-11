{ pkgs ? import <nixpkgs> {} }:
pkgs.stdenv.mkDerivation rec {
  pname = "rofi-hoogle-plugin";
  version = "0.0.1";
  src = ./src;

  nativeBuildInputs = with pkgs; [
    pkg-config
    glib
    libnotify
    makeWrapper
    rofi-unwrapped
    hs-hoogle-query
  ];

  buildInputs = with pkgs; [
    glib
    libnotify
    makeWrapper
    rofi-unwrapped
    cairo
    hs-hoogle-query
    xdg-utils
  ];

  installPhase = ''
    mkdir -p $out/lib
    make install -e INSTALL_ROOT=$out
  '';

  meta = with pkgs.lib; {
    description = "Search Hoogle from Rofi";
    homepage = "https://github.com/rebeccaskinner/rofi-hoogle";
    license = licenses.bsd3;
    platforms = platforms.linux;
  };

}
