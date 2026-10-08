{
  lib,
  stdenv,
  pkg-config,
  glib,
  cairo,
  rofi-unwrapped,
  hs-hoogle-query,
}:

stdenv.mkDerivation {
  pname = "rofi-hoogle-plugin";
  version = "0.0.1";
  src = lib.fileset.toSource {
    root = ./src;
    fileset = lib.fileset.unions [
      ./src/Makefile
      ./src/plugin.c
    ];
  };

  nativeBuildInputs = [ pkg-config ];

  buildInputs = [
    glib
    cairo
    rofi-unwrapped
    hs-hoogle-query
  ];

  makeFlags = [ "INSTALL_ROOT=${placeholder "out"}" ];

  preInstall = ''
    mkdir -p $out/lib/rofi
  '';

  meta = {
    description = "Search Hoogle from Rofi";
    homepage = "https://github.com/rebeccaskinner/rofi-hoogle";
    license = lib.licenses.bsd3;
    platforms = lib.platforms.linux;
  };
}
