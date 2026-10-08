{
  mkDerivation,
  lib,
  base,
  bytestring,
  containers,
  hoogle,
  html-entities,
  stm,
  text,
  unordered-containers,
}:

mkDerivation {
  pname = "rofi-hoogle-hs";
  version = "0.1.0.0";
  src = lib.fileset.toSource {
    root = ./.;
    fileset = lib.fileset.unions [
      ./rofi-hoogle.cabal
      ./CHANGELOG.md
      ./LICENSE
      ./src
      ./csrc
    ];
  };
  libraryHaskellDepends = [
    base
    bytestring
    containers
    hoogle
    html-entities
    stm
    text
    unordered-containers
  ];
  license = lib.licenses.bsd3;
  postInstall = ''
    # cabal installs foreign libraries under lib/ghc-<version>/lib; expose
    # them in lib/ so the plugin can link against them via pkg-config.
    ln -s $out/lib/ghc-*/lib/librofi-hoogle-native.so* $out/lib/
    install -Dm644 csrc/rofi_hoogle_hs.h $out/include/rofi_hoogle_hs.h

    mkdir -p $out/lib/pkgconfig
    cat > $out/lib/pkgconfig/rofiHoogleNative.pc <<EOF
    prefix=$out
    libdir=\''${prefix}/lib
    includedir=\''${prefix}/include

    Name: rofiHoogleNative
    Description: search hoogle
    Version: 0.1.0.0
    Cflags: -I\''${includedir}
    Libs: -L\''${libdir} -lrofi-hoogle-native
    EOF
  '';
}
