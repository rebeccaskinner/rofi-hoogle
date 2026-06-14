{ mkDerivation, lib, ghc, base, bytestring, hoogle, containers, stm, text, html-entities }:

mkDerivation rec {
  pname = "rofi-hoogle-hs";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base bytestring hoogle containers stm text html-entities];
  license = lib.licenses.bsd3;
  postInstall = ''
    cp -av $out/lib/ghc-${ghc.version}/lib/* $out/lib;
    mkdir $out/include
    cp $src/csrc/rofi_hoogle_hs.h $out/include
    mkdir $out/lib/pkgconfig

    cat <<END > $out/lib/pkgconfig/rofiHoogleNative.pc
        prefix=$out
        exec_prefix=$${prefix}
        libdir=$${prefix}/lib
        includedir=$${prefix}/include

        Name: ${pname}
        Description: search hoogle
        Version: 0.1.0
        Cflags: -I$out/include
        Libs: -L$out/lib -lrofi-hoogle-native
END
  '';
}
