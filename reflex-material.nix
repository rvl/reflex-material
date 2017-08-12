{ mkDerivation, base, bimap, containers, data-default, lens, reflex
, reflex-dom, stdenv, text
}:
mkDerivation {
  pname = "reflex-material";
  version = "0.0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bimap containers data-default lens reflex-dom text
  ];
  executableHaskellDepends = [
    base containers lens reflex reflex-dom text
  ];
  testHaskellDepends = [ base ];
  homepage = "https://github.com/rvl/reflex-material#readme";
  license = stdenv.lib.licenses.bsd3;
}
