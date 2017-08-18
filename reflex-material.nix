{ mkDerivation, base, bimap, clay, containers, data-default, ghcjs-dom
, jsaddle, jsaddle-warp, lens, reflex, reflex-dom, reflex-dom-core
, stdenv, text, wai-app-static, warp, websockets
}:
mkDerivation {
  pname = "reflex-material";
  version = "0.0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bimap containers data-default ghcjs-dom lens reflex-dom
    reflex-dom-core text
  ];
  executableHaskellDepends = [
    base clay containers ghcjs-dom jsaddle jsaddle-warp lens reflex
    reflex-dom reflex-dom-core text wai-app-static warp websockets
  ];
  testHaskellDepends = [ base ];
  homepage = "https://github.com/rvl/reflex-material#readme";
  license = stdenv.lib.licenses.bsd3;
}
