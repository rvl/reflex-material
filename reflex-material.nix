{ mkDerivation, base, bimap, bytestring, clay, containers
, data-default, ghcjs-dom, jsaddle, jsaddle-dom, jsaddle-warp, lens
, reflex, reflex-dom, reflex-dom-core, say, stdenv, text, wai
, wai-app-static, warp, websockets
, ghc, lib
}:
mkDerivation {
  pname = "reflex-material";
  version = "0.0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bimap containers data-default ghcjs-dom jsaddle jsaddle-dom
    lens reflex-dom reflex-dom-core say text
  ];
  executableHaskellDepends = [
    base bytestring clay containers ghcjs-dom jsaddle lens
    reflex reflex-dom reflex-dom-core text
  ] ++ lib.optionals (!(ghc.isGhcjs or false)) [ jsaddle-warp wai wai-app-static warp websockets ];
  homepage = "https://github.com/rvl/reflex-material#readme";
  license = stdenv.lib.licenses.bsd3;
}
