{ mkDerivation, base, bimap, clay, containers, data-default
, ghcjs-dom, jsaddle, jsaddle-warp, lens, reflex, reflex-dom
, reflex-dom-core, stdenv, text, wai, wai-app-static, warp
, websockets, ghcjs-base
, ghc, lib
}:
mkDerivation {
  pname = "reflex-material";
  version = "0.0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bimap containers data-default ghcjs-dom lens reflex-dom
    reflex-dom-core text ghcjs-base jsaddle
  ];
  executableHaskellDepends = [
    base clay containers ghcjs-dom jsaddle  lens reflex
    reflex-dom reflex-dom-core text
  ] ++ lib.optionals (!(ghc.isGhcjs or false)) [ jsaddle-warp wai wai-app-static warp websockets ];
  homepage = "https://github.com/rvl/reflex-material#readme";
  license = stdenv.lib.licenses.bsd3;
}
