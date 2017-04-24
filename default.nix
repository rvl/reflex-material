{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghcjs" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, reflex, reflex-dom, stdenv, text }:
      mkDerivation {
        pname = "reflex-material";
        version = "0.0.1.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [ base reflex-dom text ];
        executableHaskellDepends = [ base reflex reflex-dom text ];
        testHaskellDepends = [ base ];
        license = stdenv.lib.licenses.bsd3;
      };

  tryReflex = import (pkgs.fetchFromGitHub {
    owner = "reflex-frp";
    repo = "reflex-platform";
    rev = "970e6e678a5a54c2a8e8b90315169aa1079f166e";
    sha256 = "00sf3204mlcpgamyqrks5mhdl7b59p52krjgs5j5n0axwi4giy6f";
  }) {};

  # compiler could be ghcjs or ghc
  haskellPackages = tryReflex.${compiler};

  drv = haskellPackages.callPackage f {};

  nodePackages = (import ./mdc.nix { pkgs = nixpkgs; }).packages;

in

  if pkgs.lib.inNixShell then drv.env else drv
