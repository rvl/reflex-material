{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghcjs" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, reflex, reflex-dom, stdenv, text, bimap }:
      mkDerivation {
        pname = "reflex-material";
        version = "0.0.1.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [ base reflex-dom text bimap ];
        executableHaskellDepends = [ base reflex reflex-dom text bimap ];
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

  nodePackages = (import ./mdc.nix { pkgs = nixpkgs; }).package;

  # Provide NODE_MODULES in the nix-shell.
  # This will be used by the shake build script.
  env' = pkgs.lib.overrideDerivation drv.env (attrs: {
    shellHook = attrs.shellHook + ''
      export NODE_MODULES=${nodePackages}/lib/node_modules/reflex-material/node_modules
    '';
  });

in

  if pkgs.lib.inNixShell then env' else drv
