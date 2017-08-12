# --argstr compiler could be ghcjs or ghc
{ nixpkgs ? import <nixpkgs> {}
, compiler ? "ghc"
}:
let

  inherit (nixpkgs) pkgs;

  tryReflex = import (pkgs.fetchFromGitHub {
    owner = "reflex-frp";
    repo = "reflex-platform";
    rev = "b7c00b3574d0ef42974eda0f2812c794c7b5d4f3";
    sha256 = "1jfz17y2fq051caby4y4aslxrpvgwwa30ivfw0l5wn5pp5zlrpad";
  }) {};

  # compiler could be ghcjs or ghc
  haskellPackages = tryReflex.${compiler};

  drv = haskellPackages.callPackage ./reflex-material.nix {};

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
