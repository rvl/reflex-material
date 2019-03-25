{ pkgs ? reflex-platform.nixpkgs
, reflex-platform ? import reflex-platform-src {}
, reflex-platform-src ? ./reflex-platform
}:

let
  overlay = self: super: {
    reflex-material = self.callPackage ./reflex-material.nix {};
  };

  ghc = reflex-platform.ghc.extend overlay;
  ghcjs = reflex-platform.ghcjs.extend overlay;

  mkShell = haskellPackages: haskellPackages.shellFor {
    packages = p: [ p.reflex-material ];
    withHoogle = true;
  };

  nodePackages = (import ./mdc.nix { inherit pkgs; }).package.override {
    src = pkgs.lib.cleanSourceWith {
      src = ./.;
      filter = name: _: baseNameOf name == "package.json";
    };
  };

  addNodeModules = shellDrv: shellDrv.overrideAttrs (oldAttrs: {
    # Provide NODE_MODULES in the nix-shell.
    # This will be used by the shake build script.
    shellHook = (oldAttrs.shellHook or "") + ''
      export NODE_MODULES=${nodePackages}/lib/node_modules/reflex-material/node_modules
    '';
  });

in {
  inherit ghc ghcjs;

  shells = {
    ghc = mkShell ghc;
    ghcjs = mkShell ghcjs;
  };

  inherit reflex-platform pkgs;
  inherit nodePackages;
}
