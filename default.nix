{ pkgs ? reflex-platform.nixpkgs
, reflex-platform ? import reflex-platform-src {}
, reflex-platform-src ? ./reflex-platform
}:

let
  # Add this project to the haskell package set
  project = self: super: {
    reflex-material = self.callPackage ./reflex-material.nix {};
  };

  # GHCJS compile fixes
  buildFixes = self: super: with pkgs.haskell.lib; {
    Glob = dontCheck super.Glob; # tests failing on ghcjs
    SHA1 = dontCheck super.SHA1; # tests failing on ghcjs
    http-date = doJailbreak (dontCheck super.http-date); # doctest
    iproute = doJailbreak (dontCheck super.iproute); # doctest
    unix-time = doJailbreak (dontCheck super.unix-time); # doctest
    bsb-http-chunked = doJailbreak (dontCheck super.unix-time); # doctest
  };

  ghc = reflex-platform.ghc.extend project;
  ghcjs = (reflex-platform.ghcjs.extend project).extend buildFixes;

  mkShell = haskellPackages: addNodeModules (haskellPackages.shellFor {
    packages = p: [ p.reflex-material ];
    withHoogle = true;
  });

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
