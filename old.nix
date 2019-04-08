{ pkgs ? reflex-platform.nixpkgs
, reflex-platform ? import reflex-platform-src {}
, reflex-platform-src ? ./reflex-platform
}:

let
  # Add this project to the haskell package set
  project = self: super: {
    reflex-material = self.callPackage ./reflex-material.nix {
      src = pkgs.lib.cleanSource ./.;
    };
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


in {
  inherit ghc ghcjs;

  shells = {
    ghc = mkShell ghc;
    ghcjs = mkShell ghcjs;
  };

  inherit reflex-platform pkgs;
  inherit nodePackages;
}
