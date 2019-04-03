let
  proj = import ./default.nix {};
in
  proj.shells.ghc
