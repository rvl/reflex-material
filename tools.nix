{ pkgs ? import <nixpkgs> {} }:

with pkgs;

mkShell {
  name = "reflex-material-tools";
  nativeBuildInputs = [
    haskellPackages.shake
    closurecompiler
    zopfli
  ];
}
