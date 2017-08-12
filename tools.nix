{ pkgs ? import <nixpkgs> {} }:

with pkgs;

stdenv.mkDerivation {
  name = "flatris";
  src = ./.;
  buildInputs = [
    haskellPackages.shake
    closurecompiler
    zopfli
  ];
}
