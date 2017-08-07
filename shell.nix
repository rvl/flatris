# --argstr compiler could be ghcjs or ghc
{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc" }:

let

  inherit (nixpkgs) pkgs;

  tryReflex = import (pkgs.fetchFromGitHub {
    owner = "reflex-frp";
    repo = "reflex-platform";
    rev = "b7c00b3574d0ef42974eda0f2812c794c7b5d4f3";
    sha256 = "1jfz17y2fq051caby4y4aslxrpvgwwa30ivfw0l5wn5pp5zlrpad";
  }) {};

  # compiler could be ghcjs or ghc
  reflex-platform = tryReflex.${compiler};

  # overrides for reflex version compatibility
  haskellPackages = reflex-platform.override {
    overrides = self: super: {
      # diagrams-reflex = self.callPackage (pkgs.fetchFromGitHub {
      #   owner = "diagrams";
      #   repo = "diagrams-reflex";
      #   rev = "f1ba4f2f8767ffa239376daf9a5936420a932410";
      #   sha256 = "0rwgl7ix1m2wjl2b5mr74xlzagrcn8asf060f538ppr6xprj1yfs";
      # }) {};
      diagrams-reflex = self.callPackage /home/rodney/src/diagrams/diagrams-reflex {};
      reflex-dom-contrib = self.callPackage ./reflex-dom-contrib.nix {};
    };
  };

  drv = haskellPackages.callPackage ./flatris.nix {};

in

  if pkgs.lib.inNixShell then drv.env else drv
