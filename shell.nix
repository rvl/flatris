# --argstr compiler could be ghcjs or ghc
{ nixpkgs ? import <nixpkgs> {}
, compiler ? "ghc"
, enableLibraryProfiling ? false
}:
let
  inherit (nixpkgs) pkgs;

  tryReflex = import (pkgs.fetchFromGitHub {
    owner = "reflex-frp";
    repo = "reflex-platform";
    rev = "b7c00b3574d0ef42974eda0f2812c794c7b5d4f3";
    sha256 = "1jfz17y2fq051caby4y4aslxrpvgwwa30ivfw0l5wn5pp5zlrpad";
  }) {
    inherit enableLibraryProfiling;
  };

  # compiler could be ghcjs or ghc
  reflex-platform = tryReflex.${compiler};

  # overrides of various packages
  haskellPackages = reflex-platform.override {
    overrides = self: super: {
      # for version compatibility with this reflex
      reflex-dom-contrib = self.callPackage ./reflex-dom-contrib.nix {};
      # reflex-dom-core gc test hangs when built with profiling
      reflex-dom-core = pkgs.haskell.lib.dontCheck super.reflex-dom-core;
    };
  };

  drv = haskellPackages.callPackage ./flatris.nix {};

in

  if pkgs.lib.inNixShell then drv.env else drv
