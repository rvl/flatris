# Flatris

Like the popular falling block game, but flatter and less fun.

## About

This is a FRP reimplementation of a little game I wrote a long time
ago.

The original was written in C++ using OpenGL.

This version is written in Haskell using SVG in a web browser, via the
Reflex FRP library.

## Play online

https://rvl.github.io/flatris/

## Building

Building with nix and GHC:

    nix-shell --run "cabal configure && cabal build"

Building with nix ghcjs:

    nix-shell --argstr compiler ghcjs --run "cabal configure --ghcjs && cabal build"

Full site build with minification (uses nix-shell and Shake). Will
create a `docs` directory:

    ./build.sh
