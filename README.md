# Flatris

Like the popular falling block game, but flatter and less fun.

## About

This is a FRP reimplementation of a little game I wrote a long time
ago.

The original was written in C++ using OpenGL.

This version is written in Haskell using SVG in a web browser, via the
[Reflex FRP][reflex] library.

## Play online

https://rvl.github.io/flatris/

## Building

Building with nix and GHC:

    nix-shell --run "cabal configure && cabal build"

Building with nix ghcjs:

    nix-shell --argstr compiler ghcjs --run "cabal configure --ghcjs && cabal build"

Full site build with minification (uses nix-shell
and [Shake][shake]). Will create a `docs` directory:

    ./build

Note that the build depends on the [Reflex Platform][reflex]
environment. You will want to check that `./try-reflex` works
first. Make sure the binary cache for reflex is added to your Nix
config, or your computer will be building a lot of packages.

## Development

If you build for GHC, the app will run in a WebKitGtk frame. The code
can be loaded and started through `ghci`.

For automatic reloading, the app runs a web server and is viewed with
a web browser. It uses [ghcid][ghcid] to monitor the sources and
reload the code.

To start the auto-reloading dev server (do this within nix-shell):

    ./dev-server

Then visit http://localhost:3030/

[reflex]: https://github.com/reflex-frp/reflex-platform
[shake]: http://shakebuild.com/
[ghcid]: https://github.com/ndmitchell/ghcid
[jsaddle]: https://github.com/ghcjs/jsaddle
