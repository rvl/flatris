{ mkDerivation, array, base, bytestring, colour, containers
, diagrams-lib, diagrams-reflex, diagrams-svg, extra, file-embed
, ghcjs-dom, HUnit, jsaddle, jsaddle-warp, lens, mtl, QuickCheck
, quickcheck-instances, random, reflex, reflex-dom
, reflex-dom-contrib, reflex-dom-core, stdenv, tasty, tasty-hunit
, tasty-quickcheck, text, time, transformers, wai-app-static, warp
, websockets
}:
mkDerivation {
  pname = "flatris";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array base colour diagrams-lib diagrams-svg extra lens mtl random
    text time transformers
  ];
  executableHaskellDepends = [
    array base colour containers diagrams-lib diagrams-reflex extra
    file-embed ghcjs-dom jsaddle jsaddle-warp lens mtl random reflex
    reflex-dom reflex-dom-contrib reflex-dom-core text time
    transformers wai-app-static warp websockets
  ];
  testHaskellDepends = [
    array base bytestring extra HUnit lens QuickCheck
    quickcheck-instances random tasty tasty-hunit tasty-quickcheck text
    time
  ];
  license = stdenv.lib.licenses.gpl3;
}
