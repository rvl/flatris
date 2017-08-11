{ mkDerivation, array, base, bytestring, diagrams-lib, diagrams-svg
, extra, HUnit, lens, mtl, QuickCheck, quickcheck-instances, random
, stdenv, tasty, tasty-hunit, tasty-quickcheck, text, text-format, time
, file-embed
, transformers
,     diagrams-reflex
,    ghcjs-dom
, jsaddle-dom
,    reflex
,    reflex-dom
,    reflex-dom-contrib
,    jsaddle
,    jsaddle-warp
,    warp
,    wai-app-static
,    websockets
}:
mkDerivation {
  pname = "flatris";
  version = "0.1.0.0";
  src = ./.;
  configureFlags = [ "-freflex" ];
  isLibrary = true;
  isExecutable = true;
  executableHaskellDepends = [
    array base diagrams-lib extra lens mtl random time
    text transformers file-embed
    
    diagrams-reflex
    ghcjs-dom
    jsaddle-dom
    reflex
    reflex-dom
    reflex-dom-contrib

    jsaddle
    jsaddle-warp
    warp
    wai-app-static
    websockets
  ];
  libraryHaskellDepends = [
    array base diagrams-lib diagrams-svg extra lens mtl random time
    text transformers
  ];
  testHaskellDepends = [
    array base bytestring HUnit lens QuickCheck quickcheck-instances
    tasty tasty-hunit tasty-quickcheck text
  ];
  license = stdenv.lib.licenses.gpl3;
}
