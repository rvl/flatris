{ mkDerivation, fetchFromGitHub
, aeson, base, base64-bytestring, bifunctors
, bytestring, containers, data-default, exception-transformers
, ghcjs-dom, http-types, jsaddle, lens, mtl, random, readable
, ref-tf, reflex, reflex-dom-core, safe, stdenv, stm, string-conv
, text, time, transformers, uri-bytestring
}:
mkDerivation {
  pname = "reflex-dom-contrib";
  version = "0.5.1.0";
  src = fetchFromGitHub {
    owner = "reflex-frp";
    repo = "reflex-dom-contrib";
    rev = "57eeca60e5f2d579bd202e4af40c430bebd0f853";
    sha256 = "152cr0gnfqyhflpf7mc79jihsm02xydy913hr99qvjp67b917x50";
  };
  libraryHaskellDepends = [
    aeson base base64-bytestring bifunctors bytestring containers
    data-default exception-transformers ghcjs-dom http-types jsaddle
    lens mtl random readable ref-tf reflex reflex-dom-core safe stm
    string-conv text time transformers uri-bytestring
  ];
  homepage = "https://github.com/reflex-frp/reflex-dom-contrib";
  description = "A place to experiment with infrastructure and common code for reflex applications";
  license = stdenv.lib.licenses.bsd3;
}
