{ mkDerivation, fetchFromGitHub
, base, colour, containers, diagrams-core
, diagrams-lib, lens, monoid-extras, mtl, reflex, reflex-dom
, reflex-dom-contrib, stdenv, text
}:
mkDerivation {
  pname = "diagrams-reflex";
  version = "0.2";
  src = fetchFromGitHub {
    owner = "rvl";
    repo = "diagrams-reflex";
    rev = "4cd912a7dfde25cea7d0a1e24fd8ed338a200bbb";
    sha256 = "07i1jaww354nwvi3zj73732z8mil07fxjdjnrxmq2fhqn6z7cvxz";
  };
  libraryHaskellDepends = [
    base colour containers diagrams-core diagrams-lib lens
    monoid-extras mtl reflex reflex-dom reflex-dom-contrib text
  ];
  homepage = "http://projects.haskell.org/diagrams/";
  description = "reflex backend for diagrams drawing EDSL";
  license = stdenv.lib.licenses.bsd3;
}
