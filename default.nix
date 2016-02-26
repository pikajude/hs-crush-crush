{ mkDerivation, base, Cabal, containers, lens, reflex-dom
, reflex-dom-contrib, stdenv, time, transformers
}:
mkDerivation {
  pname = "crush-crush";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base Cabal containers lens reflex-dom reflex-dom-contrib time
    transformers
  ];
  description = "Date catgirls!";
  license = stdenv.lib.licenses.mit;
}
