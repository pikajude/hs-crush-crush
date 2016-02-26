{ mkDerivation, base, Cabal, containers, lens, reflex-dom
, reflex-dom-contrib, scientific, stdenv, time, transformers
}:
mkDerivation {
  pname = "crush-crush";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base Cabal containers lens reflex-dom reflex-dom-contrib scientific
    time transformers
  ];
  description = "Date catgirls!";
  license = stdenv.lib.licenses.mit;
}
