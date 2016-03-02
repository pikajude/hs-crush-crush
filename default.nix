{ mkDerivation, base, bytestring, containers, file-embed, lens, mtl
, reflex-dom, reflex-dom-contrib, scientific, stdenv, stitch, text
, time, transformers
}:
mkDerivation {
  pname = "crush-crush";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring containers file-embed lens mtl reflex-dom
    reflex-dom-contrib scientific stitch text time transformers
  ];
  description = "Date catgirls!";
  license = stdenv.lib.licenses.mit;
}
