{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghcjs" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, Cabal, lens, nodejs, reflex-dom, reflex-dom-contrib, stdenv, time
      , transformers
      }:
      mkDerivation {
        pname = "crush-crush";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base Cabal lens reflex-dom reflex-dom-contrib time transformers
        ];
        buildTools = [ nodejs ];
        description = "Date catgirls!";
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
