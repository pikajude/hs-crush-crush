{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = import ./default.nix;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  myNodePackages = pkgs.nodePackages.override {
    self = myNodePackages;
    generated = ./nix-extra/generated.nix;
  };

  drv = pkgs.haskell.lib.addBuildTools (haskellPackages.callPackage f {})
    [ myNodePackages.bower haskellPackages.cabal-install haskellPackages.ghc-mod ];

in

  if pkgs.lib.inNixShell then drv.env else drv
