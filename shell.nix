{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghcjs" }:

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
    [ myNodePackages.bower ];

in

  if pkgs.lib.inNixShell then drv.env else drv
