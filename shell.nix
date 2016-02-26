{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghcjs" }:

let

  inherit (nixpkgs) pkgs;

  f = import ./default.nix;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  myNodePackages = pkgs.nodePackages.override {
    self = myNodePackages;
    generated = ./nix-extra/node-packages-generated.nix;
  };

  drv = pkgs.haskell.lib.overrideCabal (haskellPackages.callPackage f {}) (drv: {
    buildTools = [ myNodePackages."socket.io" ];
  });

in

  if pkgs.lib.inNixShell then drv.env else drv
