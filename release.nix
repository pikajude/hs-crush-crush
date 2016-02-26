{ pkgs ? import <nixpkgs> {}, compiler ? "default", crush-crush ? { outPath = ./.; } }:

let
  haskellPackages = if compiler == "default"
    then pkgs.haskellPackages
    else pkgs.haskell.packages.${compiler};
  build = haskellPackages.callPackage ./default.nix {};

  tarball = with pkgs; releaseTools.sourceTarball rec {
    name = build.pname;
    version = build.version;
    src = crush-crush;
    buildInputs = [ git ];

    postUnpack = ''
      # Clean up when building from a working tree.
      if [[ -d $sourceRoot/.git ]]; then
        git -C $sourceRoot clean -fdx
      fi
    '';

    distPhase = ''
      tar cfj tarball.tar.bz2 * --transform 's,^,${name}/,'
      mkdir -p $out/tarballs
      cp *.tar.* $out/tarballs
    '';
  };

in pkgs.haskell.lib.overrideCabal build (drv: {
  configureFlags = [ "-fopt" ];
  src = "${tarball}/tarballs/*.tar.bz2";
})
