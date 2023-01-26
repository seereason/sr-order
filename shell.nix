{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, assoc-listlike, base, cereal, containers
      , data-default, extra, generic-lens, HUnit, lens, ListLike, mtl
      , pretty, QuickCheck, safecopy, sr-utils, stdenv, vector
      }:
      mkDerivation {
        pname = "sr-order";
        version = "1.11.1";
        src = ./.;
        libraryHaskellDepends = [
          assoc-listlike base cereal containers data-default extra
          generic-lens lens ListLike mtl pretty QuickCheck safecopy sr-utils
          vector
        ];
        testHaskellDepends = [
          base containers extra HUnit lens mtl QuickCheck
        ];
        homepage = "https://github.com/seereason/th-path";
        description = "A data structure to support lists in collaborative software";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
