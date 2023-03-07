{ nixpkgs ? import <nixpkgs> {} }:
nixpkgs.haskellPackages.developPackage { root = ./.; }
