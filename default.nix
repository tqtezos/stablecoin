# SPDX-FileCopyrightText: 2020 tqtezos
# SPDX-License-Identifier: MIT

{ sources ? import ./nix/sources.nix
, haskell-nix ? import sources."haskell.nix" { }
, pkgs ? import sources.nixpkgs haskell-nix.nixpkgsArgs
, ligo ? (import "${sources.ligo}/nix" { }).ligo-bin
}:
let
  project = pkgs.haskell-nix.stackProject {
    src = ./haskell;
    modules = [
      {
        packages.stablecoin.components.tests.stablecoin-test = {
          postInstall = "mkdir -p $out/test/resources; cp -Lr ${tezos-contract} $out/test/resources/stablecoin.tz";
        };
      }
    ];
  };
  tezos-contract = pkgs.stdenv.mkDerivation {
    name = "stablecoin.tz";
    src = ./ligo;
    nativeBuildInputs = [ ligo ];
    buildPhase = "make stablecoin.tz";
    installPhase = "cp stablecoin.tz $out";
  };
in
{
  all = project.stablecoin.components.all;
  lib = project.stablecoin.components.library;
  test = project.stablecoin.components.tests;
  inherit tezos-contract;
}
