# SPDX-FileCopyrightText: 2020 TQ Tezos
# SPDX-License-Identifier: MIT

{ sources ? import ./nix/sources.nix
, static ? true
, haskell-nix ? import sources."haskell.nix" {
    sourcesOverride = { hackage = sources."hackage.nix"; stackage = sources."stackage.nix"; };
  }
, pkgs ? import sources.nixpkgs haskell-nix.nixpkgsArgs
, weeder-hacks ? import sources.haskell-nix-weeder { inherit pkgs; }
, ligo ? (import "${sources.ligo}/nix" { }).ligo-bin
}:
let
  haskell-nix =
    if static
    then pkgs.pkgsCross.musl64.haskell-nix
    else pkgs.haskell-nix;
  local-packages = [{
      name = "stablecoin";
      subdirectory = ".";
  }];
  local-packages-names = map (p: p.name) local-packages;
  project = haskell-nix.stackProject {
    src = haskell-nix.haskellLib.cleanGit {
      name = "stablecoin";
      src = ./haskell;
    };
    modules = [
      {
        packages = pkgs.lib.genAttrs local-packages-names (packageName: {
            package.ghcOptions = with pkgs.lib;
              concatStringsSep " " ([
                "-ddump-to-file" "-ddump-hi"
                "-O0" "-Werror"
              ]);
            postInstall = weeder-hacks.collect-dump-hi-files;
        });
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
  tezos-metadata-contract = pkgs.stdenv.mkDerivation {
    name = "stablecoin.tz";
    src = ./ligo;
    nativeBuildInputs = [ ligo ];
    buildPhase = "make metadata.tz";
    installPhase = "cp metadata.tz $out";
  };
  tezos-client = (import "${sources.tezos-packaging}/nix/build/pkgs.nix" {}).ocamlPackages.tezos-client;

  # nixpkgs has weeder 2, but we use weeder 1
  weeder-legacy = pkgs.haskellPackages.callHackageDirect {
    pkg = "weeder";
    ver = "1.0.9";
    sha256 = "0gfvhw7n8g2274k74g8gnv1y19alr1yig618capiyaix6i9wnmpa";
  } {};

  weeder-script = weeder-hacks.weeder-script {
    weeder = weeder-legacy;
    hs-pkgs = project;
    local-packages = local-packages;
  };

  morley =
    (pkgs.haskell-nix.hackage-package
      { name = "morley"; version = "1.4.0"; compiler-nix-name = "ghc883"; }
    ).components.exes.morley;
in
{
  lib = project.stablecoin.components.library;
  test = project.stablecoin.components.tests.stablecoin-test;
  nettest = project.stablecoin.components.tests.stablecoin-nettest;
  stablecoin-client = project.stablecoin.components.exes.stablecoin-client;
  inherit tezos-contract tezos-metadata-contract tezos-client pkgs weeder-script morley;
}
