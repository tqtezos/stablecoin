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

            # enable haddock for local packages
            doHaddock = true;
        });

        # disable haddock for dependencies
        doHaddock = false;
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
  tezos-contract-fa1-2 = pkgs.stdenv.mkDerivation {
    name = "stablecoin.tz";
    src = ./ligo;
    nativeBuildInputs = [ ligo ];
    buildPhase = "make stablecoin.fa1.2.tz";
    installPhase = "cp stablecoin.fa1.2.tz $out";
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

  # This ugly workaround should be removed, once the https://github.com/typeclasses/hex-text/pull/5
  # is merged
  morley = pkgs.haskell.lib.doJailbreak (pkgs.haskellPackages.callHackageDirect {
    pkg = "morley";
    ver = "1.7.1";
    sha256 = "1ps2191za6agm78hqsrra185in4sjdaihqkn4hxng33csq2bj1vh";
  } {
    cryptonite = pkgs.haskell.lib.doJailbreak (pkgs.haskellPackages.callHackageDirect {
      pkg = "cryptonite";
      ver = "0.27";
      sha256 = "0y8mazalbkbvw60757av1s6q5b8rpyks4lzf5c6dhp92bb0rj5y7";
    } {});
    hex-text = pkgs.haskell.lib.doJailbreak (pkgs.haskellPackages.callHackageDirect {
      pkg = "hex-text";
      ver = "0.1.0.0";
      sha256 = "13y3yws4xv99ngd8rgn8n02cyzb3kvyyh6zd88ficmqs4cnhvw7y";
    } {
      base16-bytestring = pkgs.haskell.lib.doJailbreak (pkgs.haskellPackages.callHackageDirect {
        pkg = "base16-bytestring";
        ver = "0.1.1.7";
        sha256 = "0sv4gvaz1hwllv7dpm8b8xkrpsi1bllgra2niiwpszpq14bzpail";
      } {});
     });
  });
in
{
  lib = project.stablecoin.components.library;
  haddock = project.stablecoin.components.library.haddock;
  test = project.stablecoin.components.tests.stablecoin-test;
  nettest = project.stablecoin.components.tests.stablecoin-nettest;
  stablecoin-client = project.stablecoin.components.exes.stablecoin-client;
  inherit tezos-contract tezos-contract-fa1-2 tezos-metadata-contract tezos-client pkgs weeder-script morley;
}
