# SPDX-FileCopyrightText: 2020 tqtezos
# SPDX-License-Identifier: MIT

{ sources ? import ./nix/sources.nix
, haskell-nix ? import sources."haskell.nix" { }
, pkgs ? import sources.nixpkgs haskell-nix.nixpkgsArgs
, weeder-hacks ? import sources.haskell-nix-weeder { inherit pkgs; }
, ligo ? (import "${sources.ligo}/nix" { }).ligo-bin
}:
let
  local-packages = [{
      name = "stablecoin";
      subdirectory = ".";
  }];
  local-packages-names = map (p: p.name) local-packages;
  project = pkgs.haskell-nix.stackProject {
    src = ./haskell;
    modules = [
      {
        packages = pkgs.lib.genAttrs local-packages-names (packageName: {
            package.ghcOptions = with pkgs.lib;
              concatStringsSep " " ([
                "-ddump-to-file" "-ddump-hi"
              ]);
            postInstall = weeder-hacks.collect-dump-hi-files;
        });
        # packages.stablecoin.components.tests.stablecoin-test = {
        #   postInstall = "mkdir -p $out/test/resources; cp -Lr ${tezos-contract} $out/test/resources/stablecoin.tz";
        # };
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
  tezos-client = (import "${sources.tezos-packaging}/pkgs.nix" {}).ocamlPackages.tezos-client;
  weeder-script = weeder-hacks.weeder-script {
    hs-pkgs = project;
    local-packages = local-packages;
  };
in
{
  all = project.stablecoin.components.all;
  lib = project.stablecoin.components.library;
  test = project.stablecoin.components.tests.stablecoin-test;
  nettest = project.stablecoin.components.tests.stablecoin-nettest;
  inherit tezos-contract tezos-client pkgs weeder-script;
}
