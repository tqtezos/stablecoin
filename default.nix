# SPDX-FileCopyrightText: 2022 Oxhead Alpha
# SPDX-License-Identifier: MIT

{ sources ? import ./nix/sources.nix
, static ? true
, haskell-nix ? import sources."haskell.nix" {
    sourcesOverride = { hackage = sources."hackage.nix"; stackage = sources."stackage.nix"; };
  }
, pkgs ? import sources.nixpkgs haskell-nix.nixpkgsArgs
, ligo ? (pkgs.runCommand "ligo" {} "mkdir -p $out/bin; cp ${sources.ligo} $out/bin/ligo; chmod +x $out/bin/ligo")
, morley ? (import "${sources.morley}/ci.nix").packages.morley.exes.morley
, morley-infra ? (import sources.morley-infra)
}:
let
  inherit (morley-infra) tezos-client weeder-hacks run-chain-tests;
  haskell-nix =
    if static
    then pkgs.pkgsCross.musl64.haskell-nix
    else pkgs.haskell-nix;
  local-packages = [{
      name = "stablecoin";
      subdirectory = ".";
  }];
  local-packages-names = map (p: p.name) local-packages;

  projectSrc = haskell-nix.haskellLib.cleanGit {
    name = "stablecoin";
    src = ./haskell;
  };

  # haskell.nix does not support 'include' in package.yaml, we have to generate .cabal ourselves
  cabalFile = pkgs.runCommand "stablecoin.cabal" {} ''
    ${pkgs.haskellPackages.hpack}/bin/hpack ${projectSrc} - > $out
  '';

  project = haskell-nix.stackProject {
    # project src with .cabal file added
    src = pkgs.runCommand "src-with-cabal" {} ''
      cp -r --no-preserve=mode ${projectSrc} $out
      cp ${cabalFile} $out/stablecoin.cabal
    '';
    ignorePackageYaml = true;
    modules = [
      {
        packages = pkgs.lib.genAttrs local-packages-names (packageName: {
            ghcOptions = [
              "-ddump-to-file" "-ddump-hi"
              "-O0" "-Werror"
            ];
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

  weeder-script = morley-infra.weeder-script {
    hs-pkgs = project;
    inherit local-packages;
  };

in
{
  lib = project.stablecoin.components.library;
  haddock = project.stablecoin.components.library.haddock;
  test = project.stablecoin.components.tests.stablecoin-test;
  nettest = project.stablecoin.components.tests.stablecoin-nettest;
  stablecoin-client = project.stablecoin.components.exes.stablecoin-client;
  inherit tezos-contract tezos-contract-fa1-2 tezos-metadata-contract tezos-client pkgs weeder-script morley run-chain-tests;
}
