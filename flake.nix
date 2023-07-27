# SPDX-FileCopyrightText: 2022 Oxhead Alpha
# SPDX-License-Identifier: MIT

{
  description = "The stablecoin flake";

  nixConfig.flake-registry = "https://gitlab.com/morley-framework/morley-infra/-/raw/main/flake-registry.json";

  inputs.morley-infra.url = "gitlab:morley-framework/morley-infra";
  inputs.morley.url = "gitlab:morley-framework/morley";

  outputs = { self, flake-utils, morley-infra, morley, ... }:
    (flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = morley-infra.legacyPackages.${system};

        inherit (morley-infra.utils.${system}) ci-apps;

        ligo = pkgs.stdenv.mkDerivation {
          name = "ligo";
          src = pkgs.fetchurl {
            url = "https://gitlab.com/ligolang/ligo/-/jobs/2684632179/artifacts/raw/ligo";
            sha256 = "sha256-sr1gxTDB99F3Cmm2tl2280MAOCT4ONtW9Xv69NdOqIk=";
          };
          dontUnpack = true;
          installPhase = ''
            mkdir -p $out/bin
            cp $src $out/bin/ligo;
            chmod +x $out/bin/ligo
          '';
        };
        ligoVersion = "0.46.1";

        local-packages = [{
            name = "stablecoin";
            subdirectory = ".";
        }];
        local-packages-names = map (p: p.name) local-packages;

        projectSrc = ./haskell;

        cabalFile = pkgs.runCommand "stablecoin.cabal" {} ''
          ${pkgs.haskellPackages.hpack}/bin/hpack ${projectSrc} - > $out
        '';

        hs-pkgs = pkgs.haskell-nix.stackProject {
          # project src with .cabal file added
          src = pkgs.runCommand "src-with-cabal" {} ''
            cp -r --no-preserve=mode ${projectSrc} $out
            cp ${cabalFile} $out/stablecoin.cabal
          '';
          ignorePackageYaml = true;
          modules = [
            {
              packages = pkgs.lib.genAttrs local-packages-names (packageName: ci-apps.collect-hie false {
                  ghcOptions = [ "-O0" "-Werror" ];
                  # enable haddock for local packages
                  doHaddock = true;
              });

              # disable haddock for dependencies
              doHaddock = false;
            }
            {
              packages.stablecoin.components.library =
              let
                copyContracts = ''
                  cp ${self.packages.${system}.tezos-contract} ./test/resources/stablecoin.tz
                  cp ${self.packages.${system}.tezos-contract-fa1-2} ./test/resources/stablecoin.fa1.2.tz
                  cp ${self.packages.${system}.tezos-metadata-contract} ./test/resources/metadata.tz
                '';
              in
              {
                preBuild = ''
                  export STABLECOIN_LIGO_VERSION=${ligoVersion}
                  mkdir -p ./test/resources/
                  ${copyContracts}
                '';

                preHaddock = copyContracts;
              };
            }
          ];

          shell = {
            tools = {
              cabal = {};
              hlint = { version = "3.5"; };
              hpack = { version = "0.35.1"; };
            };
          };
        };

        # We don't distinguish development and non-development modes for now.
        hs-pkgs-development = hs-pkgs;

        flake = hs-pkgs-development.flake {};

        compileTezosContract = contract: pkgs.stdenv.mkDerivation {
          name = "stablecoin.tz";
          src = ./ligo;
          nativeBuildInputs = [ ligo ];
          buildPhase = "make ${contract}";
          installPhase = "cp ${contract} $out";
        };

        tezos-contract = compileTezosContract "stablecoin.tz";
        tezos-contract-fa1-2 = compileTezosContract "stablecoin.fa1.2.tz";
        tezos-metadata-contract = compileTezosContract "metadata.tz";

      in pkgs.lib.lists.foldr pkgs.lib.recursiveUpdate {} [
        { inherit (flake) packages devShells apps; }

        {
          utils = { run-chain-tests = morley-infra.utils.${system}.run-chain-tests; };

          legacyPackages = pkgs;

          packages = {
            haddock = self.packages.${system}."stablecoin:lib:stablecoin".haddock;

            all-components = pkgs.linkFarmFromDrvs "all-components" (builtins.attrValues flake.packages);
            morley = morley.packages.${system}."morley:exe:morley";
            default = self.packages.${system}.all-components;

            inherit tezos-contract tezos-contract-fa1-2 tezos-metadata-contract;
          };

          checks = {
            trailing-whitespace = pkgs.build.checkTrailingWhitespace ./.;

            reuse-lint = pkgs.build.reuseLint ./.;
          };
        }

        {
          apps = ci-apps.apps {
            hs-pkgs = hs-pkgs-development;
            inherit local-packages projectSrc;
          };
        }
      ]
    ));
}
