# SPDX-FileCopyrightText: 2020 tqtezos
# SPDX-License-Identifier: MIT

with import (builtins.fetchGit {
  url = "https://github.com/NixOS/nixpkgs-channels";
  ref = "nixos-unstable";
  rev = "971b731fc18c86569211a460ef62e1d8001799e9";
}) { };

haskell.lib.buildStackProject {
  name = "myEnv";
  buildInputs = [ ghc zlib ];
  buildPhase = ''
    export LANG=en_US.UTF-8
  '';
}