<!--
SPDX-FileCopyrightText: 2020 tqtezos
SPDX-License-Identifier: MIT
-->

# Stablecoin Haskell

This folder contains:

1. a Haskell library to interact with the stablecoin contract (basically Haskell bindings) and tests for it.
2. a `stablecoin-client` executable to deploy and interact with the stablecoin contract.

## Install Instructions

You can download static `stablecoin-client` binary for Linux from GitHub [releases](https://github.com/tqtezos/stablecoin/releases).
Alternatively, you can build this package from sources.

You need [Stack](http://haskellstack.org/) to build this package.

To build the library and the executable:
1. Copy or symlink the stablecoin and metadata contracts to `test/resources/` and then run `stack build`.
2. Or, alternatively, run `make build`.

Run `stack install` to install the `stablecoin-client` globally.

## Run Instructions

Pre-requisites: `tezos-client` must be installed.

If the `stablecoin-client` was installed globally with `stack install`, then run `stablecoin-client --help`
to see a list of the available commands.
Otherwise, run `stack run stablecoin-client -- --help`.

## Tests

Tests require the stablecoin and metadata contracts to be in `test/resources/`.
After you put them there you can do `stack test` to run tests.
We have a [`Makefile`](Makefile) that automates this process, so you can run `make test`.
