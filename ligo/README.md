<!--
SPDX-FileCopyrightText: 2020 TQ Tezos
SPDX-License-Identifier: MIT
-->
# Stablecoin LIGO implementation

Implementation of a token smart contract in LIGO according to the [specification](/docs/specification.md).

## Usage

We recommend using [`stablecoin-client`](/haskell/README.md#client) to interact with smart contracts.
If you don't want to do that for some reason, you can use `ligo` tool to compile smart contracts and construct their initial storage values for origination.
You also need [`tezos-client`](http://tezos.gitlab.io/introduction/howtoget.html) or similar software to create and submit operations to Tezos network.

## Build Instructions

In order to build LIGO smart contracts you need to have [`ligo`](https://ligolang.org/) in your `$PATH`.
You can provide a different name/command to launch `ligo` using `$LIGO` environment variable.
Since LIGO is an actively developed project, some versions of it may be incompatible with Stablecoin.
Revision specified in the [sources.json](/nix/sources.json) file is guaranteed to be compatible.
Use [`Makefile`](/ligo/Makefile) to compile smart contracts.
For example, running `make` without arguments should produce `stablecoin.tz`.
