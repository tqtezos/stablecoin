<!--
SPDX-FileCopyrightText: 2020 TQ Tezos
SPDX-License-Identifier: MIT
-->
# Stablecoin LIGO implementation

Implementation of a token smart contract in LIGO according to the [specification](/docs/specification.md).

## Usage

We recommend using [`stablecoin-client`](/haskell/README.md#client) to interact with the smart contracts.
If you don't want to do that for some reason, you can use the `ligo` tool to compile the smart contracts and construct their initial storage values for origination.
You also need [`tezos-client`](http://tezos.gitlab.io/introduction/howtoget.html) or similar software to create and submit operations to the Tezos network.

## Build Instructions

In order to build the LIGO smart contracts you need to have [`ligo`](https://ligolang.org/) in your `$PATH`.
You can provide a different name/command to launch `ligo` using the `$LIGO` environment variable.
Since LIGO is an actively developed project, some versions of it may be incompatible with Stablecoin.
The revision specified in the [sources.json](/nix/sources.json) file is guaranteed to be compatible.
Use [`Makefile`](/ligo/Makefile) to compile the smart contracts.
For example, running `make` without arguments in this folder should produce `stablecoin.tz`.
