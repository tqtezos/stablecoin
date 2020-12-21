<!--
SPDX-FileCopyrightText: 2020 TQ Tezos
SPDX-License-Identifier: MIT
-->

# Stablecoin Haskell

This folder contains:

1. a `stablecoin-client` executable to deploy and interact with the stablecoin contract;
1. a Haskell library to interact with the stablecoin contract (basically Haskell bindings) and tests for it.

## Client

The most important part for a user is `stablecoin-client`.
If the `stablecoin-client` was downloaded from releases or installed globally with `stack install`, then run `stablecoin-client --help`
to see a list of the available commands.
Otherwise, you should prefix all commands with `stack run` and put `--` after `stablecoin-client`, for example: `stack run stablecoin-client -- --help`.

The help message should be sufficiently descriptive, however, there are some caveats:
1. [`tezos-client`](http://tezos.gitlab.io/introduction/howtoget.html) executable must be available and is assumed to be in `$PATH` by default.
1. Node data (address, port and whether to use TLS) is taken from `tezos-client` config by default.
Make sure it points to an active node in the network where you want to submit your operations.
You can override this data using the respective options of `stablecoin-client`.
1. There is a global option `--user` that specifies which user will make the operations, it defaults to `stablecoin-user`.
You should ensure that:
  * An address with the provided alias is known to `tezos-client`.
  * Its secret key is known.
  * It has sufficient balance to make operations (pay for storage and fee).

## Build Instructions

Note: if you are on Linux and just want to get `stablecoin-client`, we recommend downloading it from the latest [release assets](https://github.com/tqtezos/stablecoin/releases/latest).

You need [Stack](http://haskellstack.org/) to build this package.

To build the library and the executable:
1. Copy or symlink the stablecoin and metadata smart contracts (their Michelson versions) to `test/resources/` and then run `stack build`.
The contracts are parsed and typechecked at compile-time.
2. Or, alternatively, run `make build` (from this folder) provided that you have `ligo` and [`morley`](https://gitlab.com/morley-framework/morley) in your `$PATH`.

Note that in the second case we automatically build all LIGO contracts and apply `morley optimize` to them.
In the first case you are supposed to provide the contracts yourself and it's up to you whether to run `morley optimize`.

Run `stack install` to install the `stablecoin-client` globally.
The installation path may or may not be in your `$PATH` depending on your system, but it should be printed as part of the output.

## Tests

Tests require the stablecoin and metadata contracts to be in `test/resources/`.
After you put them there you can do `stack test` to run tests.
We have a [`Makefile`](Makefile) that automates this process, so you can run `make test` from this folder as long as you have `morley` and `ligo` as stated in the build instructions above.
