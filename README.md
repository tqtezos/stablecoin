<!--
SPDX-FileCopyrightText: 2021 Oxhead Alpha
SPDX-License-Identifier: MIT
-->

# Tezos Stablecoin

[![Build status](https://badge.buildkite.com/c38c76106a10aeaea23f487d41b52514f4ffb84974852021f7.svg?branch=master)](https://buildkite.com/serokell/stablecoin)

Tezos Stablecoin project implements an FA2-compatible token smart contract.
It draws inspiration from popular permissioned asset contracts like [CENTRE Fiat Token](https://github.com/centrehq/centre-tokens/blob/78d964a1a8d481ffd8152772d7a66e47df54b3db/doc/tokendesign.md) and other similar contracts.
The contract is implemented in the [LIGO language](https://ligolang.org/).
This repository consists of the following:
1. LIGO source code of the Stablecoin smart contract and auxiliary smart contracts in the [`ligo/`](ligo/) folder.
1. [Contract specification](/docs/specification.md).
1. Haskell bindings with data types corresponding to Stablecoin.
It is based on the [morley framework](https://gitlab.com/morley-framework/morley).
It allows us to use features of `morley` (such as a testing engine called `cleveland`) with this contract.
1. `stablecoin-client` executable that allows deploying and interacting with the Stablecoin smart contract.

Please refer to the [`haskell/`](/haskell/) directory for details regarding `stablecoin-client` and Haskell bindings.

The project also includes an alternative FA1.2-compatible smart contract, available in the [`ligo/stablecoin/fa1.2/`](ligo/stablecoin/fa1.2/) folder.

## How to get

You can download Michelson source code of all provided smart contracts in [release assets](https://github.com/tqtezos/stablecoin/releases/latest).
We also provide a static `stablecoin-client` executable that should work on any x86_64 Linux system.
If you are using a different OS or just want to build from sources, see [instructions below](#build-instructions).

## Usage

The recommended way to deploy and interact with Stablecoin is to use `stablecoin-client`.
For example, in order to deploy the contract you can run
```
./stablecoin-client --user foo deploy --master-minter foo --contract-owner foo --pauser foo --default-expiry 300000
```
provided you have [`tezos-client`](http://tezos.gitlab.io/introduction/howtoget.html) in your `$PATH`, it's configured to use an appropriate node and knows `foo` address with sufficient balance to pay for operations.

## Build instructions

* [Smart contracts](/ligo/README.md#build-instructions)
* [Haskell (including client)](/haskell/README.md#build-instructions)

## Tests

Tests are implemented in Haskell.
Please refer to the [`haskell/`](/haskell/) directory for details.

## Gas / Transaction costs

The tables below show the gas and transaction costs of both versions (FA1.2 and FA2) of
the stablecoin contract [v1.7.3](https://github.com/tqtezos/stablecoin/releases/tag/v1.7.3) in jakartanet.

### jakartanet

* [FA1.2 operations](https://better-call.dev/jakartanet/KT1ASuzkJzZ2pCqYVKeV48p5yxunynrjmdGE/operations)
* [FA2 operations](https://better-call.dev/jakartanet/KT19mksNfM9XjWqapR2iMywM1ic5kbVQwpjB/operations)

|             | FA1.2 Gas cost | FA2 Gas cost | FA1.2 Tx cost | FA2 Tx cost |
| ----------- | -------------- | ------------ | ------------- | ----------- |
| origination | 5743           | 5809         | 2.381299 ꜩ    | 2.414438 ꜩ  |
| transfer    | 6659           | 7003         | 0.001001 ꜩ    | 0.001053 ꜩ  |

### Measuring

To measure and collect these numbers:
1. Pick a testnet:
    ```
    export STABLECOIN_TESTNET=https://jakarta.testnet.tezos.serokell.team/
    ```
1. Create a test account and grab its hash:
    ```
    $ tezos-client gen keys stablecoin-moneybag --force
    $ tezos-client -E $STABLECOIN_TESTNET show address stablecoin-moneybag
    Hash: tz1Shin6pHETKn7EroLGyG2afTenW2zT2K6w
    Public Key: edpktpFQUaRZRHPMBgk66W4jQoYK2hzZ95o9ZJrPmH38zqSzfH5Zor
    ```
1. Use a [testnet faucet](https://teztnets.xyz/) to add funds to your account.
1. Run:
    ```bash
    cd haskell
    stack test --fast --test-arguments \
        "-p \"Lorentz.Contracts.Nettest.FA1_2Comparison\" \
        --cleveland-mode only-network \
        --cleveland-moneybag-alias stablecoin-moneybag \
        -E $STABLECOIN_TESTNET"
    ```
   This deploys the FA2 and FA1.2 contracts (the TZIP-16 metadata will be stored in separate contracts),
   and calls the `transfer` entrypoint of each contract.

1. The logs should show these two messages, with two addresses:
    ```
    Originated smart contract 'Stablecoin FA1.2' with address <...>
    Originated smart contract 'Stablecoin FA2' with address <...>
    ```
1. Search for these addresses in <https://better-call.dev/>, select the "Contracts" tab, you should get 1 search result.
   Select it to see the origination and transfer costs.

Note that we also run these tests nightly as part of our CI, so another way to do this measurement is to wait for CI to run these tests and find 2 addresses in the logs of the latest scheduled CI run.

## Issue Tracker

We use [GitHub issues](https://github.com/tqtezos/stablecoin/issues).
Feel free to [open a new one](https://github.com/tqtezos/stablecoin/issues/new/choose).

## License

[MIT](/LICENSE)
