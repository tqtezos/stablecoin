<!--
SPDX-FileCopyrightText: 2020 TQ Tezos
SPDX-License-Identifier: MIT
-->

# Tezos Stablecoin

[![Build status](https://badge.buildkite.com/c38c76106a10aeaea23f487d41b52514f4ffb84974852021f7.svg?branch=master)](https://buildkite.com/serokell/stablecoin)

Tezos Stablecoin project implements an FA2-compatible token smart contract.
It draws inspiration from the [CENTRE Fiat Token](https://github.com/centrehq/centre-tokens/blob/78d964a1a8d481ffd8152772d7a66e47df54b3db/doc/tokendesign.md) and other similar contracts.
The contract is implemented in the [LIGO language](https://ligolang.org/).
See the [`ligo/`](ligo/) folder.
Contract specification is [available](/docs/specification.md).

The project also includes an alternative FA1.2-compatible smart contract, available in the [`ligo/stablecoin/fa1.2/`](ligo/stablecoin/fa1.2/) folder.

## Stablecoin client

We have a `stablecoin-client` executable that allows deploying and interacting with the stablecoin contract.
You can download it as a static Linux binary from GitHub [releases](https://github.com/tqtezos/stablecoin/releases).
Please refer to the [`haskell/`](/haskell/) directory for details.

## Haskell bindings

Apart from LIGO implementation of the contract we have [a Haskell library](haskell/) with data types corresponding to this contract.
It is based on the [morley framework](https://gitlab.com/morley-framework/morley).
It allows us to use features of `morley` (such as testing engine) with this contract.

## Tests

Tests are implemented in the same Haskell package.
Please refer to the [`haskell/`](/haskell/) directory for details.

## Gas / Transaction costs

The tables below show the gas and transaction costs of both versions (FA1.2 and FA2) of
the stablecoin contract [v1.4.0](https://github.com/tqtezos/stablecoin/releases/tag/v1.4.0) in Delphinet.

### Delphinet

* [FA1.2 operations](https://better-call.dev/delphinet/KT1AMAqrzMQqB6QK724Bagp8LL2JrdXEF3By/operations)
* [FA2 operations](https://better-call.dev/delphinet/KT1G3n8sDudm1FzCn9JYa5BM2QRaxgKry8My/operations)

|             | FA1.2 Gas cost | FA2 Gas cost | FA1.2 Tx cost | FA2 Tx cost |
| ----------- | -------------- | ------------ | ------------- | ----------- |
| origination | 75928          | 72689        | 3.942008 ꜩ    | 3.87493 ꜩ   |
| transfer    | 79196          | 73293        | 0.007978 ꜩ    | 0.007702 ꜩ  |

### Measuring

To measure and collect these numbers:
1. Configure your `tezos-client` to use a delphinet node
1. Make sure `tezos-client` has a `nettest` alias with enough ꜩ
    ```
    tezos-client get balance for nettest
    ```
1. Run `cd haskell && stack test stablecoin:test:stablecoin-nettest`
1. The logs should show these two messages, with two addresses:
    ```
    Originated smart contract Stablecoin FA1.2 with address <...>
    Originated smart contract Stablecoin FA2 with address <...>
    ```
1. Search for these addresses in <https://better-call.dev/>, select the "Contracts" tab, you should get 1 search result.
   Select it to see the origination and transfer costs.

Note that we also run these tests nightly as part of our CI, so another way to do this measurement is to wait for CI to run these tests and find 2 addresses in the logs of the latest scheduled CI run.

## Issue Tracker

We use [GitHub issues](https://github.com/tqtezos/stablecoin/issues).
Feel free to [open a new one](https://github.com/tqtezos/stablecoin/issues/new/choose).

## License

[MIT](/LICENSE)
