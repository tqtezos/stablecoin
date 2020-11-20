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
the stablecoin contract [v1.3.0](https://github.com/tqtezos/stablecoin/releases/tag/v1.3.0) in both Carthagenet and Delphinet.

### Carthagenet

* [FA1.2 operations](https://better-call.dev/carthagenet/KT1B24iMQq9QLezhXqjXW9R2WUqN2VnHUtei/operations)
* [FA2 operations](https://better-call.dev/carthagenet/KT1Wg3qi4w9ZNb2EBWTuXjJFPKygVFCXP1S3/operations)

|             | FA1.2 Gas cost | FA2 Gas cost | FA1.2 Tx cost | FA2 Tx cost |
| ----------- | -------------- | ------------ | ------------- | ----------- |
| origination | 448089         | 464076       | 12.695322 ꜩ   | 12.273509 ꜩ |
| transfer    | 412046         | 431839       | 0.041564 ꜩ    | 0.043558 ꜩ  |

### Delphinet

* [FA1.2 operations](https://better-call.dev/delphinet/KT1U9Fumr18CkHR9vXwtTnNbWnw32hYoywzG/operations)
* [FA2 operations](https://better-call.dev/delphinet/KT1Tu6yYQDfvMXa1miDfR4HUoL4PJ5c17MHx/operations)

|             | FA1.2 Gas cost | FA2 Gas cost | FA1.2 Tx cost | FA2 Tx cost |
| ----------- | -------------- | ------------ | ------------- | ----------- |
| origination | 75082          | 71888        | 3.178771 ꜩ    | 3.07229 ꜩ   |
| transfer    | 77585          | 74804        | 0.008117 ꜩ    | 0.007853 ꜩ  |

### Measuring

To measure and collect these numbers:
1. Configure your `tezos-client` to use a carthagenet or delphinet node
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

## Issue Tracker

We use [GitHub issues](https://github.com/tqtezos/stablecoin/issues).
Feel free to [open a new one](https://github.com/tqtezos/stablecoin/issues/new/choose).

## License

[MIT](/LICENSE)
