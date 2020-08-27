<!--
SPDX-FileCopyrightText: 2020 TQ Tezos
SPDX-License-Identifier: MIT
-->

# Tezos Stablecoin

[![Build status](https://badge.buildkite.com/c38c76106a10aeaea23f487d41b52514f4ffb84974852021f7.svg?branch=master)](https://buildkite.com/serokell/stablecoin)

Tezos Stablecoin project implements an FA2-compatible token smart contract.
It is based on [CENTRE Fiat Token](https://github.com/centrehq/centre-tokens/blob/78d964a1a8d481ffd8152772d7a66e47df54b3db/doc/tokendesign.md).
The contract is implemented in the [LIGO language](https://ligolang.org/).
See the [`ligo/`](ligo/) folder.
Contract specification is [available](/docs/specification.md).

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

## Issue Tracker

We use [GitHub issues](https://github.com/tqtezos/stablecoin/issues).
Feel free to [open a new one](https://github.com/tqtezos/stablecoin/issues/new/choose).

## License

[MIT](/LICENSE)
