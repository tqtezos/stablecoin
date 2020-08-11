<!--
SPDX-FileCopyrightText: 2020 tqtezos
SPDX-License-Identifier: MIT
-->
# Changelog for stablecoin

## 1.1.0
* [#84](https://github.com/tqtezos/stablecoin/pull/84)
  Update LIGO version.
  As a consequence the size of the contract was substantially decreased.
* [#79](https://github.com/tqtezos/stablecoin/pull/79)
  Implement `stablecoin-client`.
* [#75](https://github.com/tqtezos/stablecoin/pull/75)
  Incorporate safelist contract changes according to TZIP-15:
  1. Safelist renamed to "Transferlist".
  2. Transferlist interaction updated to be able to use external transferlists.

## 1.0.0

* [#73](https://github.com/tqtezos/stablecoin/pull/73)
  Update the contract to a newer FA2 revision:
  1. No `total_supply`.
  2. New way of exposing metadata.
  3. Slightly modified error messages.
* [#66](https://github.com/tqtezos/stablecoin/pull/66):
  1. Rename 'NO_ALLOWANCE_EXPECTED' to 'CURRENT_ALLOWANCE_REQUIRED'

## 0.1.0.0

MVP release.
