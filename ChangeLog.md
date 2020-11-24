<!--
SPDX-FileCopyrightText: 2020 TQ Tezos
SPDX-License-Identifier: MIT
-->
# Changelog for stablecoin

## Unreleased changes
<!-- Prepend new entries here -->
<!-- Don't forget to update the gas/transaction costs tables in the
README when a new version is released. -->
## 1.4.0

* [#134](https://github.com/tqtezos/stablecoin/pull/134)
  * Updated TZIP-17 implementation:
    * Update parameter of `setExpiry` entrypoint
    * Fail with `"EXPIRY_TOO_BIG"` when expiry parameter is too big
    * Setting expiry of zero revokes the permit.
    * Fail with `"NOT_PERMIT_ISSUER"` when someone other then permit
      issuer tries to call `setExpiry` entrypoint.
* [#135](https://github.com/tqtezos/stablecoin/pull/135)
  * Updated TZIP-17 implementation:
    * Fail with `"DUP_PERMIT"` when a permit is issued twice
    * Removed `revoke` entrypoint
  * Added support for TZIP-16

## 1.3.0

* [#118](https://github.com/tqtezos/stablecoin/pull/118)
  Include changes to FA2 w.r.t `Update_operators`.
  Remove `Is_operator` entrypoint.
* [#112](https://github.com/tqtezos/stablecoin/pull/112)
  Haskell bindings for `Storage` and related types now use
  Haskell data constructors instead of nested tuples.
* [#104](https://github.com/tqtezos/stablecoin/pull/104)
  Changed default token name from `USDC` to `Test` and token symbol
  from `USDC` to `TEST`
* [#93](https://github.com/tqtezos/stablecoin/pull/93)
  Implemented TZIP-17

## 1.2.0

Operations on the contract got cheaper in this release:

* [#96](https://github.com/tqtezos/stablecoin/pull/96)
  Metadata was moved into a separate contract.
* [#95](https://github.com/tqtezos/stablecoin/pull/95)
  Maximal number of minters is limited, they are stored in strict `map`.
* [#89](https://github.com/tqtezos/stablecoin/pull/89)
  Prune 0 balance accounts from the storage.
* [#85](https://github.com/tqtezos/stablecoin/pull/92)
  Removed `permissions_descriptor` entrypoint.
  Removed 'total_supply' from storage.
  Changed owner hook behavior to `Owner_no_hook`.

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
