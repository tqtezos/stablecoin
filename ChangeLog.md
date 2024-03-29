<!--
SPDX-FileCopyrightText: 2021 Oxhead Alpha
SPDX-License-Identifier: MIT
-->
# Changelog for stablecoin

## Unreleased changes
<!-- Prepend new entries here -->
<!-- Don't forget to update the gas/transaction costs tables in the
README when a new version is released. -->

* [#203](https://github.com/tqtezos/stablecoin/pull/203)
  Add suport for the Lima protocol.

## 1.7.4

* [#198](https://github.com/tqtezos/stablecoin/pull/198)
  * Add support for the kathmandu protocol.

## 1.7.3

* [#196](https://github.com/tqtezos/stablecoin/pull/196)
  * Update the structure of the `set_expiry` parameter to be compatible with
    [the 2020-10-30 version](https://gitlab.com/tezos/tzip/-/blob/eb1da57684599a266334a73babd7ba82dbbbce66/proposals/tzip-17/tzip-17.md)
    of TZIP-17.
* [#195](https://github.com/tqtezos/stablecoin/pull/195)
  * Update to ligo 0.46.1

## 1.7.2

* [#181](https://github.com/tqtezos/stablecoin/pull/181)
  * Remove network tests on `010` local-chain and `granadanet`.
* [#180](https://github.com/tqtezos/stablecoin/pull/180)
  * Add network tests on `011` local-chain and `hangzhounet`.
* [#168](https://github.com/tqtezos/stablecoin/pull/168)
  * Remove coercions between 'Alias' and 'AliasHint'
* [#167](https://github.com/tqtezos/stablecoin/pull/167)
  * Add unsafe way to compile view code from morley-metadata
* [#165](https://github.com/tqtezos/stablecoin/pull/165)
  * Bump `morley`, `lorentz` and other dependencies.
* [#173](https://github.com/tqtezos/stablecoin/pull/173)
  * Add network tests on `010` local-chain and `granadanet`.
* [#172](https://github.com/tqtezos/stablecoin/pull/172)
  * Remove network tests on `008` local-chain and `edo2net`.
* [#170](https://github.com/tqtezos/stablecoin/pull/170)
  * Add network tests on `009` local-chain and `florencenet`.
* [#170](https://github.com/tqtezos/stablecoin/pull/170)
  * Add network tests on `009` local-chain and `florencenet`.
* [#163](https://github.com/tqtezos/stablecoin/pull/163)
  * Remove network tests on `007` local-chain and `delphinet`.

## 1.7.1

* [#160](https://github.com/tqtezos/stablecoin/pull/160)
  * Add network tests on `008` local-chain and `edonet`.
  * Bump `morley`, `lorentz` and other dependencies to run `008` network tests.

## 1.7.0

* [#155](https://github.com/tqtezos/stablecoin/pull/155)
  * Added `--description` option to `stablecoin-client`
  * Fixed names of FA2 views (e.g. `GetBalance` is now named `get_balance`)
  * Contract metadata now contains the version of the ligo compiler used
    to compile the contract.
  * Update the types of the `is_operator` and `get_balance` views to reflect
    the [recent changes to TZIP-012](https://gitlab.com/tezos/tzip/-/merge_requests/126).
  * Update the names of the metadata keys to use the camelCase naming scheme to reflect
    the [recent changes to TZIP-016](https://gitlab.com/tezos/tzip/-/merge_requests/115).

## 1.6.0

* [#151](https://github.com/tqtezos/stablecoin/pull/151)
  * Remove `token_metadata_registry` entrypoint.
  * Add TZIP-16 storage views as required by TZIP-12.

## 1.5.0

* [#144](https://github.com/tqtezos/stablecoin/pull/144)
  * Add `total_supply` field to the contract.

* [#142](https://github.com/tqtezos/stablecoin/pull/142)
  * Deploy TZIP-16 metadata to an external contract by default.
  * Provide deploy options `--contract-metadata-in-place` and
    `--contract-metadata-off-chain` to let user to embed the metadata
    in contract or to use a custom metadata offline uri respectievely.

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
