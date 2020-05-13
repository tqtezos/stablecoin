<!--
SPDX-FileCopyrightText: 2020 tqtezos
SPDX-License-Identifier: MIT
-->

# Stablecoin Haskell

This folder contains a Haskell library to interact with the stablecoin contract (basically Haskell bindings) and tests for it.

## Build Instructions

You need [Stack](http://haskellstack.org/) to build this package.
Run `stack build` to build the library.

## Tests

Tests require the stablecoin contract to be in `test/resources/stablecoin.tz`.
After you put it there you can do `stack test` to run tests.
We have a [`Makefile`](Makefile) that automates this process, so you can run `make test`.
