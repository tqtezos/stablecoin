# SPDX-FileCopyrightText: 2020 tqtezos
# SPDX-License-Identifier: MIT

.PHONY: build-haskell build-ligo test test-ci haddock haddock-no-deps stylish lint clean all

.DEFAULT_GOAL = test-ligo

MAKE_HASKELL = $(MAKE) -C haskell/
MAKE_LIGO = $(MAKE) -C ligo/

# Build everything haskell-related (including tests and benchmarks) with development options.
build-haskell:
	$(MAKE_HASKELL) build

# Compile LIGO contract into its michelson representation
build-ligo:
	$(MAKE_LIGO) build-ligo

test:
	$(MAKE_HASKELL) test

test-ci:
	$(MAKE_HASKELL) test-ci

test-dumb-term:
	$(MAKE_HASKELL) test-dumb-term

test-hide-successes:
	$(MAKE_HASKELL) test-hide-sucesses

test-ligo:
	$(MAKE_LIGO) test-ligo
