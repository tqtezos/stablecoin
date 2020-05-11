# SPDX-FileCopyrightText: 2020 tqtezos
# SPDX-License-Identifier: MIT

.PHONY: all build-ligo build-haskell test test-dumb-term test-hide-successes clean

MAKE_HASKELL = $(MAKE) -C haskell/
MAKE_LIGO = $(MAKE) -C ligo/

all: build-ligo build-haskell

# Compile LIGO contract into its michelson representation
build-ligo:
	$(MAKE_LIGO) stablecoin.tz

# Build everything haskell-related (including tests and benchmarks) with development options.
build-haskell:
	$(MAKE_HASKELL) build

test:
	$(MAKE_HASKELL) test

test-dumb-term:
	$(MAKE_HASKELL) test-dumb-term

test-hide-successes:
	$(MAKE_HASKELL) test-hide-sucesses

clean:
	$(MAKE_HASKELL) clean
	$(MAKE_LIGO) clean
