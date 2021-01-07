# SPDX-FileCopyrightText: 2020 TQ Tezos
# SPDX-License-Identifier: MIT

.PHONY: all build-ligo build-haskell optimize-ligo test test-dumb-term test-hide-successes nettest clean

# Morley executable, it is used for optimizing Michelson version
# of stablecoin.tz
MORLEY ?= morley

MAKE_HASKELL = $(MAKE) -C haskell/
MAKE_LIGO = $(MAKE) -C ligo/

all: build-ligo build-haskell

# Compile LIGO contract into its michelson representation
build-ligo:
	$(MAKE_LIGO) stablecoin.tz
	$(MAKE_LIGO) stablecoin.fa1.2.tz
	$(MAKE_LIGO) metadata.tz

# Compile LIGO contract and then build everything haskell-related (including tests and benchmarks)
# with development options.
build-haskell:
	$(MAKE_HASKELL) build

# Optimize built ligo contract using morley optimizer
optimize-ligo:
	$(MORLEY) optimize --contract ligo/stablecoin.tz --output ligo/stablecoin.tz

test:
	$(MAKE_HASKELL) test

test-dumb-term:
	$(MAKE_HASKELL) test-dumb-term

test-hide-successes:
	$(MAKE_HASKELL) test-hide-sucesses

nettest:
	$(MAKE_HASKELL) nettest

clean:
	$(MAKE_HASKELL) clean
	$(MAKE_LIGO) clean
