# SPDX-FileCopyrightText: 2020 tqtezos
# SPDX-License-Identifier: MIT

.PHONY: build-ligo test-ligo

.DEFAULT_GOAL = test-ligo

# Ligo executable
LIGO = docker run ligolang/ligo:next

# Compile LIGO contract into its michelson representation
build-ligo:
	$(LIGO) compile-contract ligo/stablecoin.ligo main

test-ligo:
	$(LIGO) run-function ligo/stablecoin-test.ligo test "()"
