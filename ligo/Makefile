# SPDX-FileCopyrightText: 2020 tqtezos
# SPDX-License-Identifier: MIT

.PHONY: build-ligo test-ligo

.DEFAULT_GOAL = test-ligo

# Ligo executable
LIGO = ligo

# Compile LIGO contract into its michelson representation
build-ligo:
	$(LIGO) compile-contract stablecoin/core.ligo main > ../out/stablecoin.tz
