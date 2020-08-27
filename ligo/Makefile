# SPDX-FileCopyrightText: 2020 TQ Tezos
# SPDX-License-Identifier: MIT

# Ligo executable
LIGO ?= ligo

# All files with `.ligo` extension are considered source files.
# Recompilation will be triggered only if they are changed.
sources = stablecoin/*.ligo

# Compile LIGO contract into its michelson representation
stablecoin.tz: $(sources)
	$(LIGO) compile-contract --syntax pascaligo stablecoin/core.ligo stablecoin_main > stablecoin.tz

# Compile metadata LIGO contract into its michelson representation
metadata.tz: $(sources)
	$(LIGO) compile-contract --syntax pascaligo stablecoin/metadata.ligo metadata > metadata.tz

.PHONY: clean
clean:
	rm -f *.tz
