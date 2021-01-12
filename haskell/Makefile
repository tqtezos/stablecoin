# SPDX-FileCopyrightText: 2020 TQ Tezos
# SPDX-License-Identifier: MIT

.PHONY: build test test-dumb-term test-hide-successes nettest haddock haddock-no-deps stylish lint clean all

MORLEY ?= morley
LIGO_DIR = ../ligo
MAKE_LIGO = $(MAKE) -C $(LIGO_DIR)

# Options for development
STACK_DEV_OPTIONS = --fast --ghc-options -Wwarn --file-watch
# Options to build more stuff (tests and benchmarks)
STACK_BUILD_MORE_OPTIONS = --test --bench --no-run-tests --no-run-benchmarks
# Options for tests
STACK_DEV_TEST_OPTIONS = --fast --ghc-options -Wwarn
# Addtional (specified by user) options passed to test executable
TEST_ARGUMENTS ?= ""
# Packages to apply the command (build, test, e.t.c) for.
PACKAGE = stablecoin

#   Initial `../..` in `ln` is needed because of `test/resources`.
#   $(LIGO_DIR) is relative to the current directory.
define call_test
	$(call build_stablecoin_contract)
	stack test $(PACKAGE) \
		--test-arguments "--color always $(TEST_ARGUMENTS) $1" $2
endef

define build_stablecoin_contract
	$(MAKE_LIGO) stablecoin.tz
	$(MAKE_LIGO) stablecoin.fa1.2.tz
	$(MAKE_LIGO) metadata.tz
	mkdir -p test/resources
	$(MORLEY) optimize --contract "$(LIGO_DIR)/stablecoin.tz" --output "test/resources/stablecoin.tz"
	$(MORLEY) optimize --contract "$(LIGO_DIR)/stablecoin.fa1.2.tz" --output "test/resources/stablecoin.fa1.2.tz"
	cp "$(LIGO_DIR)/metadata.tz" "test/resources/metadata.tz"
	cp "../nix/sources.json" "test/resources/sources.json"
endef

# Build everything haskell-related (including tests and benchmarks) with development options.
build:
	$(call build_stablecoin_contract)
	stack build $(STACK_DEV_OPTIONS) $(STACK_BUILD_MORE_OPTIONS) $(PACKAGE)

test:
	$(call call_test,"",$(STACK_DEV_TEST_OPTIONS))

# Like 'test' command, but enforces dumb terminal which may be useful to
# workardoung some issues with `tasty`.
# Primarily this one: https://github.com/feuerbach/tasty/issues/152
test-dumb-term:
	TERM=dumb $(call call_test,"",$(STACK_DEV_TEST_OPTIONS))

# Run tests with `--hide-successes` option. It forces dumb terminal,
# because otherwise this option is likely to work incorrectly.
test-hide-successes:
	TERM=dumb $(call call_test,"--hide-successes",$(STACK_DEV_TEST_OPTIONS))

# Run network tests
nettest:
	$(call build_stablecoin_contract)
	stack test stablecoin:stablecoin-nettest \
		--test-arguments "$(TEST_ARGUMENTS)" \
		$(STACK_DEV_TEST_OPTIONS)

# Run haddock for all packages.
haddock:
	$(call build_stablecoin_contract)
	stack haddock $(STACK_DEV_OPTIONS) $(PACKAGE)

# Run haddock for all our packages, but not for dependencies.
haddock-no-deps:
	$(call build_stablecoin_contract)
	stack haddock $(STACK_DEV_OPTIONS) $(PACKAGE) --no-haddock-deps

stylish:
	find . -name '.stack-work' -prune -o -name '*.hs' -exec stylish-haskell -i {} \;

lint:
	scripts/lint.sh

clean:
	stack clean $(PACKAGE)
	rm -f stablecoin.tz
	rm -f stablecoin.fa1.2.tz
	rm -f metadata.tz
