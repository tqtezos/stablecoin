#!/usr/bin/env bash

# SPDX-FileCopyrightText: 2021 Oxhead Alpha
# SPDX-License-Identifier: MIT

# This script runs given nettest scenario using node provided
# by the NETTEST_NODE_ADDR and NETTEST_NODE_PORT variables.
# This script expects 'tezos-client' to be in PATH.
set -euo pipefail

export TASTY_CLEVELAND_DATA_DIR="$(mktemp -d --tmpdir="$PWD")"
export TASTY_CLEVELAND_MODE="only-network"
exec "$1"
