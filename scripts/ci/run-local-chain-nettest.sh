#!/usr/bin/env bash

# SPDX-FileCopyrightText: 2020 Tocqueville Group
#
# SPDX-License-Identifier: LicenseRef-MIT-TQ

# This script runs given nettest scenario using node provided
# by the NETTEST_NODE_ADDR and NETTEST_NODE_PORT variables.
# This script expects 'tezos-client' to be in PATH.
set -euo pipefail

nettest_scenario="$1"

node_endpoint="$TASTY_NETTEST_NODE_ENDPOINT"
TEMPDIR="$(mktemp -d --tmpdir="$PWD")"
tezos_client_args=(-E "$node_endpoint" -d "$TEMPDIR")

tezos-client "${tezos_client_args[@]}" import secret key nettest \
             "$TASTY_NETTEST_MONEYBAG_SECRET_KEY" --force
"$nettest_scenario" "${tezos_client_args[@]}"
