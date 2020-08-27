# SPDX-FileCopyrightText: 2020 TQ Tezos
# SPDX-License-Identifier: MIT

#!/usr/bin/env bash

export TERM="${TERM:-xterm}"

hlint --hint haskell/.hlint.yaml --hint haskell/.hlint-universum.yaml --ignore='Parse error' */

ex=$?

if [ $ex != 0 ]; then
  echo ''
  echo '====================================================================='
  echo ''
  echo 'Note: to ignore a particular hint (e.g. "Reduce duplication"), write'
  echo 'this in the source file:'
  echo ''
  tput bold
  echo '{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}'
  tput sgr0
  echo ''
  echo 'You can also apply it just to a particular function, which is better:'
  echo ''
  tput bold
  echo '{-# ANN funcName ("HLint: ignore Reduce duplication" :: Text) #-}'
  tput sgr0
  echo ''

  exit $ex
fi
