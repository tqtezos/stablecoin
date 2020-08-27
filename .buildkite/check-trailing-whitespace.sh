# SPDX-FileCopyrightText: 2020 TQ Tezos
# SPDX-License-Identifier: MIT
#!/usr/bin/env bash
files=$(git ls-files -- . | xargs grep --files-with-matches --binary-files=without-match '[[:blank:]]$')
if [[ ! -z $files ]];then
    echo '  Files with trailing whitespace found:'
    for f in "${files[@]}"; do
        echo "  * $f"
    done
    exit 1
fi