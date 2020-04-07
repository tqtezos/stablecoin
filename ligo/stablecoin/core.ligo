// SPDX-FileCopyrightText: 2020 tqtezos
// SPDX-License-Identifier: MIT

#include "actions.ligo"

function main
  ( const action : parameter
  ; const store  : storage
  ) : entrypoint
is case action of
    Transfer                 (params) -> transfer                (params, store)
  | BalanceOf                (params) -> balanceOf               (params, store)
  | GetTotalSupply           (params) -> getTotalSupply          (params, store)
  | GetTokenMetadata         (params) -> getTokenMetadata        (params, store)
  | GetPermissionsDescriptor (params) -> getPermissionsDescriptor(params, store)
  | UpdateOperators          (params) -> updateOperators         (params, store)
  | IsOperator               (params) -> isOperator              (params, store)
  | Pause                    (params) -> pause                   (params, store)
  | Unpause                  (params) -> unpause                 (params, store)
  | ConfigureMinter          (params) -> configureMinter         (params, store)
  | SetMintingAllowance      (params) -> setMintingAllowance     (params, store)
  | RemoveMinter             (params) -> removeMinter            (params, store)
  | Mint                     (params) -> mint                    (params, store)
  | Burn                     (params) -> burn                    (params, store)
end