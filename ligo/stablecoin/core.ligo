(* SPDX-FileCopyrightText: 2020 tqtezos
 * SPDX-License-Identifier: MIT *)

#include "actions.ligo"

function main
  ( const action : parameter
  ; const store  : storage
  ) : entrypoint
is case action of
    Transfer                   (params) -> transfer(params, store)
  | Balance_of                 (params) -> balance_of(params, store)
  | Total_supply               (params) -> total_supply(params, store)
  | Token_metadata             (params) -> token_metadata(params, store)
  | Permissions_descriptor     (params) -> permission_descriptor(params, store)
  | Update_operators           (params) -> update_operators_action(params, store)
  | Is_operator                (params) -> is_operator_action(params, store)
end
