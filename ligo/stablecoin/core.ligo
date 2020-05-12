// SPDX-FileCopyrightText: 2020 tqtezos
// SPDX-License-Identifier: MIT

#include "actions.ligo"

(*
 * FA2 entrypoints that are specified in [tzip-12](https://gitlab.com/tzip/tzip/-/blob/827c6c5f9e504e9c4a63e265f86dce24f5c5cef9/proposals/tzip-12/tzip-12.md)
 *)
function fa2_main
  ( const action : parameter
  ; const store  : storage
  ) : entrypoint
is case action of
    Transfer                   (params) -> transfer                 (params, store)
  | Balance_of                 (params) -> balance_of               (params, store)
  | Total_supply               (params) -> total_supply             (params, store)
  | Token_metadata             (params) -> token_metadata           (params, store)
  | Permissions_descriptor     (params) -> permission_descriptor    (params, store)
  | Update_operators           (params) -> update_operators_action  (params, store)
  | Is_operator                (params) -> is_operator_action       (params, store)
end

(*
 * Root entrypoint of stablecoin smart-contract
 *)
function stablecoin_main
  ( const action : closed_parameter
  ; const store  : storage
  ) : entrypoint
is case action of
    Call_FA2 (params) -> fa2_main (params, store)
  end
