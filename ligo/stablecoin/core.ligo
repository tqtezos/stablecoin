// SPDX-FileCopyrightText: 2020 TQ Tezos
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
    Transfer                (params) -> transfer                (params, store)
  | Balance_of              (params) -> balance_of              (params, store)
  | Token_metadata_registry (params) -> token_metadata_registry (params, store)
  | Update_operators        (params) -> update_operators_action (params, store)
  | Is_operator             (params) -> is_operator_action      (params, store)
end

(*
 * Root entrypoint of stablecoin smart-contract
 *)
function stablecoin_main
  ( const full_param : closed_parameter
  ; const store  : storage
  ) : entrypoint is block
{ fail_on (Tezos.amount =/= 0tz, "XTZ_RECEIVED") // Validate whether the contract receives non-zero amount of tokens
} with case full_param of
    Call_FA2              (params) -> fa2_main              (params, store)
  | Pause                 (params) -> pause                 (params, store)
  | Unpause               (params) -> unpause               (params, store)
  | Configure_minter      (params) -> configure_minter      (params, store)
  | Remove_minter         (params) -> remove_minter         (params, store)
  | Mint                  (params) -> mint                  (params, store)
  | Burn                  (params) -> burn                  (params, store)
  | Transfer_ownership    (params) -> transfer_ownership    (params, store)
  | Accept_ownership      (params) -> accept_ownership      (params, store)
  | Change_master_minter  (params) -> change_master_minter  (params, store)
  | Change_pauser         (params) -> change_pauser         (params, store)
  | Set_transferlist      (params) -> set_transferlist      (params, store)
  | Permit                (params) -> add_permit            (params, store)
  end
