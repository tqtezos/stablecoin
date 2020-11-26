// SPDX-FileCopyrightText: 2020 TQ Tezos
// SPDX-License-Identifier: MIT

#include "actions.ligo"

(*
 * FA2 entrypoints that are specified in [tzip-12](https://gitlab.com/tzip/tzip/-/blob/827c6c5f9e504e9c4a63e265f86dce24f5c5cef9/proposals/tzip-12/tzip-12.md)
 *)
function fa2_main
  ( const action : parameter
  ; const store  : storage
  ; const full_param : closed_parameter
  ) : entrypoint
is case action of
    Transfer                (params) -> transfer                (params, store, full_param)
  | Balance_of              (params) -> balance_of              (params, store)
  | Token_metadata_registry (params) -> token_metadata_registry (params, store)
  | Update_operators        (params) -> update_operators_action (params, store, full_param)
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
    Call_FA2              (params) -> fa2_main              (params, store, full_param)
  | Pause                 (params) -> pause                 (params, store, full_param)
  | Unpause               (params) -> unpause               (params, store, full_param)
  | Configure_minter      (params) -> configure_minter      (params, store, full_param)
  | Remove_minter         (params) -> remove_minter         (params, store, full_param)
  | Mint                  (params) -> mint                  (params, store)
  | Burn                  (params) -> burn                  (params, store)
  | Transfer_ownership    (params) -> transfer_ownership    (params, store, full_param)
  | Accept_ownership      (params) -> accept_ownership      (params, store, full_param)
  | Change_master_minter  (params) -> change_master_minter  (params, store, full_param)
  | Change_pauser         (params) -> change_pauser         (params, store, full_param)
  | Set_transferlist      (params) -> set_transferlist      (params, store, full_param)
  | Permit                (params) -> add_permit            (params, store)
  | Set_expiry            (params) -> set_expiry            (params, store, full_param)
  end
