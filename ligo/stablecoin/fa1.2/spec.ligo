// SPDX-FileCopyrightText: 2020 TQ Tezos
// SPDX-License-Identifier: MIT

(*
 * Stablecoin types
 *)

(* ------------------------------------------------------------- *)

(*
 * Parameter types
 *)

type transfer_param is michelson_pair_right_comb(
  record
    from_: address;
    to_: address;
    value: nat;
  end
)

type get_balance_param is michelson_pair_right_comb(
  record
    request: address;
    callback: contract(nat);
  end
)

type get_allowance_request is michelson_pair_right_comb(
  record
    owner: address;
    spender: address;
  end
)

type get_allowance_param is michelson_pair_right_comb(
  record
    request: get_allowance_request;
    callback: contract(nat);
  end
)

type approve_param is michelson_pair_right_comb(
  record
    spender: address;
    value: nat;
  end
)

type get_total_supply_param is michelson_pair_right_comb(
  record
    request: unit;
    callback: contract(nat);
  end
)

(* ------------------------------------------------------------- *)

type pause_params is unit

type unpause_params is unit

type configure_minter_params_ is record
  minter                    : address
; current_minting_allowance : option (nat)
; new_minting_allowance     : nat
end

type configure_minter_params is michelson_pair_right_comb (configure_minter_params_)

type remove_minter_params is address

type mint_param_ is record
  to_    : address
; amount : nat
end

type mint_param is michelson_pair_right_comb (mint_param_)

type mint_params is list (mint_param)

type burn_params is list (nat)

type transfer_ownership_param is address

type accept_ownership_param is unit

type change_master_minter_param is address

type change_pauser_param is address

type parameter is
| Transfer       of transfer_param
| Approve        of approve_param
| GetBalance     of get_balance_param
| GetAllowance   of get_allowance_param
| GetTotalSupply of get_total_supply_param

(* ------------------------------------------------------------- *)

(*
 * Transferlist
 *)

type transferlist_transfer_item is michelson_pair(address, "from", list(address), "tos")

type transferlist_assert_transfers_param is list(transferlist_transfer_item)

type transferlist_assert_receivers_param is list(address)

type set_transferlist_param is option(address)

type blake2b_hash is bytes

type seconds is nat

type permit_info is
  record [
    created_at: timestamp
  ; expiry: option(seconds)
  ]

type user_permits is
  record [
    permits: map (blake2b_hash, permit_info)
  ; expiry: option(seconds)
  ]

type permits is big_map (address, user_permits)

type permit_param is (key * (signature * blake2b_hash))

type set_expiry_param is (seconds * option(blake2b_hash * address))

(*
 * A counter that's incremented everytime a permit is created.
 *
 * When creating a new permit, the `permit` entrypoint verifies that the permit has
 * been signed with the contract's current counter. It then increments the counter by 1.
 *
 * This ensures that a permit's signature is valid for one use only.
 *)
type counter is nat

(* ------------------------------------------------------------- *)

(* Stablecoin parameter *)
type closed_parameter is
| Call_FA1_2            of parameter
| Pause                 of pause_params
| Unpause               of unpause_params
| Configure_minter      of configure_minter_params
| Remove_minter         of remove_minter_params
| Mint                  of mint_params
| Burn                  of burn_params
| Transfer_ownership    of transfer_ownership_param
| Accept_ownership      of accept_ownership_param
| Change_master_minter  of change_master_minter_param
| Change_pauser         of change_pauser_param
| Set_transferlist      of set_transferlist_param
| Permit                of permit_param
| Set_expiry            of set_expiry_param

(* ------------------------------------------------------------- *)

(*
 * Storage
 *)

type owner is address
type spender is address

type spender_allowances is
  big_map ((owner * spender), nat)

type roles is record
  owner         : address
; pending_owner : option (address)
; pauser        : address
; master_minter : address
end

type ledger is big_map (address, nat)

type minting_allowances is map (address, nat)

type metadata is big_map (string, bytes)

type storage is record
  ledger                  : ledger
; minting_allowances      : minting_allowances
; total_supply            : nat
; paused                  : bool
; roles                   : roles
; spender_allowances      : spender_allowances
; transferlist_contract   : option(address)
; permit_counter          : counter
; permits                 : permits
; default_expiry          : seconds
; metadata                : metadata
end

type entrypoint is list (operation) * storage
