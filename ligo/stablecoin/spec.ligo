// SPDX-FileCopyrightText: 2020 tqtezos
// SPDX-License-Identifier: MIT

(*
 * Stablecoin types that are specified in
 * https://github.com/tqtezos/stablecoin/blob/317b9346b287a4f2c2606c4929237d87008fdad4/docs/specification.md
 *)

(* ------------------------------------------------------------- *)

(*
 * Parameter types
 *)

type token_id is nat

const default_token_id : token_id = 0n;

(*
 * This function fails if provided token_id is not equal to
 * default one, restricting all operations to be one-token
 * (that are allowed for `default_token_id`)
 *)
function validate_token_type
  ( const token_id : token_id
  ) : unit is
    if token_id =/= default_token_id
    then failwith ("FA2_TOKEN_UNDEFINED")
    else unit

(*
 * Same as above but for a list of token ids
 *)
function validate_token_types
  ( const token_ids : list (token_id)
  ) : unit is List.fold
    ( function
        ( const u        : unit
        ; const token_id : token_id
        ) : unit is validate_token_type (token_id)
    , token_ids
    , unit
    )

type transfer_destination_ is record
  to_      : address
; token_id : token_id
; amount   : nat
end

type transfer_destination is michelson_pair_right_comb(transfer_destination_)

type transfer_param_ is record
  from_ : address
; txs : list (transfer_destination)
end

type transfer_param is michelson_pair_right_comb(transfer_param_)

type transfer_params is list (transfer_param)

type balance_of_request is record
   owner    :  address
;  token_id : token_id
end

type balance_of_response_ is record
  request : balance_of_request
; balance : nat
end

type balance_of_response is michelson_pair_right_comb(balance_of_response_)

type balance_of_params_ is record
  requests : list (balance_of_request)
; callback : contract (list (balance_of_response))
end

type balance_of_params is michelson_pair_right_comb(balance_of_params_)

type token_metadata_ is record
  token_id  : token_id
; symbol    : string
; name      : string
; decimals  : nat
; extras    : map (string, string)
end

type token_metadata is michelson_pair_right_comb(token_metadata_)

type token_metadata_registry_params is contract (address)

type operator_param_ is record
  owner    : address
; operator : address
end

type operator_param is michelson_pair_right_comb(operator_param_)

type update_operator_param is
| Add_operator    of operator_param
| Remove_operator of operator_param

type update_operator_params is list (update_operator_param)

type is_operator_response_ is record
  operator    : operator_param
; is_operator : bool
end

type is_operator_response is michelson_pair_right_comb(is_operator_response_)

type is_operator_params_ is record
  operator : operator_param
; callback : contract (is_operator_response)
end

type is_operator_params is michelson_pair_right_comb(is_operator_params_)

(* ------------------------------------------------------------- *)

type operator_transfer_policy_ is
| No_transfer
| Owner_transfer
| Owner_or_operator_transfer

type operator_transfer_policy is michelson_or_right_comb(operator_transfer_policy_)

type owner_hook_policy_ is
| Owner_no_hook
| Optional_owner_hook
| Required_owner_hook

type owner_hook_policy is michelson_or_right_comb(owner_hook_policy_)

type custom_permission_policy_ is record
  tag        : string
; config_api : option (address)
end

type custom_permission_policy is michelson_pair_right_comb(custom_permission_policy_)

type permissions_descriptor_ is record
  operator : operator_transfer_policy
; receiver : owner_hook_policy
; sender   : owner_hook_policy
; custom   : option (custom_permission_policy)
end

type permissions_descriptor is michelson_pair_right_comb(permissions_descriptor_)

type permissions_descriptor_params is
  contract (permissions_descriptor)

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
  Transfer                of transfer_params
| Balance_of              of balance_of_params
| Token_metadata_registry of token_metadata_registry_params
| Permissions_descriptor  of permissions_descriptor_params
| Update_operators        of update_operator_params
| Is_operator             of is_operator_params

(* ------------------------------------------------------------- *)

(*
 * Hooks
 *)

// Currently it has the same structure as `transfer_param` but
// with optional participants
type transfer_destination_descriptor_ is record
  to_      : option (address)
; token_id : token_id
; amount   : nat
end

type transfer_destination_descriptor is michelson_pair_right_comb(transfer_destination_descriptor_)

type transfer_descriptor_ is record
  from_ : option (address)
; txs   : list (transfer_destination_descriptor)
end

type transfer_descriptor is michelson_pair_right_comb(transfer_descriptor_)

type transfer_descriptor_param_ is record
   fa2      : address
;  batch    : list(transfer_descriptor)
;  operator : address
end

type transfer_descriptor_param is michelson_pair_right_comb(transfer_descriptor_param_)

(*
 * SafeList
 *)

type safelist_transfer_item is michelson_pair(address, "from", list(address), "tos")

type safelist_assert_transfers_param is list(safelist_transfer_item)

type safelist_assert_receiver_param is address

type safelist_assert_receivers_param is list(address)

type set_safelist_param is option(address)

(* ------------------------------------------------------------- *)

(* Stablecoin parameter *)
type closed_parameter is
| Call_FA2              of parameter
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
| Set_safelist          of set_safelist_param

(* ------------------------------------------------------------- *)

(*
 * Storage
 *)

type owner is address
type operator is address

type operators is
  big_map ((owner * operator), unit)

type roles is record
  owner         : address
; pending_owner : option (address)
; pauser        : address
; master_minter : address
end

type ledger is big_map (address, nat)

type minting_allowances is big_map (address, nat)

type storage is record
  ledger             : ledger
; token_metadata     : big_map (token_id, token_metadata)
; minting_allowances : minting_allowances
; total_supply       : nat
; paused             : bool
; roles              : roles
; operators          : operators
; safelist_contract  : option(address)
end

type entrypoint is list (operation) * storage
