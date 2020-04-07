// SPDX-FileCopyrightText: 2020 tqtezos
// SPDX-License-Identifier: MIT

/*
 * Stablecoin smart-contract
 */

/* ------------------------------------------------------------- */

/*
 * Parameter
 */

type token_id is nat

const default_token_id : token_id = abs (0);

type transfer_param is record
  token_id : token_id
; from_    : address
; to_      : address
; amount   : nat
end

type transfer_params is list (transfer_param)

type balance_of_request is record
  token_id : token_id
; owner   :  address
end

type balance_of_response is record
  request : balance_of_request
; balance : nat
end

type balance_of_params is record
  requests : list (balance_of_request)
; callback : contract (list (balance_of_response))
end



type total_supply_response is record
  token_id     : token_id
; total_supply : nat
end

type total_supply_params is record
  token_ids : list (token_id)
; callback  : contract (list (total_supply_response))
end



type token_metadata is record
  token_id  : token_id
; symbol    : string
; name      : string
; decimals  : nat
; extras    : map (string, string)
end

type get_token_metadata_response is token_metadata

type get_token_metadata_params is record
  token_ids : list (token_id)
; callback  : contract (list (get_token_metadata_response))
end



type operator_tokens is
| All_tokens
| Some_tokens of set (token_id)

type operator_param is record
  owner    : address
; operator : address
; tokens   : operator_tokens
end

type update_operator_param is
| Add_operator    of operator_param
| Remove_operator of operator_param

type update_operator_params is list (update_operator_param)

type is_operator_response is record
  operator   : operator_param
; is_operator : bool
end

type is_operator_params is record
  operator : operator_param
; callback : contract (is_operator_response)
end

/* ------------------------------------------------------------- */

type self_transfer_policy is
| Self_transfer_permitted
| Self_transfer_denied

type operator_transfer_policy is
| Operator_transfer_permitted
| Operator_transfer_denied

type owner_transfer_policy is
| Owner_no_op
| Optional_owner_hook
| Required_owner_hook

type custom_permission_policy is record
  tag       : string
; config_api : option (address)
end

type permissions_descriptor is record
  self     : self_transfer_policy
; operator : operator_transfer_policy
; receiver : owner_transfer_policy
; sender   : owner_transfer_policy
; custom   : option (custom_permission_policy)
end

type permissions_descriptor_params is
  contract (permissions_descriptor)

type pause_params is unit

type unpause_params is unit

type configure_minter_params is address

type set_minting_allowance_params is record
  minter : address
; amount : nat
end

type remove_minter_params is address

type mint_params is record
  to_    : address
; amount : nat
end

type burn_params is record
  from_  : address
; amount : nat
end

type parameter is
  // FA2
  Transfer                 of transfer_params
| Balance_of                of balance_of_params
| Total_supply           of total_supply_params
| Get_token_metadata         of get_token_metadata_params
| Permissions_descriptor of permissions_descriptor_params
| Update_operators          of update_operator_params
| Is_operator               of is_operator_params
  // Custom
| Pause               of pause_params
| Unpause             of unpause_params
| Configure_minter     of configure_minter_params
| Set_minting_allowance of set_minting_allowance_params
| Remove_minter        of remove_minter_params
| Mint                of mint_params
| Burn                of burn_params

/* ------------------------------------------------------------- */

/*
 * Hooks
 */

type registry is set (address)

// TODO: currently it's the same as `transfer` but with optional
// participants.
type transfer_descriptor is record
  from_    : option (address)
; to_      : option (address)
; token_id : token_id
; amount   : nat
end

type transfer_descriptor_param is record
  contract_address : address
; batch    : list (transfer_descriptor)
; operator : address
end

type token_receiver is
| Tokens_received of transfer_descriptor_param

type token_sender is
| Tokens_sent of transfer_descriptor_param

type set_hook_param is record
  hook : unit -> contract (transfer_descriptor_param)
; permissions_descriptor : permissions_descriptor
end

// Entrypoints with param hook
type closed_parameter is
| Parameter of parameter
| Set_transfer_hook of set_hook_param

/* ------------------------------------------------------------- */

/*
 * Storage
 */

type operator_actions is
| All_operator_tokens
| Some_operator_tokens of set (token_id)
| All_operator_tokens_except of set (token_id)

type owner is address
type operator is address

type operator_storage is
  big_map ((owner * operator), operator_actions)

type roles is record
  owner         : address
; pauser        : address
; master_minter : address
end

type storage_skeleton is record
  roles        : roles
; operators    : operator_storage
; paused       : bool
; total_supply : nat
end

type approvals is map (address, nat)

type ledger_value is record
  balance   : nat
; operator  : address
; approvals : approvals
end

type ledger is map (address, ledger_value)

type storage is record
  ledger         : ledger
; permissions    : permissions_descriptor
; token_metadata : token_metadata
; fields         : storage_skeleton
end

type entrypoint is list (operation) * storage
