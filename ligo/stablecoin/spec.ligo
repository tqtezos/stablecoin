(* SPDX-FileCopyrightText: 2020 tqtezos
   SPDX-License-Identifier: MIT *)

(* Stablecoin smart-contract *)

(* ------------------------------------------------------------- *)

// let validate_operator (txs, tx_policy, ops_storage 
//     : (transfer_descriptor list) * operator_transfer_policy * operators) : unit =
//   let can_owner_tx, can_operator_tx = match tx_policy with
//   | No_transfer -> (failwith "TX_DENIED" : bool * bool)
//   | Owner_transfer -> true, false
//   | Owner_or_operator_transfer -> true, true
//   in
//   let operator = Current.sender in
//   let owners = List.fold
//     (fun (owners, tx : (address set) * transfer_descriptor) ->
//       match tx.from_ with
//       | None -> owners
//       | Some o -> Set.add o owners
//     ) txs (Set.empty : address set) in

//   Set.iter
//     (fun (owner : address) ->
//       if can_owner_tx && owner = operator
//       then unit
//       else if not can_operator_tx
//       then failwith "NOT_OWNER"
//       else
//           let key = owner, operator in
//           let is_op_opt = Big_map.find_opt key ops_storage in
//           match is_op_opt with
//           | None -> failwith "NOT_OPERATOR"
//           | Some o -> unit
//     ) owners

(* Parameter *)

type token_id is nat

const default_token_id : token_id = abs (0);

type transfer_param_ is record
  from_    : address
; to_      : address
; token_id : token_id
; amount   : nat
end

type transfer_param is michelson_pair_right_comb(transfer_param_)

type transfer_params is list (transfer_param)

type balance_of_request is record
   owner   :  address
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

type total_supply_response is record
  token_id     : token_id
; total_supply : nat
end

type total_supply_params_ is record
  token_ids : list (token_id)
; callback  : contract (list (total_supply_response))
end

type total_supply_params is michelson_pair_right_comb(total_supply_params_)

type token_metadata_ is record
  token_id  : token_id
; symbol    : string
; name      : string
; decimals  : nat
; extras    : map (string, string)
end

type token_metadata is michelson_pair_right_comb(token_metadata_)

type token_metadata_response is token_metadata

type token_metadata_params_ is record
  token_ids : list (token_id)
; callback  : contract (list (token_metadata_response))
end

type token_metadata_params is michelson_pair_right_comb(token_metadata_params_)

type operator_tokens_ is
| All_tokens
| Some_tokens of set (token_id)

type operator_tokens is michelson_or_right_comb(operator_tokens_)

type operator_param_ is record
  owner    : address
; operator : address
; tokens   : operator_tokens
end

type operator_param is michelson_pair_right_comb(operator_param_)

type update_operator_param is
| Add_operator    of operator_param
| Remove_operator of operator_param

type update_operator_params is list (update_operator_param)

type is_operator_response_ is record
  operator   : operator_param
; is_operator : bool
end

type is_operator_response is michelson_pair_right_comb(is_operator_response_)

type is_operator_params_ is record
  operator : operator_param
; callback : contract (is_operator_response)
end

type is_operator_params is michelson_pair_right_comb(is_operator_params_)

(* ------------------------------------------------------------- *)

type self_transfer_policy_ is
| Self_transfer_permitted
| Self_transfer_denied

type self_transfer_policy is michelson_or_right_comb(self_transfer_policy_)

type operator_transfer_policy_ is
| Operator_transfer_permitted
| Operator_transfer_denied

type operator_transfer_policy is michelson_or_right_comb(operator_transfer_policy_)

type owner_transfer_policy_ is
| Owner_no_op
| Optional_owner_hook
| Required_owner_hook

type owner_transfer_policy is michelson_or_right_comb(owner_transfer_policy_)

type custom_permission_policy_ is record
  tag       : string
; config_api : option (address)
end

type custom_permission_policy is michelson_pair_right_comb(custom_permission_policy_)

type permissions_descriptor_ is record
  self     : self_transfer_policy
; operator : operator_transfer_policy
; receiver : owner_transfer_policy
; sender   : owner_transfer_policy
; custom   : option (custom_permission_policy)
end

type permissions_descriptor is michelson_pair_right_comb(permissions_descriptor_)

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
  (* FA2 *)
  Transfer                 of transfer_params
| Balance_of                of balance_of_params
| Total_supply           of total_supply_params
| Token_metadata         of token_metadata_params
| Permissions_descriptor of permissions_descriptor_params
| Update_operators          of update_operator_params
| Is_operator               of is_operator_params

(* ------------------------------------------------------------- *)

(* Hooks *)

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

(* ------------------------------------------------------------- *)

(*
 * Storage
 *)

type operator_actions is
| All_operator_tokens
| Some_operator_tokens of set (token_id)
| All_operator_tokens_except of set (token_id)

type owner is address
type operator is address

type operators is
  big_map ((owner * operator), unit)

type storage_skeleton is record
  paused       : bool
end

type ledger is big_map (address, nat)

type storage is record
  ledger         : ledger
; permissions    : permissions_descriptor_
; token_metadata : token_metadata_
; fields         : storage_skeleton
; total_supply : nat
; operators    : operators
end

type entrypoint is list (operation) * storage

type operators is
  big_map ((owner * operator), unit)
