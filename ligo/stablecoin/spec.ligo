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
    then failwith ("TOKEN_UNDEFINED")
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

type transfer_param_ is record
  from_    : address
; to_      : address
; token_id : token_id
; amount   : nat
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

type total_supply_response_ is record
  token_id     : token_id
; total_supply : nat
end

type total_supply_response is michelson_pair_right_comb (total_supply_response_)

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
  tag        : string
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

type mint_params is record
  to_    : address
; amount : nat
end

type burn_params is record
  from_  : address
; amount : nat
end

type parameter is
  Transfer               of transfer_params
| Balance_of             of balance_of_params
| Total_supply           of total_supply_params
| Token_metadata         of token_metadata_params
| Permissions_descriptor of permissions_descriptor_params
| Update_operators       of update_operator_params
| Is_operator            of is_operator_params

(* ------------------------------------------------------------- *)

(*
 * Hooks
 *)

// Currently it has the same structure as `transfer_param` but
// with optional participants
type transfer_descriptor_ is record
  from_    : option (address)
; to_      : option (address)
; token_id : token_id
; amount   : nat
end

type transfer_descriptor is michelson_pair_right_comb(transfer_descriptor_)

type transfer_descriptor_param_ is record
   fa2      : address
;  batch    : list(transfer_descriptor)
;  operator : address
end

type transfer_descriptor_param is michelson_pair_right_comb(transfer_descriptor_param_)

(* ------------------------------------------------------------- *)

(* Stablecoin parameter *)
type closed_parameter is
| Call_FA2 of parameter

(* ------------------------------------------------------------- *)

(*
 * Storage
 *)

type owner is address
type operator is address

type operators is
  big_map ((owner * operator), unit)

type ledger is big_map (address, nat)

type storage is record
  ledger         : ledger
; permissions    : permissions_descriptor_
; token_metadata : token_metadata_
; total_supply   : nat
; operators      : operators
end

type entrypoint is list (operation) * storage
