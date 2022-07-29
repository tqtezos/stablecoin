// SPDX-FileCopyrightText: 2020 TQ Tezos
// SPDX-License-Identifier: MIT

(*
 * Stablecoin types that are specified in
 * https://github.com/tqtezos/stablecoin/blob/master/docs/specification.md
 *)

(* ------------------------------------------------------------- *)

(*
 * Parameter types
 *)

type token_id is nat

const default_token_id : token_id = 0n;

(*
 * This function fails if the provided `token_id` is not equal to the
 * default one, restricting all operations to be one-token
 * (that are allowed for `default_token_id`).
 *)
[@inline] function validate_token_type
  ( const token_id : token_id
  ) : unit is
    if token_id =/= default_token_id
    then failwith ("FA2_TOKEN_UNDEFINED")
    else unit

type transfer_destination is [@layout:comb] record [
  to_      : address;
  token_id : token_id;
  amount   : nat;
]

type transfer_param is [@layout:comb] record [
  from_ : address;
  txs : list (transfer_destination);
]

type transfer_params is list (transfer_param)

type balance_of_request is record [
   owner    :  address;
   token_id : token_id;
]

type balance_of_response is [@layout:comb] record [
  request : balance_of_request;
  balance : nat;
]

type balance_of_params is [@layout:comb] record [
  requests : list (balance_of_request);
  callback : contract (list (balance_of_response));
]

type token_metadata is [@layout:comb] record [
  token_id  : token_id;
  symbol    : string;
  name      : string;
  decimals  : nat;
  extras    : map (string, string);
]

type token_metadata_registry_params is contract (address)

type operator_param is [@layout:comb] record [
  owner    : address;
  operator : address;
  token_id : token_id;
]

type update_operator_param is
| Add_operator    of operator_param
| Remove_operator of operator_param

type update_operator_params is list (update_operator_param)

(* ------------------------------------------------------------- *)

type operator_transfer_policy is [@layout:comb]
| No_transfer
| Owner_transfer
| Owner_or_operator_transfer

type pause_params is unit

type unpause_params is unit

type configure_minter_params is [@layout:comb] record [
  minter                    : address;
  current_minting_allowance : option (nat);
  new_minting_allowance     : nat;
]

type remove_minter_params is address

type mint_param is [@layout:comb] record [
  to_    : address;
  amount : nat;
]

type mint_params is list (mint_param)

type burn_params is list (nat)

type transfer_ownership_param is address

type accept_ownership_param is unit

type change_master_minter_param is address

type change_pauser_param is address

type parameter is
  Transfer                of transfer_params
| Balance_of              of balance_of_params
| Update_operators        of update_operator_params

(* ------------------------------------------------------------- *)

(*
 * Hooks
 *)

// Currently it has the same structure as `transfer_param` but
// with optional participants
type transfer_destination_descriptor is [@layout:comb] record [
  to_      : option (address);
  token_id : token_id;
  amount   : nat;
]

type transfer_descriptor is [@layout:comb] record [
  from_ : option (address);
  txs   : list (transfer_destination_descriptor);
]

type transfer_descriptor_param is [@layout:comb] record [
   fa2      : address;
   batch    : list(transfer_descriptor);
   operator : address;
]

(*
 * Transferlist
 *)

type transferlist_transfer_item is [@layout:comb] record [
  [@annot:from] from_: address;
  tos: list(address);
]

type transferlist_assert_transfers_param is list(transferlist_transfer_item)

type transferlist_assert_receiver_param is address

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

type permit_signature is [@layout:comb] record [
  signature : signature;
  permit_hash : blake2b_hash;
]
type permit_param is (key * permit_signature)

type revoke_param is blake2b_hash * address
type revoke_params is list(revoke_param)

type set_expiry_param is [@layout:comb] record [
  issuer: address;
  expiry: seconds;
  permit_hash: option(blake2b_hash);
]

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
| Set_transferlist      of set_transferlist_param
| Permit                of permit_param
| Set_expiry            of set_expiry_param

(* ------------------------------------------------------------- *)

(*
 * Storage
 *)

type owner is address
type operator is address

type operators is
  big_map ((owner * operator), unit)

type roles is record [
  owner         : address;
  pending_owner : option (address);
  pauser        : address;
  master_minter : address;
]

type ledger is big_map (address, nat)

type minting_allowances is map (address, nat)

type metadata is big_map (string, bytes)

type storage is record [
  ledger                  : ledger;
  minting_allowances      : minting_allowances;
  paused                  : bool;
  roles                   : roles;
  operators               : operators;
  transferlist_contract   : option(address);
  permit_counter          : counter;
  permits                 : permits;
  default_expiry          : seconds;
  metadata                : metadata;
  total_supply            : nat;
]

type entrypoint is list (operation) * storage
