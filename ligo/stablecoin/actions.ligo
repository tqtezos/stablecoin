// SPDX-FileCopyrightText: 2020 tqtezos
// SPDX-License-Identifier: MIT

(*
 * Root entrypoints for stablecoin smart-contract
 *)

#include "operator.ligo"

(* ------------------------------------------------------------- *)

(*
 * Helpers
 *)

(*
 * Increases the balance of an address associated with it
 * and present in ledger. If not, creates a new address with
 * that balance given.
 *)
function credit_to
  ( const parameter : mint_params
  ; const store     : storage
  ) : storage is block
{ var updated_balance : nat := 0n
; case store.ledger[parameter.to_] of
    Some (ledger_balance) -> updated_balance := ledger_balance
  | None -> skip
  end
; updated_balance := updated_balance + parameter.amount
; const updated_ledger : ledger =
    Big_map.update(parameter.to_, Some (updated_balance), store.ledger)
; const ups : storage = store with record [ ledger = updated_ledger ]
} with ups

(*
 * Decreases the balance of an address that is present in
 * ledger. Fails if it's not or given address has not enough
 * tokens to be burn from.
 *)
function debit_from
  ( const parameter : burn_params
  ; const store     : storage
  ) : storage is block
{ var updated_ledger : ledger := store.ledger
; case store.ledger[parameter.from_] of
    Some (ledger_balance) -> if parameter.amount > ledger_balance
      then failwith ("INSUFFICIENT_BALANCE")
      else
      { const updated_balance : nat = abs (ledger_balance - parameter.amount)
      ; updated_ledger := Big_map.update(parameter.from_, Some (updated_balance), updated_ledger)
      }
  | None -> failwith ("Given address is not present in ledger")
  end
} with store with record [ ledger = updated_ledger ]

(* ------------------------------------------------------------- *)

(*
 * FA2-specific entrypoints
 *)

(*
 * Initiates transfers from a given list of parameters. Fails
 * if one of the transfer does. Otherwise this function returns
 * updated storage and a list of transactions that call transfer
 * hooks if such provided for associated addresses.
 *)
function transfer
  ( const parameters : transfer_params
  ; const store      : storage
  ) : entrypoint is block
{ validate_operators (store.permissions, parameters, store.operators)

; function transfer_tokens
    ( const accumulator : storage
    ; const parameter   : transfer_param
    ) : storage is block
  { validate_token_type (parameter.1.1.0)
  ; const debit_params : burn_params = record
     [ from_  = parameter.0
     ; amount = parameter.1.1.1
     ]
  ; const credit_params : mint_params = record
     [ to_    = parameter.1.0
     ; amount = parameter.1.1.1
     ]
  ; const debited_storage  : storage =
      debit_from (debit_params, accumulator)
  ; const credited_storage : storage =
      credit_to (credit_params, debited_storage)
  } with credited_storage

; const upd : list (operation) =
    generic_transfer_hook (parameters, store.permissions)
; const ups : storage =
    List.fold (transfer_tokens, parameters, store)
} with (upd, ups)

(*
 * Retrieves a list of `balance_of` items that is specified for multiple
 * token types in FA2 spec and leaves the storage untouched. In reality
 * we restrict all of them to be of `default_token_id`. Fails if one of
 * the parameters address to non-default token id.
 *)
function balance_of
  ( const parameter : balance_of_params
  ; const store     : storage
  ) : entrypoint is block
{ function retreive_balance
    ( const request : balance_of_request
    ) : balance_of_response is block
  { validate_token_type (request.token_id)
  ; var retreived_balance : nat := 0n
  ; case Big_map.find_opt (request.owner, store.ledger) of
      Some (ledger_balance) -> retreived_balance := ledger_balance
    | None -> skip
    end
  } with (request, retreived_balance)
; const responses : list (balance_of_response) =
    List.map (retreive_balance, parameter.0)
; const transfer_operation : operation =
    Tezos.transaction (responses, 0mutez, parameter.1)
} with (list [transfer_operation], store)

(*
 * Retrieves a list of `total_supply` that is specified for multiple
 * token types in FA2 spec and leaves the storage untouched. In reality
 * we restrict all of them to be of `default_token_id`.
 *)
function total_supply
  ( const parameter : total_supply_params
  ; const store     : storage
  ) : entrypoint is block
{ // We suppose that the token is already validated in `get_total_supply`
  // and equal to `default_token_id`
  const total_supply_response_ : total_supply_response_ = record
    [ token_id     = default_token_id
    ; total_supply = store.total_supply
    ]
; const total_supply_response : total_supply_response =
    Layout.convert_to_right_comb ((total_supply_response_ : total_supply_response_))
; function get_total_supply
    ( const ops      : list (total_supply_response)
    ; const token_id : token_id
    ) : list (total_supply_response) is block
    { validate_token_type (token_id)
    } with total_supply_response # ops
} with
  ( list [Tezos.transaction
    ( List.fold
        ( get_total_supply
        , parameter.0
        , (nil : list (total_supply_response))
        )
    , 0mutez
    , parameter.1
    )]
  , store
  )

(*
 * Returns a list of `token_metadata` responses in one call.
 * This is supposed to be used for multiple token types but
 * in this smart-contract we restrict all of them to be of
 * `default_token_type`. Fails if one of the parameters has
 * different token id that is specified
 *)
function token_metadata
  ( const parameter : token_metadata_params
  ; const store     : storage
  ) : entrypoint is block
{ const token_metadata : token_metadata =
    Layout.convert_to_right_comb ((store.token_metadata : token_metadata_))
; function get_token_metadata
    ( const ops      : list (token_metadata_response)
    ; const token_id : token_id
    ) : list (token_metadata_response) is block
    { validate_token_type (token_id)
    } with token_metadata # ops
} with
  ( list [Tezos.transaction
    ( List.fold
        ( get_token_metadata
        , parameter.0
        , (nil : list (token_metadata_response))
        )
    , 0mutez
    , parameter.1
    )]
  , store
  )

(*
 * Retrieves current permissions for stablecoin smart contract.
 *)
function permission_descriptor
  ( const parameter : permissions_descriptor_params
  ; const store     : storage
  ) : entrypoint is block
{ const permissions : permissions_descriptor =
    Layout.convert_to_right_comb ((store.permissions : permissions_descriptor_))
} with
  ( list [Tezos.transaction
    ( permissions
    , 0mutez
    , parameter
    )]
  , store
  )

(*
 * Add or remove operators for provided owners.
 *)
function update_operators_action
  ( const parameter : update_operator_params
  ; const store     : storage
  ) : entrypoint is block
{ const updated_operators : operators =
    update_operators (parameter, store.operators)
} with
  ( (nil : list (operation))
  , store with record [ operators = updated_operators ]
  )

(*
 * Retrieves a boolean of whether the given address is an operator
 * for `owner` address and remains the store untouched
 *)
function is_operator_action
  ( const parameter : is_operator_params
  ; const store     : storage
  ) : entrypoint is block
  { const operator_transfer_policy : operator_transfer_policy_ =
      Layout.convert_from_right_comb ((store.permissions.operator : operator_transfer_policy))
  ; case operator_transfer_policy of
      Operator_transfer_permitted (u) -> skip
    | Operator_transfer_denied (u) -> failwith ("TX_DENIED")
    end
  } with
  ( list [is_operator (parameter, store.operators)]
  , store
  )