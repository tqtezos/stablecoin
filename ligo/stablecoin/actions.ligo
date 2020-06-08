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
 * Helper function used to reduce
 *  `if (condition) then failwith (message) else skip;`
 * appearances.
 *)
function fail_on
  ( const condition : bool
  ; const message   : string
  ) : unit is if condition then failwith (message) else unit

(*
 * Authorizes current contract owner and fails otherwise.
 *)
function authorize_owner
  ( const store : storage
  ) : unit is
  fail_on
    ( Tezos.sender =/= store.roles.owner
    , "NOT_OWNER"
    )

(*
 * Ensures that sender is current pending owner
 *)
function authorize_pending_owner
  ( const store : storage
  ) : address is case store.roles.pending_owner of
    Some (pending_owner) -> if Tezos.sender =/= pending_owner
      then (failwith ("NOT_PENDING_OWNER") : address)
      else pending_owner
    | None -> (failwith ("NO_PENDING_OWNER_SET") : address)
    end

(*
 * Authorizes contract pauser and fails otherwise.
 *)
function authorize_pauser
  ( const store : storage
  ) : unit is
  fail_on
    ( Tezos.sender =/= store.roles.pauser
    , "NOT_PAUSER"
    )

(*
 * Authorizes master_minter and fails otherwise.
 *)
function authorize_master_minter
  ( const store : storage
  ) : unit is
  fail_on
    ( Tezos.sender =/= store.roles.master_minter
    , "NOT_MASTER_MINTER"
    )

(*
 * Authorizes minter and returns it's current minting allowance.
 * Fails if sender is not minter.
 *)
function authorize_minter
  ( const store : storage
  ) : nat is case store.minting_allowances[Tezos.sender] of
    Some (currentAllowance) -> currentAllowance
  | None -> (failwith ("NOT_MINTER") : nat)
  end

(*
 * Helper that fails if contract is paused.
 *)
function ensure_not_paused
  ( const store : storage
  ) : unit is
  fail_on
    ( store.paused
    , "CONTRACT_PAUSED"
    )

(*
 * Helper that fails if contract is not paused.
 *)
function ensure_is_paused
  ( const store : storage
  ) : unit is
  fail_on
    ( not store.paused
    , "CONTRACT_NOT_PAUSED"
    )

(*
 * Increases the balance of an address associated with it
 * and present in ledger. If not, creates a new address with
 * that balance given.
 *)
function credit_to
  ( const parameter : mint_param_
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
} with store with record [ ledger = updated_ledger ]

type debit_param_ is record
  from_  : address
; amount : nat
end

(*
 * Decreases the balance of an address that is present in
 * ledger. Fails if it's not or given address has not enough
 * tokens to be burn from.
 *)
function debit_from
  ( const parameter : debit_param_
  ; const store     : storage
  ) : storage is block
{ var updated_ledger : ledger := store.ledger

; const curr_balance : nat = case store.ledger[parameter.from_] of
    Some (ledger_balance) -> ledger_balance
  | None -> 0n // Interpret non existant account as zero balance as per FA2
  end

; if parameter.amount > curr_balance
      then failwith ("INSUFFICIENT_BALANCE")
      else
      { const updated_balance : nat = abs (curr_balance - parameter.amount)
      ; updated_ledger := Big_map.update(parameter.from_, Some (updated_balance), updated_ledger)
      }
} with store with record [ ledger = updated_ledger ]

(*
 * Converts a transfer item into a safelist transfer item
 *)
function convert_to_safelist_transfer
  ( const tp : transfer_param
  ) : safelist_transfer_item is
    record
      [ from_ = tp.0
      ; to_ = List.map
          ( function
              ( const dst: transfer_destination
              ) : address is dst.0
          , tp.1
          )
      ]

(*
 * Calls `assert_transfer` of the provided safelist contract using
 * the provided transfer_descriptors.
 *)
function call_assert_transfers
  ( const ops_in : list(operation)
  ; const opt_sl_address : option(address)
  ; const transfer_params : transfer_params
  ) : list(operation) is block
  { const operations: list(operation) =
      case opt_sl_address of
        Some (sl_address) ->
          case (Tezos.get_entrypoint_opt ("%assertTransfers", sl_address) : option(contract(safelist_assert_transfers_param))) of
            Some (sl_caddress) ->
              Tezos.transaction
                ( List.map(convert_to_safelist_transfer, transfer_params)
                , 0mutez
                , sl_caddress) # ops_in
            | None -> (failwith ("BAD_SAFELIST_CONTRACT") : list(operation))
            end
        | None -> ops_in
        end
  } with operations

(*
 * Calls `Assert_receivers` of the provided safelist contract using
 * the provided transfer_descriptors.
 *)
function call_assert_receivers
  ( const opt_sl_address : option(address)
  ; const receivers : list(address)
  ) : list(operation) is block
  {
    const ops_in : list(operation) = nil
  ; const operations:list(operation) =
      case opt_sl_address of
        Some (sl_address) ->
          case (Tezos.get_entrypoint_opt ("%assertReceivers", sl_address) : option(contract(safelist_assert_receivers_param))) of
            Some (sl_caddress) ->
              Tezos.transaction
                ( receivers
                , 0mutez
                , sl_caddress) # ops_in
            | None -> (failwith ("BAD_SAFELIST_CONTRACT") : list(operation))
            end
      | None -> ops_in
      end
  } with operations

(*
 * Calls `Assert_receiver` of the provided safelist contract using
 * the provided transfer_descriptors.
 *)
function call_assert_receiver
  ( const opt_sl_address : option(address)
  ; const receiver : address
  ) : list(operation) is block
  { const ops_in : list(operation) = nil
  ; const operations:list(operation) =
      case opt_sl_address of
        Some (sl_address) ->
          case (Tezos.get_entrypoint_opt ("%assertReceiver", sl_address) : option(contract(safelist_assert_receiver_param))) of
            Some (sl_caddress) ->
              Tezos.transaction
                ( receiver
                , 0mutez
                , sl_caddress) # ops_in
            | None -> (failwith ("BAD_SAFELIST_CONTRACT") : list(operation))
            end
      | None -> ops_in
      end
  } with operations

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
  ( const params : transfer_params
  ; const store  : storage
  ) : entrypoint is block
{ ensure_not_paused (store)

; const sender_addr : address = Tezos.sender

; function make_transfer
    ( const acc       : entrypoint
    ; const parameter : transfer_param
    ) : entrypoint is block
  { validate_operators (parameter, acc.1.operators)
  ; function transfer_tokens
      ( const accumulator : storage
      ; const destination : transfer_destination
      ) : storage is block
    { validate_token_type (destination.1.0)
    ; const debit_param_ : debit_param_ = record
      [ from_  = parameter.0
      ; amount = destination.1.1
      ]
    ; const credit_param_ : mint_param_ = record
      [ to_    = destination.0
      ; amount = destination.1.1
      ]
    ; const debited_storage : storage =
        debit_from (debit_param_, accumulator)
    ; const credited_storage : storage =
        credit_to (credit_param_, debited_storage)
    } with credited_storage

  ; const operator : address = parameter.0
  ; const txs : list (transfer_destination) = parameter.1

  ; const upd : list (operation) =
      call_assert_transfers
        ( generic_transfer_hook (parameter)
        , store.safelist_contract
        , params
        )
  ; const ups : storage =
      List.fold (transfer_tokens, txs, acc.1)
  } with (merge_operations (upd, acc.0), ups)
} with List.fold (make_transfer, params, ((nil : list (operation)), store))

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
    (0n, ("USDC", ("tz", (0n, (Map.empty : map (string, string))))))
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
  { const operator_permissions : operator_transfer_policy = Layout.convert_to_right_comb((Owner_or_operator_transfer : operator_transfer_policy_))
  ; const owner_hook_policy : owner_hook_policy = Layout.convert_to_right_comb((Optional_owner_hook : owner_hook_policy_))
  ; const permissions : permissions_descriptor =
      (operator_permissions, (owner_hook_policy, (owner_hook_policy, (None : option (custom_permission_policy)))))

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
  ) : entrypoint is
  ( list [is_operator (parameter, store.operators)]
  , store
  )

(*
 * Pauses whole contract in order to prevent all transferring, burning,
 * minting and allowance operations. All other operations remain
 * unaffected. Fails if the contract is already paused.
 *)
function pause
  ( const parameter : pause_params
  ; const store     : storage
  ) : entrypoint is block
{ ensure_not_paused (store)
; authorize_pauser (store)
} with
  ( (nil : list (operation))
  , store with record [paused = True]
  )

(*
 * Unpauses whole contract so that all transferring, burning, minting and
 * allowance operations can be performed. Fails if the contract is already
 * unpaused.
 *)
function unpause
  ( const parameter : unpause_params
  ; const store     : storage
  ) : entrypoint is block
{ ensure_is_paused (store)
; authorize_pauser (store)
} with
  ( (nil : list (operation))
  , store with record [paused = False]
  )

(*
 * Adds minter to minting allowances map setting its allowance
 * to 0. Resets the allowance if associated address is already
 * present. Fails if sender is not master minter.
 *)
function configure_minter
  ( const parameter : configure_minter_params
  ; const store     : storage
  ) : entrypoint is block
{ ensure_not_paused (store)
; authorize_master_minter (store)
; const present_minting_allowance : option (nat) =
    store.minting_allowances[parameter.0]
; case parameter.1.0 of
    None -> case present_minting_allowance of
      Some (u) -> failwith ("NO_ALLOWANCE_EXPECTED")
    | None -> skip
    end
  | Some (current_minting_allowance) ->
      case present_minting_allowance of
        Some (allowance) -> if allowance =/= current_minting_allowance
          then failwith ("ALLOWANCE_MISMATCH")
          else skip
      | None -> failwith ("NOT_MINTER")
      end
  end
} with
  ( (nil : list (operation))
  , store with record
      [ minting_allowances = Big_map.update
          ( parameter.0
          , Some (parameter.1.1)
          , store.minting_allowances
          )
      ]
  )

(*
 * Removes minter from minting allowances map. Fails if minter
 * is not present. Fails if sender is not master minter.
 *)
function remove_minter
  ( const parameter : remove_minter_params
  ; const store     : storage
  ) : entrypoint is block
{ authorize_master_minter (store)
; case store.minting_allowances[parameter] of
    Some (u) -> skip
  | None -> failwith ("NOT_MINTER")
  end
} with
  ( (nil : list (operation))
  , store with record
      [ minting_allowances = Big_map.remove
          ( parameter
          , store.minting_allowances
          )
      ]
  )

(*
 * Produces tokens to a wallet associated with the given address
 *)
function mint
  ( const parameters : mint_params
  ; const store      : storage
  ) : entrypoint is block
{ ensure_not_paused (store)
; var current_allowance : nat := authorize_minter (store)
; var current_storage : storage := store

; const senderAddress : address = Tezos.sender

; for mint_param in list parameters block {
    const unwrapped_parameter : mint_param_ = record 
      [ to_ = mint_param.0
      ; amount = mint_param.1
      ]
  ; current_allowance :=
      case is_nat (current_allowance - unwrapped_parameter.amount) of
        Some (n) -> n
      | None -> (failwith ("ALLOWANCE_EXCEEDED") : nat)
      end
  ; const updated_allowances : minting_allowances =
      Big_map.update (Tezos.sender, Some (current_allowance), current_storage.minting_allowances)
  ; current_storage := current_storage with record
      [ minting_allowances = updated_allowances
      ; total_supply = current_storage.total_supply + unwrapped_parameter.amount
      ]
  ; current_storage := credit_to (unwrapped_parameter, current_storage)
  } 

; const receivers : list(address) =
    List.map
      ( function
          ( const mint_param: mint_param
          ) : address is mint_param.0
      , parameters
      )

; const upds : list(operation) = call_assert_receivers
    ( store.safelist_contract
    , Tezos.sender # receivers
    )

} with
  ( upds
  , current_storage
  )


(*
 * Decreases balance and total supply of tokens by the given amount
 *)
function burn
  ( const parameters : burn_params
  ; const store      : storage
  ) : entrypoint is block
{ ensure_not_paused (store)
; const unused : nat = authorize_minter (store)
; const sender_address : address = Tezos.sender
; function burn_tokens
    ( const accumulator : storage
    ; const burn_amount : nat
    ) : storage is block
  { const debited_storage : storage = debit_from
      ( record
          [ from_ = sender_address
          ; amount = burn_amount
          ]
      , accumulator
      )
  } with debited_storage with record
      [ total_supply = case is_nat (debited_storage.total_supply - burn_amount) of
          Some (nat) -> nat
        | None -> (failwith ("NEGATIVE_TOTAL_SUPPLY") : nat)
        end
      ]

; const upds : list(operation) = call_assert_receiver
    ( store.safelist_contract
    , Tezos.sender
    )

} with
  ( upds
  , List.fold (burn_tokens, parameters, store)
  )

(*
 * Sets contract owner to a new address
 *)
function transfer_ownership
  ( const parameter : transfer_ownership_param
  ; const store     : storage
  ) : entrypoint is block
{ authorize_owner (store)
} with
  ( (nil : list (operation))
  , store with record [roles.pending_owner = Some (parameter)]
  )

(*
 * When pending contract owner calls this entrypoint, the address
 * receives owner privileges for contract
 *)
function accept_ownership
  ( const parameter : accept_ownership_param
  ; const store     : storage
  ) : entrypoint is
  ( (nil : list (operation))
  , store with record
      [ roles.pending_owner = (None : option (address))
      ; roles.owner = authorize_pending_owner (store)
      ]
  )

(*
 * Moves master minter priveleges to a new address.
 * Fails if sender is not contract owner.
 *)
function change_master_minter
  ( const parameter : change_master_minter_param
  ; const store     : storage
  ) : entrypoint is block
{ authorize_owner (store)
} with
  ( (nil : list (operation))
  , store with record [ roles.master_minter = parameter ]
  )

(*
 * Moves pauser priveleges to a new address.
 * Fails if sender is not contract owner.
 *)
function change_pauser
  ( const parameter : change_pauser_param
  ; const store     : storage
  ) : entrypoint is block
{ authorize_owner (store)
} with
  ( (nil : list (operation))
  , store with record [ roles.pauser = parameter ]
  )

(*
 * Sets safelist contract address
 *)
function set_safelist
  ( const parameter : set_safelist_param
  ; const store     : storage
  ) : entrypoint is block
{ authorize_owner (store)

; case parameter of
    Some (sl_address) ->
      case (Tezos.get_entrypoint_opt ("%assertTransfers", sl_address) : option(contract(safelist_assert_transfers_param))) of
        Some (sl_caddress) -> case (Tezos.get_entrypoint_opt ("%assertReceivers", sl_address) : option(contract(safelist_assert_receivers_param))) of
          Some (sl_caddress) -> case (Tezos.get_entrypoint_opt ("%assertReceiver", sl_address) : option(contract(safelist_assert_receiver_param))) of
            Some (sl_caddress) -> skip
          | None -> failwith ("BAD_SAFELIST_CONTRACT")
          end
        | None -> failwith ("BAD_SAFELIST_CONTRACT")
        end
      | None -> failwith ("BAD_SAFELIST_CONTRACT")
      end
  | None -> skip
  end

} with
  ( (nil : list (operation))
  , store with record [ safelist_contract = parameter ]
  )
