// SPDX-FileCopyrightText: 2020 TQ Tezos
// SPDX-License-Identifier: MIT

(*
 * Root entrypoints for stablecoin smart-contract
 *)

#include "operator.ligo"
#include "permit.ligo"

(* ------------------------------------------------------------- *)

(*
 * Helpers
 *)

(*
 * Helper function used to reduce
 *  `if (condition) then failwith (message) else unit;`
 * appearances.
 *)
[@inline] function fail_on
  ( const condition : bool
  ; const message   : string
  ) : unit is if condition then failwith (message) else unit

(*
 * Authorizes current contract owner and fails otherwise.
 *)
[@inline] function authorize_contract_owner
  ( const store : storage
  ; const full_param : closed_parameter
  ) : storage is
  sender_check(store.roles.owner, store, full_param, "NOT_CONTRACT_OWNER")

(*
 * Ensures that sender is current pending contract owner.
 *)
function authorize_pending_owner
  ( const store : storage
  ; const full_param : closed_parameter
  ) : storage * address is case store.roles.pending_owner of
    Some (pending_owner) ->
      ( sender_check(pending_owner, store, full_param, "NOT_PENDING_OWNER")
      , pending_owner
      )
    | None -> (failwith ("NO_PENDING_OWNER_SET") : storage * address)
    end

(*
 * Authorizes contract pauser and fails otherwise.
 *)
[@inline] function authorize_pauser
  ( const store : storage
  ; const full_param : closed_parameter
  ) : storage is
  sender_check(store.roles.pauser, store, full_param, "NOT_PAUSER")

(*
 * Authorizes master_minter and fails otherwise.
 *)
[@inline] function authorize_master_minter
  ( const store : storage
  ; const full_param : closed_parameter
  ) : storage is
  sender_check(store.roles.master_minter, store, full_param, "NOT_MASTER_MINTER")

(*
 * Authorizes minter and fails otherwise.
 *)
[@inline] function authorize_minter
  ( const store : storage
  ) : unit is case store.minting_allowances[Tezos.sender] of
    Some (u) -> unit
  | None -> failwith ("NOT_MINTER")
  end

(*
 * Helper that fails if the contract is paused.
 *)
[@inline] function ensure_not_paused
  ( const store : storage
  ) : unit is
  fail_on
    ( store.paused
    , "CONTRACT_PAUSED"
    )

(*
 * Helper that fails if the contract is not paused.
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
 * if present in the ledger. If not, creates a new address entry with
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
 * ledger. Fails if it's not or the given address has not enough
 * tokens to burn from.
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

 ; const updated_balance: option(nat) = case is_nat(curr_balance - parameter.amount) of
       Some (ub) -> if ub = 0n then (None : option(nat)) else Some(ub) // If balance is 0 set None to remove it from ledger
     | None -> (failwith ("FA2_INSUFFICIENT_BALANCE") : option(nat))
     end
 ; updated_ledger := Big_map.update(parameter.from_, updated_balance, updated_ledger)

} with store with record [ ledger = updated_ledger ]

(*
 * Converts a transfer item into a transferlist transfer item
 *)
function convert_to_transferlist_transfer
  ( const tp : transfer_param
  ) : transferlist_transfer_item is
    ( tp.0
    , List.map
          ( function
              ( const dst: transfer_destination
              ) : address is dst.0
          , tp.1
          )
    )

(*
 * Calls `assert_transfer` of the provided transferlist contract using
 * the provided transfer_descriptors.
 *)
function call_assert_transfers
  ( const opt_sl_address : option(address)
  ; const transfer_params : transfer_params
  ) : list(operation) is block
  { const operations: list(operation) =
      case opt_sl_address of
        Some (sl_address) ->
          case (Tezos.get_entrypoint_opt ("%assertTransfers", sl_address) : option(contract(transferlist_assert_transfers_param))) of
            Some (sl_caddress) ->
              Tezos.transaction
                ( List.map(convert_to_transferlist_transfer, transfer_params)
                , 0mutez
                , sl_caddress) # (nil : list(operation))
            | None -> (failwith ("BAD_TRANSFERLIST_CONTRACT") : list(operation))
            end
        | None -> (nil : list(operation))
        end
  } with operations

(*
 * Calls `Assert_receivers` of the provided transferlist contract using
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
          case (Tezos.get_entrypoint_opt ("%assertReceivers", sl_address) : option(contract(transferlist_assert_receivers_param))) of
            Some (sl_caddress) ->
              Tezos.transaction
                ( receivers
                , 0mutez
                , sl_caddress) # ops_in
            | None -> (failwith ("BAD_TRANSFERLIST_CONTRACT") : list(operation))
            end
      | None -> ops_in
      end
  } with operations

(*
 * Assert that all addresses in a list are equal,
 * fails with `err_msg` otherwise.
 *
 * If the list is empty, returns `None`,
 * otherwise returns `Some` with the unique address.
 *)
function all_equal
  ( const addrs: list(address)
  ; const err_msg: string
  ) : option(address) is
  case addrs of
  | nil -> (None : option(address))
  | first # rest ->
      block {
        List.iter
          ( function (const addr : address): unit is
              fail_on (addr =/= first, err_msg)
          , rest
          )
      } with Some(first)
  end

(* ------------------------------------------------------------- *)

(*
 * FA2-specific entrypoints
 *)

(*
 * Verify the sender of a `transfer` action.
 *
 * The check is successful if either of these is true:
 * 1) The sender is either the owner or an approved operator for each and every
 *    account from which funds will be withdrawn.
 * 2) All transfers withdraw funds from a single account, and the account owner
 *    has issued a permit allowing this call to go through.
 *)
function transfer_sender_check
  ( const params : transfer_params
  ; const store : storage
  ; const full_param : closed_parameter
  ) : storage is block
{ // check if the sender is either the owner or an approved operator for all transfers
  const is_approved_operator_for_all : bool =
    List.fold
      ( function(const acc: bool; const p : transfer_param) is
          acc and is_approved_operator(p, store.operators)
      , params
      , True
      )
} with
    if is_approved_operator_for_all then
      store
    else
      case params of
        nil -> store
      | first_param # rest -> block {
          // check whether `from_` has issued a permit
          const from_: address = first_param.0
        ; const updated_store : storage = sender_check(from_, store, full_param, "FA2_NOT_OPERATOR")
          // check that all operations relate to the same owner.
        ; List.iter
            ( function (const param : transfer_param): unit is
                if param.0 =/= from_
                  then failwith ("FA2_NOT_OPERATOR")
                  else Unit
            , rest
            )
        } with updated_store
      end

(*
 * Initiates transfers from a given list of parameters. Fails
 * if one of the transfer does. Otherwise this function returns the
 * updated storage and a list of transactions that call transfer
 * hooks if such are provided for associated addresses.
 *)
function transfer
  ( const params : transfer_params
  ; const store  : storage
  ; const full_param : closed_parameter
  ) : entrypoint is block
{ ensure_not_paused (store)
; const store : storage = transfer_sender_check(params, store, full_param)

; const sender_addr : address = Tezos.sender

; function make_transfer
    ( const acc       : entrypoint
    ; const parameter : transfer_param
    ) : entrypoint is block
  { function transfer_tokens
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
        ( store.transferlist_contract
        , params
        )
  ; const ups : storage =
      List.fold (transfer_tokens, txs, acc.1)
  } with (merge_operations (upd, acc.0), ups)
} with List.fold (make_transfer, params, ((nil : list (operation)), store))

(*
 * Retrieves a list of `balance_of` items that is specified for multiple
 * token types in the FA2 spec and leaves the storage untouched. In reality
 * we restrict all of them to be of `default_token_id`. Fails if one of
 * the parameters address to a non-default token id.
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
 * Add or remove operators for the provided owners.
 *)
function update_operators_action
  ( const params : update_operator_params
  ; const store : storage
  ; const full_param : closed_parameter
  ) : entrypoint is block
{ ensure_not_paused (store)
; const owners : list(address) = List.map(get_owner, params)

// A user can only modify their own operators.
// So we check that all operations affect the same `owner`,
// and that the sender *is* the owner or the owner has issued a permit.
; const store : storage =
    case all_equal(owners, "NOT_TOKEN_OWNER") of
    | None -> store
    | Some(owner) -> sender_check(owner, store, full_param, "NOT_TOKEN_OWNER")
    end

; const updated_operators : operators =
    update_operators (params, store.operators)
} with
  ( (nil : list (operation))
  , store with record [ operators = updated_operators ]
  )

(*
 * Pauses the whole contract in order to prevent all transferring, burning,
 * minting and allowance operations. All other operations remain
 * unaffected. Fails if the contract is already paused.
 *)
function pause
  ( const parameter : pause_params
  ; const store     : storage
  ; const full_param : closed_parameter
  ) : entrypoint is block
{ ensure_not_paused (store)
; const store: storage = authorize_pauser(store, full_param)
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
  ; const full_param : closed_parameter
  ) : entrypoint is block
{ ensure_is_paused (store)
; const store: storage = authorize_pauser(store, full_param)
} with
  ( (nil : list (operation))
  , store with record [paused = False]
  )

(*
 * Adds the minter to the minting allowances map, setting its allowance
 * to 0. Resets the allowance if the associated address is already
 * present. Fails if the sender is not master minter.
 *)
function configure_minter
  ( const parameter : configure_minter_params
  ; const store     : storage
  ; const full_param : closed_parameter
  ) : entrypoint is block
{ ensure_not_paused (store)
; const store : storage = authorize_master_minter (store, full_param)
; const present_minting_allowance : option (nat) =
    store.minting_allowances[parameter.0]
; const minter_limit : nat = 12n
; case parameter.1.0 of
    None -> case present_minting_allowance of
      Some (u) -> failwith ("CURRENT_ALLOWANCE_REQUIRED")
    | None ->
        // We are adding a new minter. Check the minter limit is not exceeded.
        if Map.size(store.minting_allowances) >= minter_limit then failwith ("MINTER_LIMIT_REACHED") else skip
    end
  | Some (current_minting_allowance) ->
      case present_minting_allowance of
        Some (allowance) -> if allowance =/= current_minting_allowance
          then failwith ("ALLOWANCE_MISMATCH")
          else skip
      | None -> failwith ("ADDR_NOT_MINTER")
      end
  end
} with
  ( (nil : list (operation))
  , store with record
      [ minting_allowances = Map.update
          ( parameter.0
          , Some (parameter.1.1)
          , store.minting_allowances
          )
      ]
  )

(*
 * Removes the minter from the minting allowances map. Fails if the minter
 * is not present. Fails if the sender is not the master minter.
 *)
function remove_minter
  ( const parameter : remove_minter_params
  ; const store     : storage
  ; const full_param : closed_parameter
  ) : entrypoint is block
{ const store : storage = authorize_master_minter (store, full_param)
; case store.minting_allowances[parameter] of
    Some (u) -> skip
  | None -> failwith ("ADDR_NOT_MINTER")
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
 * Adds the provided value to the total_supply field and returns the storage.
 * The input is an 'int' so that this can be used to increment and decrement
 * the `total_supply` field.
 *)
function inc_total_supply
  ( const count : int
  ; const store  : storage
  ) : storage is block
  { const new_total_supply : int = store.total_supply + count
  ; const new_total_supply_nat : nat = case is_nat(new_total_supply) of
      Some (n) -> n
    | None -> (failwith ("NEGATIVE_TOTAL_SUPPLY") : nat)
    end
  } with store with record [ total_supply = new_total_supply_nat ]

(*
 * Produces tokens to the wallet associated with the given address.
 *)
function mint
  ( const parameters : mint_params
  ; const store      : storage
  ) : entrypoint is block
{ ensure_not_paused (store)
; authorize_minter (store)

; const senderAddress : address = Tezos.sender

; function mint_tokens
    ( const accumulator       : storage
    ; const mint_param        : mint_param
    ; const current_allowance : nat
    ) : storage is block
  { const unwrapped_parameter : mint_param_ =
      Layout.convert_from_right_comb ((mint_param : mint_param))
  ; const updated_allowance : nat =
      case is_nat (current_allowance - unwrapped_parameter.amount) of
        Some (n) -> n
      | None -> (failwith ("ALLOWANCE_EXCEEDED") : nat)
      end
  ; const updated_allowances : minting_allowances =
      Big_map.update (Tezos.sender, Some (updated_allowance), accumulator.minting_allowances)
  ; const updated_store : storage = accumulator with record
      [ minting_allowances = updated_allowances
      ]
  } with
      inc_total_supply(int(unwrapped_parameter.amount),
        credit_to (unwrapped_parameter, updated_store))

; const receivers : list(address) =
    List.map
      ( function
          ( const mint_param: mint_param
          ) : address is mint_param.0
      , parameters
      )

; const upds : list(operation) = call_assert_receivers
    ( store.transferlist_contract
    , Tezos.sender # receivers
    )

} with
  ( upds
  , List.fold
      ( function
          ( const accumulator : storage
          ; const mint_param  : mint_param
          ) : storage is case accumulator.minting_allowances[senderAddress] of
            None -> (failwith ("NOT_MINTER") : storage)
          | Some (current_allowance) ->
              mint_tokens (accumulator, mint_param, current_allowance)
          end
      , parameters
      , store
      )
  )

(*
 * Decreases the balance of tokens by the given amount.
 *)
function burn
  ( const parameters : burn_params
  ; const store      : storage
  ) : entrypoint is block
{ ensure_not_paused (store)
; authorize_minter (store)
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
  } with inc_total_supply(-burn_amount, debited_storage)

; const upds : list(operation) = call_assert_receivers
    ( store.transferlist_contract
    , Tezos.sender # nil
    )

} with
  ( upds
  , List.fold (burn_tokens, parameters, store)
  )

(*
 * Sets the contract owner to a new address.
 *)
function transfer_ownership
  ( const parameter : transfer_ownership_param
  ; const store     : storage
  ; const full_param : closed_parameter
  ) : entrypoint is block
{ const store : storage = authorize_contract_owner (store, full_param)
} with
  ( (nil : list (operation))
  , store with record [roles.pending_owner = Some (parameter)]
  )

(*
 * When the pending contract owner calls this entrypoint, the address
 * receives owner privileges for the contract.
 *)
function accept_ownership
  ( const parameter : accept_ownership_param
  ; const store     : storage
  ; const full_param : closed_parameter
  ) : entrypoint is block
{ const auth_result : (storage * address) = authorize_pending_owner (store, full_param)
} with
  ( (nil : list (operation))
  , auth_result.0 with record
      [ roles.pending_owner = (None : option (address))
      ; roles.owner = auth_result.1
      ]
  )

(*
 * Moves master minter priveleges to a new address.
 * Fails if the sender is not the contract owner.
 *)
function change_master_minter
  ( const parameter : change_master_minter_param
  ; const store     : storage
  ; const full_param : closed_parameter
  ) : entrypoint is block
{ const store : storage = authorize_contract_owner (store, full_param)
} with
  ( (nil : list (operation))
  , store with record [ roles.master_minter = parameter ]
  )

(*
 * Moves pauser priveleges to a new address.
 * Fails if the sender is not the contract owner.
 *)
function change_pauser
  ( const parameter : change_pauser_param
  ; const store     : storage
  ; const full_param : closed_parameter
  ) : entrypoint is block
{ const store : storage = authorize_contract_owner (store, full_param)
} with
  ( (nil : list (operation))
  , store with record [ roles.pauser = parameter ]
  )

(*
 * Sets transferlist contract address.
 *)
function set_transferlist
  ( const parameter : set_transferlist_param
  ; const store     : storage
  ; const full_param : closed_parameter
  ) : entrypoint is block
{ const store : storage = authorize_contract_owner (store, full_param)

; case parameter of
    Some (sl_address) ->
      case (Tezos.get_entrypoint_opt ("%assertTransfers", sl_address) : option(contract(transferlist_assert_transfers_param))) of
        Some (sl_caddress) -> case (Tezos.get_entrypoint_opt ("%assertReceivers", sl_address) : option(contract(transferlist_assert_receivers_param))) of
          Some (sl_caddress) -> skip
        | None -> failwith ("BAD_TRANSFERLIST")
        end
      | None -> failwith ("BAD_TRANSFERLIST")
      end
  | None -> skip
  end

} with
  ( (nil : list (operation))
  , store with record [ transferlist_contract = parameter ]
  )

(*
 * Creates a new permit, allowing any user to act on behalf of the user
 * who signed the permit.
 *
 * Fails with `"DUP_PERMIT"` if the same permit hash is re-uploaded before it expires.
 *)
function add_permit
  ( const parameter: permit_param
  ; const store    : storage
  ) : entrypoint is block
{ const key : key = parameter.0
; const signature : signature = parameter.1.0
; const permit : blake2b_hash = parameter.1.1
; const issuer: address = Tezos.address(Tezos.implicit_account(Crypto.hash_key(key)))

// form the structure that is to be signed by pairing self contract address, chain id, counter
// and permit hash and pack it to get bytes.
; const to_sign : bytes = Bytes.pack (((Tezos.self_address, Tezos.chain_id), (store.permit_counter, permit)))

// check the included signature against public_key from parameter and bytes derived from the
// operation in parameter.
; const store : storage =
    if (Crypto.check (key, signature, to_sign)) then
      store with record
        [ permit_counter = store.permit_counter + 1n
        ; permits =
            delete_expired_permits(store.default_expiry, issuer,
              insert_permit(store.default_expiry, issuer, permit, store.permits)
            )
        ]
    else block {
      // We're using Embedded Michelson here to get around the limitation that,
      // currently, Ligo's `failwith` only supports strings and numeric types.
      // https://ligolang.org/docs/advanced/embedded-michelson/
      //
      // Once this MR is merged, we should be able to use Ligo's `failwith`:
      // https://gitlab.com/ligolang/ligo/-/issues/193
      const failwith_ : (string * bytes -> storage) =
        [%Michelson ({| { FAILWITH } |} : string * bytes -> storage)];
    } with failwith_(("MISSIGNED", to_sign))
} with
    ( (nil : list(operation))
    , store
    )

(*
 * Sets the default expiry for the sender (if the param contains a `None`)
 * or for a specific permit (if the param contains a `Some`).
 *
 * When the permit whose expiry should be set does not exist,
 * nothing happens.
 *)
function set_expiry
  ( const param: set_expiry_param
  ; const store : storage
  ; const full_param: closed_parameter
  ) : entrypoint is block
{ const owner : address = param.0
; const new_expiry : seconds = param.1.0
; const specific_permit_or_default : option(blake2b_hash) = param.1.1
; const updated_store : storage = sender_check(owner, store, full_param, "NOT_PERMIT_ISSUER")

; const updated_permits : permits =
    case specific_permit_or_default of
    | None ->
        set_user_default_expiry(owner, new_expiry, updated_store.permits)
    | Some(permit_hash) ->
        set_permit_expiry(owner, permit_hash, new_expiry, updated_store.permits, store.default_expiry)
    end
} with
    ( (nil : list(operation))
    , updated_store with record [ permits = updated_permits ]
    )
