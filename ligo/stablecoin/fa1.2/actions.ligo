// SPDX-FileCopyrightText: 2020 TQ Tezos
// SPDX-License-Identifier: MIT

(*
 * Root entrypoints for stablecoin smart-contract
 *)

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
 * Ensures that the sender is the current pending contract owner.
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
 * Authorizes the contract pauser and fails otherwise.
 *)
[@inline] function authorize_pauser
  ( const store : storage
  ; const full_param : closed_parameter
  ) : storage is
  sender_check(store.roles.pauser, store, full_param, "NOT_PAUSER")

(*
 * Authorizes the master_minter and fails otherwise.
 *)
[@inline] function authorize_master_minter
  ( const store : storage
  ; const full_param : closed_parameter
  ) : storage is
  sender_check(store.roles.master_minter, store, full_param, "NOT_MASTER_MINTER")

(*
 * Authorizes the minter and fails otherwise.
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
  | None -> 0n // Interpret non existant account as zero balance as per FA1.2
  end

 // We're using Embedded Michelson here to get around the limitation that,
 // currently, Ligo's `failwith` only supports strings and numeric types,
 // but not, for example, pairs.
 // https://ligolang.org/docs/advanced/embedded-michelson/
 //
 // Once this MR is merged, we should be able to use Ligo's `failwith`:
 // https://gitlab.com/ligolang/ligo/-/issues/193
 ; const failwith_ : (string * (nat * nat) -> option(nat)) =
    [%Michelson ({| { FAILWITH } |} : string * (nat * nat) -> option(nat))]

 ; const updated_balance: option(nat) = case is_nat(curr_balance - parameter.amount) of
       Some (ub) -> if ub = 0n then (None : option(nat)) else Some(ub) // If balance is 0 set None to remove it from ledger
     | None -> (failwith_ ("NotEnoughBalance", (parameter.amount, curr_balance)) : option(nat))
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
    , list [ tp.1.0 ]
    )

(*
 * Calls `assert_transfer` of the provided transferlist contract using
 * the provided transfer_param.
 *)
function call_assert_transfer
  ( const opt_sl_address : option(address)
  ; const transfer_param : transfer_param
  ) : list(operation) is block
  { const operations: list(operation) =
      case opt_sl_address of
        Some (sl_address) ->
          case (Tezos.get_entrypoint_opt ("%assertTransfers", sl_address) : option(contract(transferlist_assert_transfers_param))) of
            Some (sl_caddress) ->
              Tezos.transaction
                ( list [ convert_to_transferlist_transfer(transfer_param) ]
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

(* ------------------------------------------------------------- *)

(*
 * FA1.2-specific entrypoints
 *)

(*
 * Initiates a transfer. This function returns
 * updated storage and a list of transactions that call transfer
 * hooks if such provided for associated addresses.
 *
 * Fails with "NotEnoughAllowance" if none of these 3 conditions hold:
 *   - The sender is the owner of the account being debited.
     - The owner has granted the sender enough allowance for this
       transaction to go through by using the `approve` entrypoint.
     - The owner has issued a permit allowing other users to make
       transfers from the owner's account.
 *
 * Fails with "NotEnoughBalance" if the account being debited does
 * not have enough tokens.
 *
 * Fails with "CONTRACT_PAUSED" if the contract is paused.
 *)
function transfer
  ( const param: transfer_param
  ; const store: storage
  ; const full_param: closed_parameter
  ) : entrypoint is block
{ ensure_not_paused(store)

; const store: storage =
    case deduct_from_allowance(param.0, Tezos.sender, param.1.1, store.spender_allowances) of
    | Deducted(updated_spender_allowances) ->
        store with record [spender_allowances = updated_spender_allowances]
    | NotEnoughAllowance(err) -> block {
          // We're using Embedded Michelson here to get around the limitation that,
          // currently, Ligo's `failwith` only supports strings and numeric types,
          // but not, for example, pairs.
          // https://ligolang.org/docs/advanced/embedded-michelson/
          //
          // Once this MR is merged, we should be able to use Ligo's `failwith`:
          // https://gitlab.com/ligolang/ligo/-/issues/193
          const failwith_ : (string * (nat * nat) -> storage) =
            [%Michelson ({| { FAILWITH } |} : string * (nat * nat) -> storage)]

        ; function on_err(const u: unit): storage is
            failwith_(("NotEnoughAllowance", (err.required, err.present)))

      } with
        sender_check_(param.0, store, full_param, on_err)
    end
; const debit_param_ : debit_param_ = record
  [ from_  = param.0
  ; amount = param.1.1
  ]
; const credit_param_ : mint_param_ = record
  [ to_    = param.1.0
  ; amount = param.1.1
  ]
} with
    ( call_assert_transfer(store.transferlist_contract, param)
    , credit_to(credit_param_, debit_from(debit_param_, store))
    )

(*
 * Authorizes the given `spender` to transfer the given amount of tokens
 * on the sender's behalf.
 *
 * Fails with "UnsafeAllowanceChange" if the spender's old allowance and new allowance
 * are both non-zero.
 *)
function approve
  ( const param: approve_param
  ; const store: storage
  ; const full_param: closed_parameter
  ) : entrypoint is
  ( (nil : list (operation))
  , store with record
      [ spender_allowances =
          approve_allowance(Tezos.sender, param.0, param.1, store.spender_allowances)
      ]
  )

(*
 * Retrieves an account's balance and leaves the storage untouched.
 *)
function get_balance
  ( const param: get_balance_param
  ; const store: storage
  ): entrypoint is block
{ const account_balance: nat =
    case Big_map.find_opt(param.0, store.ledger) of
    | Some(account_balance) -> account_balance
    | None -> 0n
    end
} with
  ( list [ Tezos.transaction (account_balance, 0mutez, param.1) ]
  , store
  )

(*
 * Retrieves an account's spending allowance and leaves the storage untouched.
 *)
function get_allowance
  ( const param: get_allowance_param
  ; const store: storage
  ): entrypoint is
  ( list
      [ Tezos.transaction(
          get_allowance(param.0.0, param.0.1, store.spender_allowances),
          0mutez,
          param.1
        )
      ]
  , store
  )

(*
 * Retrieves the total number of tokens in circulation.
 *)
function get_total_supply
  ( const param: get_total_supply_param
  ; const store: storage
  ): entrypoint is
  ( list
      [ Tezos.transaction(
          store.total_supply,
          0mutez,
          param.1
        )
      ]
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
 * Adds minter to minting allowances map setting its allowance
 * to 0. Resets the allowance if associated address is already
 * present. Fails if sender is not master minter.
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
 * Removes minter from minting allowances map. Fails if minter
 * is not present. Fails if sender is not master minter.
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
 * Produces tokens to a wallet associated with the given address
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
      ; total_supply = accumulator.total_supply + unwrapped_parameter.amount
      ]
  } with credit_to (unwrapped_parameter, updated_store)

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
 * Decreases balance and total supply of tokens by the given amount
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
  } with debited_storage with record
      [ total_supply = case is_nat (debited_storage.total_supply - burn_amount) of
          Some (nat) -> nat
        | None -> (failwith ("NEGATIVE_TOTAL_SUPPLY") : nat)
        end
      ]

; const upds : list(operation) = call_assert_receivers
    ( store.transferlist_contract
    , Tezos.sender # nil
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
  ; const full_param : closed_parameter
  ) : entrypoint is block
{ const store : storage = authorize_contract_owner (store, full_param)
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
 * Fails if sender is not contract owner.
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
 * Fails if sender is not contract owner.
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
 * Sets transferlist contract address
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
  ) : entrypoint is block
{ const new_expiry : seconds = param.0
; const updated_permits : permits =
    case param.1 of
    | None ->
        set_user_default_expiry(Tezos.sender, new_expiry, store.permits)
    | Some(hash_and_address) ->
        set_permit_expiry(hash_and_address.1, hash_and_address.0, new_expiry, store.permits, store.default_expiry)
    end
} with
    ( (nil : list(operation))
    , store with record [ permits = updated_permits ]
    )
