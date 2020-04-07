// SPDX-FileCopyrightText: 2020 tqtezos
// SPDX-License-Identifier: MIT

#include "spec.ligo"

/* ------------------------------------------------------------- */

/*
 * Helpers
 */

/*
 * Authorizes current owner.
 * Fails when the given address is not owner.
 *
 * parameter is address
 */
function authorizeOwner
  ( const givenAddress : address
  ; const store        : storage
  ) : unit is block
{
  if givenAddress =/= store.fields.roles.owner
  then failwith ("Given address is not owner")
  else skip
} with unit

/*
 * Authorizes current pauser.
 * Fails when the given address is not pauser.
 *
 * parameter is address
 */
function authorizePauser
  ( const givenAddress : address
  ; const store        : storage
  ) : unit is block
{
  if givenAddress =/= store.fields.roles.pauser
  then failwith ("Given address is not pauser")
  else skip
} with unit

/*
 * Authorizes current master minter.
 * Fails when the given address is not master minter.
 *
 * parameter is address
 */
function authorizeMasterMinter
  ( const store : storage
  ) : unit is block
{
  if Tezos.sender =/= store.fields.roles.masterMinter
  then failwith ("Given address is not master minter")
  else skip
} with unit

/* ------------------------------------------------------------- */

/*
 * Helper function used to reduce
 *  `if (condition) then failwith (message) else skip;`
 * appearances.
 */
function failOn
  ( const condition : bool
  ; const message   : string
  ) : unit is if condition then failwith (message) else unit

/*
 * Helper that fails if the given contract is paused.
 */
// TODO
function ensureNotPaused
  ( const store : storage
  ) : unit is unit

/*
 * Returns the allowance of the given address that is present
 * in ledger or return 0 otherwise.
 */
function allowance
  ( const parameter : allowanceParams
  ; const store     : storage
  ) : nat is block
{
  var return : nat := abs (0);

  case store.ledger[parameter.owner] of
    Some (ledgerValue) ->
      case ledgerValue.approvals[parameter.spender] of
        Some (approval) ->
          return := approval
      | None -> skip
      end
  | None -> skip
  end

} with return

/*
 * Helper that decreases balance for the given wallet.
 *
 * parameter is record
 * [ fromAddress : address
 * ; amount      : nat
 * ]
 */
function debitFrom
  ( const parameter : burnParams
  ; const store     : storage
  ) : entrypoint is block
{
  const upd : list (operation) = nil;
  var ups : storage := store;

  case ups.ledger[parameter.fromAddress] of
    Some (ledgerValue) -> block
    {
      if ledgerValue.balance < parameter.amount
      then failwith ("Not enough balance to debit from a given wallet")
      else {
        ledgerValue.balance := abs (ledgerValue.balance - parameter.amount);
        ups.fields.totalSupply := abs (ups.fields.totalSupply - parameter.amount);
      }
    }
  | None -> failwith ("Given address is not present in ledger")
  end
} with (upd, ups)


/*
 * Helper that increases the balance for the given wallet.
 *
 * parameter is record
 * [ toAddress : address
 * ; amount    : nat
 * ]
 */
function creditTo
  ( const parameter : mintParams
  ; const store     : storage
  ) : entrypoint is block
{
  const upd : list (operation) = nil;
  var ups : storage := store;

  case ups.ledger[parameter.toAddress] of
    Some (ledgerValue) -> block
    {
      ledgerValue.balance := ledgerValue.balance + parameter.amount;
      ups.fields.totalSupply := ups.fields.totalSupply + parameter.amount;
    }
  | None -> failwith ("Given address is not present in ledger")
  end
} with (upd, ups)

/*
 * Helper that sets the given allowance for spender.
 *
 * parameter is record
 * [ owner   : address
 * ; spender : address
 * ; value   : nat
 * ]
 */
function setAllowance
  ( const parameter : allowanceParams
  ; const store     : storage
  ) : entrypoint is block
{
  const upd : list (operation) = nil;
  var ups : storage := store;

  case ups.ledger[parameter.owner] of
    Some (ledgerValue) ->
      case ledgerValue.approvals[parameter.spender] of
        Some (approval) ->
          ledgerValue.approvals[parameter.spender] := parameter.value
      | None ->
          failwith ("Spender address is not present in owner's minter list")
      end
  | None -> failwith ("Owner address is not present in ledger")
  end
} with (upd, ups)

/* ------------------------------------------------------------- */

/*
 * Entrypoints
 */

/*
 * Transfers given amount of tokens between addresses
 *
 * parameter is list of record
 * [ batch    : list (transferDescriptor)
 * ; operator : address
 * ]
 */
function transfer
  ( const parameter : transferParams
  ; const store     : storage
  ) : entrypoint is block
{
  // Remnants of fa1.2 interface
  //const debitParams : burnParams = record
  //  [ fromAddress = parameter.fromAddress
  //  ; amount      = parameter.amount
  //  ];
  //const creditParams : mintParams = record
  //  [ toAddress = parameter.toAddress
  //  ; amount    = parameter.amount
  //  ];

  // const ep1 : entrypoint = debitFrom (debitParams  , store);
  // const ep2 : entrypoint = creditTo  (creditParams , store);

  const upd : list (operation) = nil;
  const ups : storage = store;
} with (upd, ups)

/*
 * TODO
 */
function balanceOf
  ( const parameter : balanceOfParams
  ; const store     : storage
  ) : entrypoint is block
{
  const upd : list (operation) = nil;
  const ups : storage = store;
} with (upd, ups)

/*
 * TODO
 */
function getTotalSupply
  ( const parameter : getTotalSupplyParams
  ; const store     : storage
  ) : entrypoint is block
{
  const upd : list (operation) = nil;
  const ups : storage = store;
} with (upd, ups)

/*
 * TODO
 */
function getTokenMetadata
  ( const parameter : getTokenMetadataParams
  ; const store     : storage
  ) : entrypoint is block
{
  const upd : list (operation) = nil;
  const ups : storage = store;
} with (upd, ups)

/*
 * TODO
 */
function getPermissionsDescriptor
  ( const parameter : getPermissionsDescriptorParams
  ; const store     : storage
  ) : entrypoint is block
{
  const upd : list (operation) = nil;
  const ups : storage = store;
} with (upd, ups)

/*
 * TODO
 */
function setTransferHook
  ( const parameter : setTransferHookParams
  ; const store     : storage
  ) : entrypoint is block
{
  const upd : list (operation) = nil;
  const ups : storage = store;
} with (upd, ups)

/*
 * TODO
 */
function updateOperators
  ( const parameter : updateOperatorParams
  ; const store     : storage
  ) : entrypoint is block
{
  const upd : list (operation) = nil;
  const ups : storage = store;
} with (upd, ups)

/*
 * TODO
 */
function isOperator
  ( const parameter : isOperatorParams
  ; const store     : storage
  ) : entrypoint is block
{
  const upd : list (operation) = nil;
  const ups : storage = store;
} with (upd, ups)

/*
 * Pauses whole contract in order to prevent all transferring, burning,
 * minting and allowance operations. All other operations remain
 * unaffected. Fails if the contract is already paused.
 *
 * parameter is unit
 */
function pause
  ( const parameter : pauseParams
  ; const store     : storage
  ) : entrypoint is block
{
  if (store.fields.paused)
  then failwith ("Contract is already paused")
  else skip;

  const upd : list (operation) = nil;
  const ups : storage = store with record [fields.paused = True];
} with (upd, ups)

/*
 * Unpauses whole contract so that all transferring, burning, minting and
 * allowance operations can be performed. Fails if the contract is already
 * unpaused.
 *
 * parameter is unit
 */
function unpause
  ( const parameter : unpauseParams
  ; const store     : storage
  ) : entrypoint is block
{
  if (store.fields.paused)
  then skip
  else failwith ("Contract is already unpaused");

  const upd : list (operation) = nil;
  const ups : storage = store with record [fields.paused = False];
} with (upd, ups)

/*
 * Adds minter to sender's minter list.
 *
 * parameter is address
 */
function configureMinter
  ( const parameter : configureMinterParams
  ; const store     : storage
  ) : entrypoint is block
{
  authorizeMasterMinter (store);

  const upd : list (operation) = nil;
  var ups : storage := store;

  case ups.ledger[Tezos.sender] of
    Some (ledgerValue) ->
      // TODO: currently it resets given approval if it's present
      ledgerValue.approvals[parameter] := abs (0)
  | None -> failwith ("Sender address is not present in ledger")
  end
} with (upd, ups)

/*
 * Sets the amount of allowance for specific address.
 *
 * parameter is record
 * [ minter : address
 * ; amount : nat
 * ]
 */
function setMintingAllowance
  ( const parameter : setMintingAllowanceParams
  ; const store     : storage
  ) : entrypoint is block
{
  authorizeMasterMinter (store);

  const upd : list (operation) = nil;
  var ups : storage := store;

  case ups.ledger[Tezos.sender] of
    Some (ledgerValue) ->
      // TODO: currently it adds an address if it's non-present
      // TODO: there's an `Tezos.amount`
      ledgerValue.approvals[parameter.minter] := parameter.amount
  | None -> failwith ("Sender address is not present in ledger")
  end
} with (upd, ups)

/*
 * Removes minter from the sender's minter list. Once minter is removed, it
 * will no longer be able to mint or burn tokens.
 *
 * parameter is address
 */
function removeMinter
  ( const parameter : removeMinterParams
  ; const store     : storage
  ) : entrypoint is block
{
  authorizeMasterMinter (store);

  const upd : list (operation) = nil;
  var ups : storage := store;

  case ups.ledger[Tezos.sender] of
    Some (ledgerValue) ->
      // TODO: currently it adds an address if it's non-present
      remove parameter from map ledgerValue.approvals
  | None -> failwith ("Sender address is not present in ledger")
  end
} with (upd, ups)

/*
 * Produces tokens to a wallet associated with the given address
 *
 * parameter is record
 * [ toAddress : address
 * ; amount    : nat
 * ]
 */
function mint
  ( const parameter : mintParams
  ; const store     : storage
  ) : entrypoint is block
{ authorizeMasterMinter (store) }
with (creditTo (parameter, store))

/*
 * Decreases balance and total supply of otkens by the given amount
 *
 * parameter is record
 * [ fromAddress : address
 * ; amount      : nat
 * ]
 */
function burn
  ( const parameter : burnParams
  ; const store     : storage
  ) : entrypoint is block
{ authorizeMasterMinter (store) }
with (debitFrom (parameter, store))
