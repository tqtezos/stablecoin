// SPDX-FileCopyrightText: 2020 tqtezos
// SPDX-License-Identifier: MIT

/*
 * Stablecoin smart-contract
 */

/* ------------------------------------------------------------- */

/*
 * Parameter
 */

type tokenId is nat



type transferParam is record
  id          : tokenId
; fromAddress : address
; toAddress   : address
; amount      : nat
end

type transferParams is list (transferParam)



type balanceOfRequest is record
  id      : tokenId
; owner   : address
end

type balanceOfResponse is record
  request : balanceOfRequest
; balance : nat
end

type balanceOfParams is record
  requests : list (balanceOfRequest)
; callback : contract (list (balanceOfResponse))
end



type getTotalSupplyResponse is record
  id          : tokenId
; totalSupply : nat
end

type getTotalSupplyParams is record
  ids      : list (tokenId) // list of requests
; callback : contract (list (getTotalSupplyResponse))
end



type getTokenMetadataResponse is record
  id       : tokenId
; symbol   : string
; name     : string
; decimals : nat
; extras   : map (string, string)
end

type getTokenMetadataParams is record
  id       : tokenId
; callback : contract (list (getTokenMetadataResponse))
end



type operatorTokens is
| AllTokens
| SomeTokens of set (tokenId)

type operatorParam is record
  owner    : address
; operator : address
; tokens   : operatorTokens
end

type updateOperator is
| AddOperator    of operatorParam
| RemoveOperator of operatorParam

type updateOperatorParams is list (updateOperator)

type isOperatorResponse is record
  operator   : operatorParam
; isOperator : bool
end

type isOperatorParams is record
  operator : operatorParam
; callback : contract (isOperatorResponse)
end

/* ------------------------------------------------------------- */

type selfTransferPolicy is
| SelfTransferPermitted
| SelfTransferDenied

type operatorTransferPolicy is
| OperatorTransferPermitted
| OperatorTransferDenied

type ownerTransferPolicy is
| OwnerNoOp
| OptionalOwnerHook
| RequiredOwnerHook

type customPermissionPolicy is record
  tag       : string
; configApi : option (address)
end

type permissionsDescriptor is record
  self : selfTransferPolicy
; operator : operatorTransferPolicy
; receiver : ownerTransferPolicy
; sender : ownerTransferPolicy
; custom : option (customPermissionPolicy)
end

type getPermissionsDescriptorParams is
  contract (permissionsDescriptor)

/*
 * TODO: currently LIGO does not provide capability of
 * working with lambdas properly
 */
type setTransferHookParams is record
  hook : unit -> contract (transferParam)
; permissionsDescriptor : permissionsDescriptor
end

/* ------------------------------------------------------------- */
/* Should be unused */

type allowanceParams is record
  owner   : address
; spender : address
; value   : nat
end

type pauseParams is unit

type unpauseParams is unit

type configureMinterParams is address

type setMintingAllowanceParams is record
  minter : address
; amount : nat
end

type removeMinterParams is address

type mintParams is record
  toAddress : address
; amount    : nat
end

type mintBatchParams is list (mintParams)

type burnParams is record
  fromAddress : address
; amount      : nat
end

type parameter is
// FA2
  Transfer                 of transferParams
| BalanceOf                of balanceOfParams
| GetTotalSupply           of getTotalSupplyParams
| GetTokenMetadata         of getTokenMetadataParams
| GetPermissionsDescriptor of getPermissionsDescriptorParams
| UpdateOperators          of updateOperatorParams
| IsOperator               of isOperatorParams
// Custom
| Pause               of pauseParams
| Unpause             of unpauseParams
| ConfigureMinter     of configureMinterParams
| SetMintingAllowance of setMintingAllowanceParams
| RemoveMinter        of removeMinterParams
| Mint                of mintParams
| Burn                of burnParams

/* ------------------------------------------------------------- */

/*
 * Storage
 */

type roles is record
  owner        : address
; pauser       : address
; masterMinter : address
end

type storageSkeleton is record
  roles       : roles
; minters     : list (address)
; paused      : bool
; totalSupply : nat
end

type approvals is map (address, nat)

type ledgerValue is record
  balance   : nat
; operator  : address
; approvals : approvals
end

type ledger is map (address, ledgerValue)

type storage is record
  ledger : ledger
; fields : storageSkeleton
end

type entrypoint is list (operation) * storage
