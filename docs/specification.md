<!--
SPDX-FileCopyrightText: 2020 tqtezos
SPDX-License-Identifier: MIT
-->

**Overview**
------------

These specifications were assembled with the following references:

- [*CENTRE Fiat Token design*](https://github.com/centrehq/centre-tokens/blob/78d964a1a8d481ffd8152772d7a66e47df54b3db/doc/tokendesign.md) which is served as a reference specification to tezos stablecoin smart contract.

- Tezos Token Standard:
  [*FA2*](https://gitlab.com/tzip/tzip/-/blob/76d5f3791bfbfe3c9bf95ad5ec5fc6cbeeca2d0e/proposals/tzip-12/tzip-12.md) and
  [*ManagedLedger*](https://gitlab.com/tzip/tzip/blob/ae2f1e7ebb3454d811a2bea3cd0698b0e64ccea5/proposals/tzip-7/ManagedLedger.md)

- [*Michelson Contract Interfaces and Conventions*](https://gitlab.com/tzip/tzip/blob/ae2f1e7ebb3454d811a2bea3cd0698b0e64ccea5/proposals/tzip-4/tzip-4.md) TZIP which defines `view` and `void` type synonyms

**General Requirements**
------------------------

- The token(s) must be and FA2 compatible as to facilitate listing on
  exchanges and interaction with services which support FA2

**Ledger**
==========

Every address that is stored in ledger is associated with its
current balance, token id and current minting allowance.

**Roles**
=========

The token supports the following list of user roles as is described in
CENTRE Fiat Token specification and the taxonomy of permission policies that are described in FA2 token specification.

<!-- Here're the roles that are present in current version of stablecoin token.  -->

Below we define some roles for stablecoin contract token.

**owner**

- Can re-assign any role of the token.

**masterMinter**

- Can add and remove minters.

- Can change minting allowance for any minter if that is present in
  its list of minters.

- Stores the list of current minters within itself, rather
  than exposing the map of master minter and its minters globally
  for the whole token.

**minter**

- Can create and destroy coins (that are allowed by their current
  minting allowance).

- Minting allowance is stored locally within the address
  allowing for multiple minters creating and destroying tokens.

**pauser**

- Can pause transferring, burning and minting operations.
  During the pause, these operations cannot be performed
  and fail with an error if the user decides to try them.

**blacklister (TBD)**

- Can blacklist a particular address preventing it from
  transferring, minting, burning and receiving tokens by
  removing them from whitelist.

**Token Functions**
===================

**Standard Token Functions**
----------------------------

Functions for the stablecoin token implementation which are common to the [*FA2 Tezos
Token Standard*](https://gitlab.com/tzip/tzip/-/blob/76d5f3791bfbfe3c9bf95ad5ec5fc6cbeeca2d0e/proposals/tzip-12/tzip-12.md). The
token implementation in general follows an [*event loop*](https://en.wikipedia.org/wiki/Event_loop) pattern meaning that every parameter
handles an additional callback call describing what needs to be done with
its result.

**transfer**

Types
```
transferParam
  ( address :from_
  , address :to_
  , nat     :value
  )
transfer = list transferParam
```

Parameter (in Michelson):
```
(list %transfer
  (pair
    (address %from_)
    (pair
      (address %to_)
      (nat %amount)
  ))
)
```

- Transfers the given amounts of tokens between addresses.

- Each transfer must happen atomically, if one of them fails, then
  the whole transaction fail.

- Transfers must follow permission policies that are described above.

- The amount of a token transfer must not exceed the existing token
  owner's balance. If the transfer amount exceeds the existing balance,
  then the whole transfer operation fail.

- Core transfer behavior may be extended. If additional constraints on tokens
  transfer are required, FA2 token contract implementation may invoke
  additional permission policies. If the additional permission hook fails,
  then the whole transfer fail.

- Core transfer behavior must update token balances exactly as the
  operation parameters specify it. No changes to amount values or
  additional transfers are allowed.

- Sender and receiver addresses must be whitelisted.

- Contract must not be paused.

**getBalance**

Types
```
getBalanceRequest = address
getBalanceResponse =
  ( getBalanceRequest :request
  , nat               :balance
  )
getBalanceParam =
  ( list getBalanceRequest             :requests
  , contract (list getBalanceResponse) :callback
  )
getBalance = getBalanceParam
```

Parameter (in Michelson):
```
(pair %getBalance
  (list %requests address)
  (contract %callback
    (list
      (pair
        (address %owner)
        (nat %balance)
      )
    )
  )
)
```

- Returns current balance of mutliple addresses. It accepts a list
  of `getBalanceRequest` and a callback which accepts a list of
  `getBalanceResponse` which is conequently passed to it.

**getTotalSupply**

Types
```
getTotalSupply = (contract nat)
```

Parameter (in Michelson)
```
(contract %getTotalSupply nat)
```

- Returns total supply for the token. It accepts a callback which
  accepts current total supply amount.

**permissionsDescriptor**

Types
```
selfTransferPolicy =
  | SelfTransferPermitted
  | SelfTransferDenied
operatorTransferPolicy =
  | OperatorTransferPermitted
  | OperatorTransferDenied
ownerTransferPolicy =
  | OwnerNoOp
  | OptionalOwnerHook
  | RequiredOwnerHook
customPermissionPolicy =
  ( string         :tag
  , option address :configApi
  )
permissionsDescriptorParam
  ( selfTransferPolicy            :self
  , operatorTransferPolicy        :receiver
  , ownerTransferPolicy           :operator
  , ownerTransferPolicy           :sender
  , option customPermissionPolicy :custom
  )
permissionsDescriptor = contract permissionsDescriptorParam
```

Parameter (in Michelson)
```
(contract %permissionsDescriptor
  (pair
    (or %self
      (unit %selfTransferPermitted)
      (unit %selfTransferDenied)
    )
    (pair
      (or %operator
        (unit %operatorTransferPermitted)
        (unit %operatorTransferDenied)
      )
      (pair
        (or %receiver
          (unit %ownerNoOp)
          (or
            (unit %optionalOwnerHook)
            (unit %requiredOwnerHook)
        ))
        (pair
          (or %sender
            (unit %ownerNoOp)
            (or
              (unit %optionalOwnerHook)
              (unit %requiredOwnerHook)
          ))
        (option %custom
          (pair
            (string %tag)
            (option %configApi address)
          )
        )
  ))))
)
```

- Get the descriptor of the transfer permission policy. This allows
  for external contracts to discover an FA2 contract's permission
  policy as well as to configure it. For additional information
  please refer to FA2 token contract specification.

- Some of the permission options require an additional configuration
  API which can be implemented either within FA2 token contract
  itself or in a separate contract.

Each address that participates in transfer is separated into 2 types:
`operator` and `owner`. `operator` is a Tezos address that initiates
token tranfser operation on behalf of the `owner` that actually holds
tokens.

**updateOperators**

Types
```
updateOperatorParam =
  | AddOperator    address
  | RemoveOperator address
updateOperators = list updateOperatorParam
```

Parameter (in Michelson)
```
(list %updateOperators
  (or
    (address %addOperator)
    (address %removeOperator)
  )
)
```

- Add or Remove operators for sender.

- If two different commands in the list add and remove an operator
  for the same token type, then the last one must take effect.

- Sender and operation receiver must be whitelisted.

- Contract must not be paused.

**approveOperatorUpdate**

Types
```
approveOperatorUpdate = address
```

Parameter (in Michelson):
```
(address %approveOperatorUpdate)
```

- Enables given address to operate on tokens held by sender.

- Sender must be whitelisted.

- Contract must not be paused.

**isOperator**

Types
```
isOperatorResponse =
  ( address :operator
  , bool    :isOperator
  )
isOperatorParam =
  ( address                     :operator
  , contract isOperatorResponse :callback
  )
isOperator = isOperatorParam
```

Parameter (in Michelson):
```
(pair %isOperator
  (address %operator)
  (contract %callback
    (pair %isOperatorResponse
      (address %operator)
      (bool %isOperator)
    )
  )
)
```

- Inspect if an address is an operator for sender.

**pause**

Types
```
pause = unit
```

Parameter (in Michelson)
```
unit
```

- Pauses transferring, burning and minting operations so
  that they cannot be performed. All other operations remain
  unaffected.

- Cannot be called multiple times.

- Sender must be a pauser.

**unpause**

Types
```
unpause = unit
```

Parameter (in Michelson)
```
unit
```

- Unpauses the contract so that transferring, burning and minting
  operations can be performed by users with corresponding roles.

- Cannot be called multiple times.

- Sender must be a pauser.

**getPaused**

Types
```
getPaused = contract bool
```

Parameter (in Michelson)
```
(contract bool)
```

- Returns token pause status.

**Custom token functions**
==========================

**Issuing and destroyng tokens**
--------------------------

**configureMinter**

Types
```
configureMinter = address
```

Parameter (in Michelson)
```
address
```

- Adds `minter` to global minter list to allow him mint tokens.

- Sender must be master minter.

- Minter cannot mint tokens until `setMintingAllowance`
  is specified.

**removeMinter**

Types
```
removeMinter = address
```

Parameter (in Michelson)
```
address
```

- Removes minter from the global minter list and sets its minting
  allowance to 0. Once minter is removed it will no longer be able
  to mint or burn tokens.

- Sender must be master minter.

**setMintingAllowance**

Types
```
setMintingAllowance =
  ( address :minter
  , nat     :amount
  )
```

Parameter (in Michelson)
```
(pair %setMintingAllowance
  (address %minter)
  (nat %amount)
)
```

- Set the amount of allowed minting allowance for an address.

- Minter must be in minter list.

- Sender and minter must be whitelisted.

**getMintingAllowance**

Types
```
getMintingAllowanceResponse =
  ( address :minter
  , nat     :allowance
  )
getMintingAllowanceParam =
  ( list address                                :requests
  , contract (list getMintingAllowanceResponse) :callback
  )
getMintingAllowance = getMintingAllowanceParam
```

Parameter (in Michelson):
```
(pair %getMintingAllowance
  (list %requests address)
  (contract %callback
    (list %getMintingAllowanceResponse
      (pair
        (address %owner)
        (nat %allowance)
      )
    )
  )
)
```

- Returns current minting allowance for a list of addresses and a
  callback which accepts pairs of addresses and their allowances.

**getTotalMinted**

Types
```
getTotalMinted = contract nat
```

Parameter (in Michelson)
```
(contract nat)
```
- Returns current amount of minted coins for a token.

-- TODO: clarify under which conditions and how should
-- token reset this allowance

**setMintingAllowanceResetInterval**

Types
```
setMintingAllowanceResetInterval = timestamp
```

Parameter (in Michelson)
```
(timestamp %setMintingAllowanceResetInterval)
```

- Set the default interval of minting allowance reset.

- Set to 0 to make disable minting allowance reset by stablecoin token.

TODO: minting parameter probably should be a pair which also accepts a callback

**mint**

Types
```
mintParam
  ( address :recipient
  , nat     :value
  )
mint = list mintParam
```

Parameter (in Michelson):
```
(list %mint
  (pair
    (address %recipient)
    (nat %value)
  )
)
```

- Produces the given amount of tokens to the wallets associated with the
  given addresses.

- Each minting must happen atomically, so if one of them fails, then the whole
  operation must fail.

- The operation must follow permission policies described above.

- Receiving addresses must be whitelisted.

- Sender must be minter and be in global minter list.

- The total amount of minted coins must not exceed the current
  minting allowance specified for each minter individually.

- Minting allowance will decrease by the amount of tokens minted
  and increase the balance of receiver and total minted value.

- Contract must not be paused.

**burn**

Types
```
burn = nat
```

Parameter (in Michelson):
```
nat
```

- Decreases balance of sender wallet specified by the given amount.

- The operation must follow permission policies described above.

- Sender must be minter and must have a sufficient amount of
  funds to be destroyed and also be in global minter list.

- A minter with 0 minting allowance is allowed to burn tokens.

- Contract must not be paused.

**Whitelisting (TBD)**
----------------

Whitelisting functions for the TZUSDC token implementation which are outside the FA1.2 Tezos Token Standard.

**addToWhitelist** address

- Add an address to whitelist, enabling it to transfer and
  approve tokens as well as modify allowances.

- Sender must be a blacklister.

**addToWhitelistBatch** (list address)

- Adds each address to whitelist, enabling it to transfer and
  approve tokens as well as modify allowances.

- Sender must be a blacklister.

**removeFromWhitelist** address

- Remove address from whitelist.

- Sender must be a blacklister.

**removeFromWhitelistBatch** (list address)

- Remove provided addresses from whitelist.

- If some of specified addresses are not in whitelist, then
  it fails returning list of those addresses.

- Sender must be a blacklister.

**checkWhitelisted** (void address bool)

- Return a boolean value describing whether the
  given address is present in whitelist.

Role reassigning functions
==========================

**Owner**
---------

**transferOwnership**

Types
```
transferOwnership = address
```

Parameter (in Michelson):
```
address
```

- Set token owner to a new address.

- Sender must be current token owner.

- The current owner retains his priveleges up until
  `acceptOwnership` is called.

- Can be called multiple times, each call replaces pending
  owner with the new one. Note, that if proposed
  owner is the same as the current one, then the call
  is simply invalidated.

**getOwner**

Types
```
getOwner = contract address
```

Parameter (in Michelson):
```
contract address
```

- Returns current token owner address.

**acceptOwnership**

Types
```
acceptOwnership = unit
```

Parameter (in Michelson):
```
unit
```

- Accept ownership privileges.

- Sender must be a pending owner.

**Master Minter**
---------------

**changeMasterMinter**

Types
```
changeMasterMinter = address
```

Parameter (in Michelson):
```
address
```

- Set master minter to a new address.

- Sender must be token owner.

- The current master minter retains his priveleges up until
  `acceptMasterMinterRole` is called.

- Can be called multiple times, each call replaces pending
  master minter with the new one. Note, that if proposed
  master minter is the same as the current one, then the call
  is simply invalidated.

**getMasterMinter**

Types
```
getMasterMinter = contract address
```

Parameter (in Michelson):
```
contract address
```

- Return current master minter address.

**acceptMasterMinterRole**

Types
```
acceptMasterMinterRole = unit
```

Parameter (in Michelson):
```
unit
```

- Accept master minter privileges.

- Sender must be a pending master minter.

**Pauser**
----------

**changePauser**

Types
```
changePauser = address
```

Parameter (in Michelson):
```
address
```
- Set pauser to a new address.

- Sender must be token owner.

- The current pauser retains his priveleges up until
  `acceptPauserRole` is called.

- Can be called multiple times, each call replaces pending
  pauser with the new one. Note, that if proposed pauser is
  the same as the current one, then the call is simply
  invalidated.

**getPauser**

Types
```
getPauser = contract address
```

Parameter (in Michelson):
```
contract address
```

- Return current pauser address.

**acceptPauserRole**

Types
```
acceptPauserRole = unit
```

Parameter (in Michelson):
```
unit
```
- Accept pauser privileges.

- Sender must be a pending pauser.

**Blacklister (TBD)**
---------------

**changeBlacklister** address

- Set pauser to a new address.

- Sender must be token owner.

- The current pauser retains his priveleges up until
  `acceptBlacklister` is called.

- Can be called multiple times, each call replaces pending
  blacklister with the new one. Note, that if proposed
  blacklister is the same as the current one, then the call
  is simply invalidated.

**getBlacklister** (view unit address)

- Return current blacklister address.

**acceptBlacklisterRole** unit

- Accept blacklister privileges.

- Sender must be a pending blacklister.