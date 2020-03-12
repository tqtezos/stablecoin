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
current balance, current allowance and a list of minters.

**Roles**
=========

The token supports the following list of user roles as is described in
CENTRE Fiat Token specification and the taxonomy of permission policies that are described in FA2 token specification.

<!-- Here're the roles that are present in current version of stablecoin token.  -->

Below we define some roles for stablecoin contract token.

**owner**

- Can re-assign any role of the token

**masterMinter**

- Can add and remove minters

- Can increase minting allowance for any minter

- Stores the list of current minters within itself, rather
  than exposing the map of master minter and its minters globally
  for the whole token

**minter**

- Can create and destroy coins (that are allowed by their current minting allowance)

- Minting allowance is stored locally within the address
  allowing for multiple minters creating and destroying tokens.

**pauser**

- Can pause transferring, burning and minting operations.
  During the pause, these operations cannot be performed
  and remain unaffected

**blacklister (TBD)**

- Can blacklist a particular address preventing it from
  transferring, minting,burning and receiving tokens by
  removing them from whitelist

**Token Functions**
===================

**Standard Token Functions**
----------------------------

Functions for the stablecoin token implementation which are common to the [*FA2 Tezos
Token Standard*](https://gitlab.com/tzip/tzip/-/blob/76d5f3791bfbfe3c9bf95ad5ec5fc6cbeeca2d0e/proposals/tzip-12/tzip-12.md)

**transfer**

Types
```
id = nat

transferParam
  ( address :from_
  , address :to_
  , id      :id
  , amount  :nat
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
      (pair
        (nat %id)
        (nat %amount)
  )))
)
```

- Transfers given amounts of tokens between addresses.

- Each tarnsfer must happen atomically, if one of them fails, then
  the whole transaction fail.

- Transfers must follow permission policies that are described above.

- The amount of a token transfer must not exceed the existing token
  owner's balance. If the transfer amount for the particular token
  type and token owner exceeds the existing balance, then the whole
  transfer operation fail.

- Core transfer behavior may be extended. If additional constraints   on tokens transfer are required, FA2 token contract
  implementation may invoke additional permission policies. If the
  additional permission hook fails, then the whole transfer fail.

- Core transfer behavior must update token balances exactly as the
  operation parameters specify it. No changes to amount values or
  additional transfers are allowed.

- Sender must be a minter.

- Sender and receiver addresses must be whitelisted.

- Contract must not be paused.

**getBalance**

Types
```
id = nat

getBalanceRequest =
  ( address :owner
  , id      :id
  )

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
  (list %requests
    (pair
      (address %owner)
      (nat %id)
    )
  )
  (contract %callback
    (list
      (pair
        (pair %request
          (address %owner)
          (nat %id)
        )
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
id = nat

getTotalSupplyResponse =
  ( id  :id
  , nat :totalSupply
  )

getTotalSupplyParam =
  ( list id                                :ids
  , contract (list getTotalSupplyResponse) :callback
  )

getTotalSupply = getTotalSupplyParam
```

Parameter (in Michelson)
```
(pair %getTotalSupply
  (list %ids nat)
  (contract %callback
    (list
      (pair %getTotalSupplyResponse
        (nat %id)
        (nat %totalSupply)
      )
    )
  )
)
```

- Returns total supply for multiple tokens. It accepts a list of
  token ids and a callback which accepts a list of
  `getTotalSupplyResponse` which is conequently passed to it.

**getTokenMetadata**

Types
```
id = nat

getTokenMetadataResponse =
  ( id                   :id
  , string               :symbol
  , string               :name
  , nat                  :decimals
  , map (string, string) :extras
  )

getTokenMetadataParam =
  ( list id                                  :ids
  , contract (list getTokenMetadataResponse) :callback
  )

getBalance = getBalanceParam
```

Parameter (in Michelson)
```
(pair %tokenMetadata
  (list %ids nat)
  (contract %callback
    (list
      (pair %getTokenMetadataResponse
        (nat %id)
        (pair
          (string %symbol)
          (pair
            (string %name)
            (pair
              (nat %decimals)
              (map %extras string string)
      ))))
    )
  )
)
```

- Get a list of token metadata for multiple tokens. It accepts a
  list of token ids and a callback which accepts a list of
  `getTokenMetadataResponse` which is consequently passed to it.

- The smallest amount of tokens which may be transferred, burned or
  minted is always 1.

- `decimals` describe the number of digits to use after the decimal
  point when displaying the token amounts and must not affect
  transaction in any way.

**permissionsDescriptor**

Types
```
id = nat

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

permissionsDescriptor contract permissionsDescriptorParam
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

Each minter is separated into 2 types of addresses: `operator` and
`owner`. `operator` is a Tezos address that initiates token tranfser
operation on behalf of the `owner` that actually holds tokens.

**updateOperators**

Types
```
id = nat

operatorTokens =
  | AllTokens
  | SomeTokens (set id)

operatorParam =
  ( address        :owner
  , address        :operator
  , operatorTokens :tokens
  )

updateOperatorParam =
  | AddOperator    operatorParam
  | RemoveOperator operatorParam

updateOperators = list updateOperatorParam
```

Parameter (in Michelson)
```
(list %updateOperators
  (or
    (pair %addOperator
      (address %owner)
      (pair
        (address %operator)
        (or %tokens
          (unit %allTokens)
          (set %someTokens nat)
        )
    ))
    (pair %removeOperator
      (address %owner)
      (pair
        (address %operator)
        (or %tokens
          (unit %allTokens)
          (set %someTokens nat)
        )
    ))
  )
)
```

- Add or Remove token operators for the specified owners and token
  types.

- If two different commands in the list add and remove an operator
  for the same token type, then the last one must take effect.

- It's possible to update an operator for some specific tokens, or
  to all tokens (TODO: discuss).

- Sender must be minter.

- Sender and operation receiver must be whitelisted.

- Contract must not be paused.

**approveOperatorUpdate (TODO)**

Types
```
operatorTokens =
  | AllTokens
  | SomeTokens (set id)

operatorParam =
  ( address        :owner
  , address        :operator
  , operatorTokens :tokens
  )

approveOperatorUpdate = operatorParam
```

Parameter (in Michelson):
```
(pair %approveOperatorUpdate
  (pair %operator
      (address %owner)
      (pair
        (address %operator)
        (or %tokens
          (unit %allTokens)
          (set %someTokens nat)
        )
    ))
  (contract %callback
    (pair %operator
      (address %owner)
      (pair
        (address %operator)
        (or %tokens
          (unit %allTokens)
          (set %someTokens nat)
        )
      )
    )
  )
)
```

- Enables `operator` to operate on tokens held by `owner`.

- `owner` and `operator` must be whitelisted.

- Contract must not be paused.

**isOperator**

Types
```
id = nat

operatorTokens =
  | AllTokens
  | SomeTokens (set id)

operatorParam =
  ( address        :owner
  , address        :operator
  , operatorTokens :tokens
  )

isOperatorResponse =
  ( operatorParam :operator
  , bool          :isOperator
  )

isOperatorParam =
  ( operatorParam               :operator
  , contract isOperatorResponse :callback
  )

isOperator = isOperatorParam
```

Parameter (in Michelson):
```
(pair %isOperator
  (pair %operator
    (address %owner)
    (pair
      (address %operator)
      (or %tokens
        (unit %allTokens)
        (set %someTokens nat)
      )
  ))
  (contract %callback
    (pair
      (pair %operator
        (address %owner)
        (pair
          (address %operator)
          (or %tokens
            (unit %allTokens)
            (set %someTokens nat)
          )
      ))
      (bool %isOperator)
    )
  )
)
```

- Inspect if an address is an operator for the specified owner and    token types.

- If the adddress is not an operator for at least one
  requested token type, then the result is `false`.

- It's possible to make this query for some specific tokens, or to
  all tokens.

**Managed Ledger Functions**
----------------------------

Functions for the TZUSDC token implementation found in Tezos’ [*ManagedLedger*](https://gitlab.com/tzip/tzip/blob/ae2f1e7ebb3454d811a2bea3cd0698b0e64ccea5/proposals/tzip-7/ManagedLedger.md)

**pause** unit

- Pauses transferring, burning, minting and allowance operations so
  that they cannot be performed. All other operations remain
  unaffected.

- Cannot be called multiple times.

- Sender must be a pauser.

**unpause** unit

- Unpauses the contract so that transferring, burning, minting and
  allowance operations can be performed by users with corresponding
  roles.

- Cannot be called multiple times.

- Sender must be a pauser.

**getPaused**

Types
```
id = nat

getPausedResponse =
  ( id  :id
  , nat :paused
  )

getPausedParam =
  ( list id                           :ids
  , contract (list getPausedResponse) :callback
  )

getPuased = getPausedParam
```

Parameter (in Michelson)
```
(pair %getTotalPaused
  (list %ids nat)
  (contract %callback
    (list
      (pair %getPausedResponse
        (nat %id)
        (bool %paused)
      )
    )
  )
)
```

- Returns token pause status.

- *Note: It's not a part of ManagedLedger interface*.

**Custom token functions**
==========================

**Issuing and destroyng tokens**
--------------------------

**configureMinter** address

- Adds `minter` to sender's minter list.

- Sender must master minter.

- Minter cannot mint tokens untill `setMintingAllowance`
  is specified.

- Cannot add minter if present in someone else's minter list.

**setMintingAllowance** (address :minter, nat :amount)

- Set the amount of allowed minting allowance for a specific address.

- Minter must present in sender's minter list.

**getMintingAllowance** (void address nat)

- Returns current minting allowance for an address.

**getTotalMinted** (void address nat)

- Returns current amount of minted coins for an address.

-- TODO: clarify under which conditions and how should
-- token reset this allowance

**setMintingAllowanceResetInterval** timestamp

- Set the default interval of minting allowance reset.

- Set to 0 to make disable minting allowance reset by TZUSDC
  token.

**removeMinter** address

- Removes minter from the sender's minter list and sets its
  minting allowance to 0. Once minter is removed it will no
  longer be able to mint or burn tokens.

- Sender must be master minter.

- Minter address must present in sender's minter list.

**mint** (address :to, nat :value)

- Produces tokens to the wallet associated with the given address.

- Receiving address must be whitelisted.

- Sender must have `minter` role.

- The total amount of minted coins must not exceed the current
  minting allowance.

- Minting allowance will descrease by the amount of tokens minted
  and increase the balance of receiver and total minted value.

**mintBatch** (list (address :to, nat :value))

- Produces tokens on the account associated with each given
  address.

- Receiving addresses must be whitelisted.

- Sender must have `minter` role.

- The total amount of minted coins must not exceed the current
  amount of the minting allowance.

- Minting allowance will decrease and the balance for each given
  address will increase accordingly to amounts provided within
  and the total minted value will be increased as well.

**burn** nat

- Decreases the balance and total supply of tokens by the given
  amount.

- Sender must have `minter` role and sufficient amount of funds
  to be destroyed.

- A minter with 0 minting allowance is allowed to burn tokens.

**approveCAS** (address :spender, nat :value, nat :expected)

- Enables spender to withdraw given amount from sender.

- Sender and spender must be whitelisted.

- Expected value must match the current sender allowance value.

**Whitelisting (TBD)**
----------------

Functions for the TZUSDC token implementation which are outside the FA1.2 Tezos Token Standard.

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

**transferOnership** address

- Set token owner to a new address.

- Sender must be current token owner.

- The current owner retains his priveleges up until
  `acceptOwnership` is called.

- Can be called multiple times, each call replaces pending
  owner with the new one. Note, that if proposed
  owner is the same as the current one, then the call
  is simply invalidated.

**getAdmin** (view unit address)

- Return token administrator address.

**acceptOwnership** unit

- Accept ownership privileges.

- Sender must be a pending owner.

**Admin**
---------

**changeTokenAdmin** address

- Set token administrator to a new address.

- Sender must be current token admin.

- The current administrator retains his priveleges up until
  `acceptAdminRole` is called.

- Can be called multiple times, each call replaces pending
  administrator with the new one. Note, that if proposed
  administrator is the same as the current one, then the call
  is simply invalidated.

**getAdmin** (view unit address)

- Return token administrator address.

**acceptAdminRole** unit

- Accept administrator privileges.

- Sender must be a pending administrator.

**Master Minter**
---------------

**changeMasterMinter** address

- Set master minter to a new address.

- Sender must be token owner.

- The current master minter retains his priveleges up until
  `acceptMasterMinterRole` is called.

- Can be called multiple times, each call replaces pending
  master minter with the new one. Note, that if proposed
  master minter is the same as the current one, then the call
  is simply invalidated.

**getMasterMinter** (view unit address)

- Return current master minter address.

**acceptMasterMinterRole** unit

- Accept master minter privileges.

- Sender must be a pending master minter.

**Pauser**
----------

**changePauser** address

- Set pauser to a new address.

- Sender must be token owner.

- The current pauser retains his priveleges up until
  `acceptPauserRole` is called.

- Can be called multiple times, each call replaces pending
  pauser with the new one. Note, that if proposed pauser is
  the same as the current one, then the call is simply
  invalidated.

**getPauser** (view unit address)

- Return current pauser address.

**acceptPauserRole** unit

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