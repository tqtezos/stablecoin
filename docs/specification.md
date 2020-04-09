<!--
SPDX-FileCopyrightText: 2020 tqtezos
SPDX-License-Identifier: MIT
-->

# Overview

These specifications were assembled with the following references:

- [*CENTRE Fiat Token design*](https://github.com/centrehq/centre-tokens/blob/78d964a1a8d481ffd8152772d7a66e47df54b3db/doc/tokendesign.md) which is served as a reference specification to tezos stablecoin smart contract.

- Tezos Token Standard:
  [*FA2*](https://gitlab.com/tzip/tzip/-/blob/76d5f3791bfbfe3c9bf95ad5ec5fc6cbeeca2d0e/proposals/tzip-12/tzip-12.md)

- [*Michelson Contract Interfaces and Conventions*](https://gitlab.com/tzip/tzip/blob/ae2f1e7ebb3454d811a2bea3cd0698b0e64ccea5/proposals/tzip-4/tzip-4.md) TZIP which defines `view` and `void` type synonyms

# General Requirements

- The token contract must be FA2 compatible as to facilitate listing on
  exchanges and interaction with services which support FA2.

- The token contract must store tokens of a single type.

# Ledger

Every address that is stored in ledger is associated with its
current balance and current minting allowance.

# Roles

The token supports the following list of user roles as is described in
CENTRE Fiat Token specification:

* **owner**
  - Can assign and re-assign any role of the token.
  - There always must be exactly one owner.

* **master minter**
  - Can add and remove minters.
  - Can change minting allowance for any minter.
  - There always must be exactly one master minter.

* **minter**
  - Can create and destroy coins (that are allowed by their current
    minting allowance).
  - Minting allowance is stored locally within the address
    allowing for multiple minters creating and destroying tokens.
  - There can be any number of minters.
<!-- TODO: clarify with the customer whether there is an upper limit -->

* **pauser**
  - Can pause transferring, burning and minting operations.
    During the pause, these operations cannot be performed
    and fail with an error if the user decides to try them.
  - There always must be exactly one pauser.

* **blacklister (TBD)**
  - Can blacklist a particular address preventing it from
    transferring, minting, burning and receiving tokens by
    removing them from whitelist.
  - There are no clear requirements yet. It can be modified or even removed.

# Entrypoints

## Standard FA2 Token Functions

Functions for the stablecoin token implementation which are common to the [*FA2 Tezos
Token Standard*](https://gitlab.com/tzip/tzip/-/blob/76d5f3791bfbfe3c9bf95ad5ec5fc6cbeeca2d0e/proposals/tzip-12/tzip-12.md).
Some entrypoints (e. g. getters) follow an [*event loop*](https://en.wikipedia.org/wiki/Event_loop)
pattern meaning that their arguments have an additional callback call
describing what needs to be done with its result.

**transfer**

Types
```
token_id = nat

transfer_param
  ( address :from_
  , address :to_
  , token_id :token_id
  , nat     :value
  )

transfer = list transfer_param
```

Parameter (in Michelson):
```
(list %transfer
  (pair
    (address %from_)
    (pair
      (address %to_)
      (pair
        (nat %token_id)
        (nat %amount)
  )))
)
```

- Transfers given amounts of tokens between addresses.

- Since the contract supports only a single token type, `token_id` must be 0.
  It is passed because FA2 requires that.

- Each transfer must happen atomically, if one of them fails, then
  the whole transaction fail.

- Transfers must follow permission policies that are described above.

- The amount of a token transfer must not exceed the existing token
  owner's balance. If the transfer amount for the particular token
  type and token owner exceeds the existing balance, then the whole
  transfer operation fail.

- Core transfer behavior may be extended. If additional constraints on tokens
  transfer are required, FA2 token contract implementation may invoke
  additional permission policies. If the additional permission hook fails,
  then the whole transfer fail.

- Core transfer behavior must update token balances exactly as the
  operation parameters specify it. No changes to amount values or
  additional transfers are allowed.

- Contract must not be paused.

**balance_of**

Types
```
token_id = nat

balance_of_request =
  ( address :owner
  , token_id :token_id
  )

balance_of_response =
  ( balance_of_request :request
  , nat :balance
  )

balance_of_param =
  ( list balance_of_request :requests
  , contract (list balance_of_response) :callback
  )

balance_of = balance_of_param
```

Parameter (in Michelson):
```
(pair %balance_of
  (list %requests
    (pair
      (address %owner)
      (nat %token_id)
    )
  )
  (contract %callback
    (list
      (pair
        (pair %request
          (address %owner)
          (nat %token_id)
        )
        (nat %balance)
      )
    )
  )
)
```

- Returns current balance of mutliple addresses. It accepts a list
  of `balance_of_request` and a callback which accepts a list of
  `balance_of_response` which is conequently passed to it.

- Since the contract supports only a single token type, `token_id` must be 0.
  It is passed because FA2 requires that.

**total_supply**

Types
```
token_id = nat

total_supply_response =
  ( token_id :token_id
  , nat :total_supply
  )

total_supply_param =
  ( list token_id :token_ids
  , contract (list total_supply_response) :callback
  )

total_supply = total_supply_param
```

Parameter (in Michelson)
```
(pair %total_supply
  (list %token_ids nat)
  (contract %callback
    (list
      (pair
        (nat %token_id)
        (nat %total_supply)
      )
    )
  )
)
```

- Returns total supply for multiple tokens. It accepts a list of
  token ids and a callback which accepts a list of
  `total_supply_response` which is conequently passed to it.

- Since the contract supports only a single token type, `token_id` must be 0.
  It is passed because FA2 requires that.

**get_token_metadata**

Types
```
token_id = nat

get_token_metadata_response =
  ( token_id             :token_id
  , string               :symbol
  , string               :name
  , nat                  :decimals
  , map (string, string) :extras
  )

get_token_metadata_param =
  ( list token_id                               :token_ids
  , contract (list get_token_metadata_response) :callback
  )

get_token_metadata = get_token_metadata_param
```

Parameter (in Michelson)
```
(pair %token_metadata
  (list %token_ids nat)
  (contract %callback
    (list
      (pair %get_token_metadata_response
        (nat %token_id)
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
  `get_token_metadata_response` which is consequently passed to it.

- The smallest amount of tokens which may be transferred, burned or
  minted is always 1.

- `decimals` describe the number of digits to use after the decimal
  point when displaying the token amounts and must not affect
  transaction in any way.

- Since the contract supports only a single token type, `token_id` must be 0.
  It is passed because FA2 requires that.

**permissions_descriptor**

Types
```
self_transfer_policy =
  | Self_transfer_permitted
  | Self_transfer_denied

operator_transfer_policy =
  | Operator_transfer_permitted
  | Operator_transfer_denied

owner_transfer_policty =
  | Owner_no_op
  | Optional_owner_hook
  | Required_owner_hook

custom_permission_policy =
  ( string         :tag
  , option address :config_api
  )

permissions_descriptor_param
  ( self_transfer_policy            :self
  , operator_transfer_policy        :receiver
  , owner_transfer_policty          :operator
  , owner_transfer_policty          :sender
  , option custom_permission_policy :custom
  )

permissions_descriptor = contract permissions_descriptor_param
```

Parameter (in Michelson)
```
(contract %permissions_descriptor
  (pair
    (or %self
      (unit %self_transfer_permitted)
      (unit %self_transfer_denied)
    )
    (pair
      (or %operator
        (unit %operator_transfer_permitted)
        (unit %operator_transfer_denied)
      )
      (pair
        (or %receiver
          (unit %owner_no_op)
          (or
            (unit %optional_owner_hook)
            (unit %required_owner_hook)
        ))
        (pair
          (or %sender
            (unit %owner_no_op)
            (or
              (unit %optional_owner_hook)
              (unit %required_owner_hook)
          ))
        (option %custom
          (pair
            (string %tag)
            (option %config_api address)
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

**update_operators**

Types
```
token_id = nat

operator_tokens =
  | All_tokens
  | Some_tokens (set token_id)

operator_param =
  ( address         :owner
  , address         :operator
  , operator_tokens :tokens
  )

update_operator_param =
  | Add_operator    operator_param
  | Remove_operator operator_param

update_operators = list update_operator_param
```

Parameter (in Michelson)
```
(list %update_operators
  (or
    (pair %add_operator
      (address %owner)
      (pair
        (address %operator)
        (or %tokens
          (unit %all_tokens)
          (set %some_tokens nat)
        )
    ))
    (pair %remove_operator
      (address %owner)
      (pair
        (address %operator)
        (or %tokens
          (unit %all_tokens)
          (set %some_tokens nat)
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

- Contract must not be paused.

- Since the contract supports only a single token type, `token_id` must be 0.
  It is passed because FA2 requires that.

**is_operator**

Types
```
token_id = nat

operator_tokens =
  | All_tokens
  | Some_tokens (set token_id)

operator_param =
  ( address         :owner
  , address         :operator
  , operator_tokens :tokens
  )

is_operator_response =
  ( operator_param :operator
  , bool           :is_operator
  )

is_operator_param =
  ( operator_param                :operator
  , contract is_operator_response :callback
  )

is_operator = is_operator_param
```

Parameter (in Michelson):
```
(pair %is_operator
  (pair %operator
    (address %owner)
    (pair
      (address %operator)
      (or %tokens
        (unit %all_tokens)
        (set %some_tokens nat)
      )
  ))
  (contract %callback
    (pair
      (pair %operator
        (address %owner)
        (pair
          (address %operator)
          (or %tokens
            (unit %all_tokens)
            (set %some_tokens nat)
          )
      ))
      (bool %is_operator)
    )
  )
)
```

- Inspect if an address is an operator for the specified owner and
  token types.

- If the adddress is not an operator for at least one
  requested token type, then the result is `false`.

- It's possible to make this query for some specific tokens, or to
  all tokens.

- Since the contract supports only a single token type, `token_id` must be 0.
  It is passed because FA2 requires that.

## Custom (non-FA2) token functions

Functions for the stablecoin token implementation not present in FA2, but related to token transfers.
They do not have `token_id` argument because it is redundant (always must be 0) and not necessary (since they are not part of FA2).

### Pausing

**pause**

Parameter (in Michelson): `unit`.

- Pauses transferring, burning and minting operations so
  that they cannot be performed. All other operations remain
  unaffected.

- Cannot be called multiple times.

- Sender must be a pauser.

**unpause**

Parameter (in Michelson): `unit`.

- Unpauses the contract so that transferring, burning and minting
  operations can be performed by users with corresponding roles.

- Cannot be called multiple times.

- Sender must be a pauser.

**get_paused**

Types
```
get_paused_param =
  ( unit :unit
  , contract bool :callback
  )

get_paused = get_paused_param
```

Parameter (in Michelson)
```
(pair %get_total_paused
  (unit %dummy)
  (contract %callback bool)
)
```

- Returns token pause status.

### Issuing and destroying tokens

**configure_minter**

Types
```
configure_minter =
  ( address :minter
  , nat     :minting_allowance
  )
```

Parameter (in Michelson)
```
(pair %configure_minter
  (address %minter)
  (nat %minting_allowance)
)
```

- Adds `minter` to the global minter list to allow him to mint tokens.

- Sets the specified minting allowance for this minter.

- Sender must be master minter.

**remove_minter**

Parameter (in Michelson): `address :minter`.

- Removes minter from the global minter list and sets its minting
  allowance to 0. Once minter is removed it will no longer be able to
  mint or burn tokens.

- Sender must be master minter.

**get_minting_allowance**

Types
```
get_minting_allowance_response =
  ( address :owner
  , nat :allowance
  )

get_minting_allowance_param =
  ( list address :requests
  , contract (list get_minting_allowance_response) :callback
  )

get_minting_allowance = get_minting_allowance_param
```

Parameter (in Michelson):
```
(pair %get_minting_allowance
  (list %requests address)
  (contract %callback
    (list
      (pair
        (address %owner)
        (nat %allowance)
      )
    )
  )
)
```

- Returns current minting allowance for each address in a list.

**mint**

Types
```
mint_param
  ( address :recipient
  , nat     :value
  )

mint = list mint_param
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

- Produces the given amounts of tokens to the wallets associated with the
  given addresses.

- Each minting must happen atomically, so if one of them fails, then the whole
  operation must fail.

- The operation must follow permission policies described above.

- Sender must be minter.

- The total amount of minted coins must not exceed the current
  minting allowance specified for each minter individually.

- Minting allowance will decrease by the amount of tokens minted
  and increase the balance of receiver and total minted value.

- Contract must not be paused.

**burn**

Types
```
burn_param = nat :amount

burn = list burn_param
```

Parameter (in Michelson):
```
(list %burn address)
```

- Decreases balances for senders and the total supply of tokens by the given amount.

- Each burning operation must happen atomically, so if one of them fails,
  then the whole operation must fail.

- The operation must follow permission policies described above.

- Sender must be a minter and must have a sufficient amount of
  funds to be destroyed.

- A minter can only burn tokens which it owns.

- A minter with 0 minting allowance is allowed to burn tokens.

- Burning tokens will not increase the mintingAllowance of the address doing the burning.

- Contract must not be paused.

## Role reassigning functions

### Owner

**transfer_ownership**

Parameter (in Michelson): `address`.

- Set token owner to a new address.

- Sender must be current token owner.

- The current owner retains his priveleges up until
  `accept_ownership` is called.

- Can be called multiple times, each call replaces pending owner with
  the new one. Note, that if proposed owner is the same as the current
  one, then the pending owner is simply invalidated.

**get_owner**

Types
```
get_owner = (contract address :callback)
```

Parameter (in Michelson):
```
(pair %get_owner
  (contract %callback address)
)
```

- Returns current token owner address.

**accept_ownership**

Types
```
accept_ownership = unit
```

Parameter (in Michelson):
```
(unit %accept_ownership)
```

- Accept ownership privileges.

- Sender must be a pending owner.

### Master Minter

**change_master_minter**

Parameter (in Michelson): `address`.

- Set master minter to a new address.

- Sender must be token owner.

**get_master_minter**

Types
```
get_master_minter = (contract address :callback)
```

Parameter (in Michelson):
```
(pair %get_master_minter
  (contract %callback address)
)
```

- Return current master minter address.

### Pauser

**change_pauser**

Parameter (in Michelson): `address`.

- Set pauser to a new address.

- Sender must be token owner.

**get_pauser**

Types
```
get_pauser = (contract address :callback)
```

Parameter (in Michelson):
```
(pair %get_pauser
  (contract %callback address)
)
```

- Return current pauser address.
