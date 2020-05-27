<!--
SPDX-FileCopyrightText: 2020 tqtezos
SPDX-License-Identifier: MIT
-->

# Overview

These specifications were assembled with the following references:

- [*CENTRE Fiat Token design*](https://github.com/centrehq/centre-tokens/blob/78d964a1a8d481ffd8152772d7a66e47df54b3db/doc/tokendesign.md) which is served as a reference specification to tezos stablecoin smart contract.

- Tezos Token Standard:
  [*FA2*][FA2]

- [*Michelson Contract Interfaces and Conventions*](https://gitlab.com/tzip/tzip/blob/ae2f1e7ebb3454d811a2bea3cd0698b0e64ccea5/proposals/tzip-4/tzip-4.md) TZIP which defines `view` and `void` type synonyms

# General Requirements

- The token contract must be FA2 compatible as to facilitate listing on
  exchanges and interaction with services which support FA2.

- The token contract must store tokens of a single type.

- The storage of the contract must have annotations for all fields and must be documented to make its interpretation easy for users.

- The token contract should reject transfers with non-zero XTZ AMOUNT (when someone calls the contract with non-zero amount).
The reason is that there is no way to spend XTZ owned by the contract.

# State model

This chapter provides a high-level overview of the contract's state.
Note that the actual storage type is implementation detail and is not specified here.

* The contract maintains a ledger of addresses and manages some special roles as described below.
* Token operations can be paused, so the contract also knows whether it is paused.
* Token operations can be guarded by the [`Safelist` contract](#safelist), so the contract also optionally stores an address of such contract.
If its value is `None`, no safelist checks are performed.

## Roles

The token supports a number of "global" user roles as is described in
CENTRE Fiat Token specification. These roles apply to the whole contract
(hence "global"):

* **contract owner**
  - Can assign and re-assign any role of the token.
  - There always must be exactly one contract owner.

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

* **pauser**
  - Can pause transferring, burning and minting operations.
    During the pause, these operations cannot be performed
    and fail with an error if the user decides to try them.
  - There always must be exactly one pauser.

Additionally, the contract inherits the **operator** role from FA2.
This role is "local" to a particular address.
Each address can have any number of operators and be an operator of any number of addresses.

## Ledger

Every address that is stored in ledger is associated with its current balance.
Additionally, each minter is associated with its current minting allowance.

# Safelist

Before describing the token contract's entrypoints we describe the `Safelist` interface.
The syntax is: `entrypointName(argumentType)`.
Required `Safelist` entrypoints:
* `assertTransfers(list(pair (address :from) (address :to)))`
  * Checks whether a transfer is permitted from one address to the other one.

    TODO: currently implementation doesn't match this interface because of the recent changes to FA2 interface
    and we are discussing what's the best way to proceed

  Fails if transfer is prohibited for any item from this list.
* `assertReceiver(address)`
  * Fails if address is not whitelisted or it is blacklisted.
* `assertReceivers(list address)`
  * Fails if any address in the list is not whitelisted or is blacklisted.

# Errors

| Error                 | Description |
|-----------------------|-------------|
| NOT_OWNER             | Authorized sender is not contract owner                                                                                        |
| NOT_PENDING_OWNER     | Authorized sender is not current contract pending owner                                                                        |
| NO_PENDING_OWNER_SET  | Throws when trying to authorize as pending owner whilst is not set for a contract                                              |
| NOT_PAUSER            | Authorized sender is not contract pauser                                                                                       |
| NOT_MASTER_MINTER     | Authorized sender is not master minter                                                                                         |
| NOT_MINTER            | Sender is not registered as minter                                                                                             |
| CONTRACT_PAUSED       | Operation cannot be performed during contract pause                                                                            |
| CONTRACT_NOT_PAUSED   | Operation cannot be peformed if the contract is not paused                                                                     |
| INSUFFICIENT_BALANCE  | Cannot debit from a wallet because of excessive amount of tokens                                                               |
| NOT_IN_LEDGER         | Wallet is not present in ledger                                                                                                |
| TX_DENIED             | Operator transfer is permitted                                                                                                 |
| NOT_OPERATOR          | Trying to transfer tokens when sender is not an operator                                                                       |
| NOT_OWNER             | Trying to configure operators for a different wallet which sender does not own                                                 |
| NO_ALLOWANCE_EXPECTED | Throws when trying to configure minter with Nothing value in parameter provided                                                |
| ALLOWANCE_MISMSATCH   | Throws when expected allowance in configure minter parameter does not match the actual one                                     |
| NOT_MINTER            | Throws when trying to configure minter but expected address is not one                                                         |
| ALLOWANCE_EXCEEDED    | Throws when trying to mint tokens more than currently allowed for an address                                                   |
| NEGATIVE_TOTAL_SUPPLY | Throws if contract is faced negative total supply. This may appear during the internal error and is not expected to be thrown |

# Entrypoints

Full list:
* [`transfer`](#transfer)
* [`balance_of`](#balance_of)
* [`total_supply`](#total_supply)
* [`token_metadata`](#token_metadata)
* [`permissions_descriptor`](#permissions_descriptor)
* [`update_operators`](#update_operators)
* [`is_operator`](#is_operator)
* [`pause`](#pause)
* [`unpause`](#unpause)
* [`configure_minter`](#configure_minter)
* [`remove_minter`](#remove_minter)
* [`mint`](#mint)
* [`burn`](#burn)
* [`transfer_ownership`](#transfer_ownership)
* [`accept_ownership`](#accept_ownership)
* [`change_master_minter`](#change_master_minter)
* [`change_pauser`](#change_pauser)
* [`set_safelist`](#set_safelist)

Format:
```
**entrypoint_name**

<optional pseudocode description of the parameter type>
Parameter (in Michelson): X

<description>
```

* Top-level contract parameter type MUST have all entrypoints listed below.
* Each entrypoint MUST be callable using the standard entrypoints machinery of Michelson by specifying **entrypoint_name** and a value of the type `X` (its argument).
* The previous bullet point implies that each `X` must have a field annotations with the corresponding entrypoint name.
In the definitions below it may be omitted, but it is still implied.

Pseudocode is semi-formally defined as a list of assignments where the last assignment has `entrypoint_name` on the left and its argument type (that maps to `X`) on the right.

Note: pseudocode is provided only for readability.
If Michelson type contradics what's written in pseudocode, the Michelson defition takes precedence.

## Standard FA2 Token Functions

Functions for the stablecoin token implementation which are common to the
[*FA2 Tezos Token Standard*][FA2].
Some entrypoints (e. g. getters) follow an [*event loop*](https://en.wikipedia.org/wiki/Event_loop)
pattern meaning that their arguments have an additional callback call
describing what needs to be done with its result.

### **transfer**

Types
```
token_id = nat

transfer_param =
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

- This entrypoint MUST follow the FA2 requirements.

- Permission logic requirements specific to this contract are described in the ["FA2 Specifics"](#fa2-specifics) chapter.

- If safelist contract is set, this entrypoint MUST call its `assertTransfers` entrypoint (either directly or from the transfer hook).

- Since the contract supports only a single token type, all `token_id` values MUST be 0.
  They are passed because FA2 requires that.

- Contract must not be paused.

### **balance_of**

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

balance_of =
  ( list balance_of_request :requests
  , contract (list balance_of_response) :callback
  )
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

- This entrypoint MUST follow the FA2 requirements.

- Since the contract supports only a single token type, all `token_id` values MUST be 0.
  They are passed because FA2 requires that.

### **total_supply**

Types
```
token_id = nat

total_supply_response =
  ( token_id :token_id
  , nat :total_supply
  )

total_supply =
  ( list token_id :token_ids
  , contract (list total_supply_response) :callback
  )
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

- This entrypoint MUST follow the FA2 requirements.

- Since the contract supports only a single token type, all `token_id` values MUST be 0.
  They are passed because FA2 requires that.

### **token_metadata**

Types
```
token_id = nat

token_metadata_response =
  ( token_id             :token_id
  , string               :symbol
  , string               :name
  , nat                  :decimals
  , map (string, string) :extras
  )

token_metadata =
  ( list token_id                           :token_ids
  , contract (list token_metadata_response) :callback
  )
```

Parameter (in Michelson)
```
(pair %token_metadata
  (list %token_ids nat)
  (contract %callback
    (list
      (pair %token_metadata_response
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

- This entrypoint MUST follow the FA2 requirements.

- Since the contract supports only a single token type, all `token_id` values MUST be 0.
  They are passed because FA2 requires that.

- Token metadata for 0 `token_id` is constant and should be hardcoded in contract code or put into storage during origination.

### **permissions_descriptor**

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

- This entrypoint MUST follow the FA2 requirements.

- Permission logic requirements specific to this contract are described in the ["FA2 Specifics"](#fa2-specifics) chapter.

- Since the contract supports only a single token type, all `token_id` values MUST be 0.
  They are passed because FA2 requires that.

### **update_operators**

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

- This entrypoint MUST follow the FA2 requirements.

- Permission logic requirements specific to this contract are described in the ["FA2 Specifics"](#fa2-specifics) chapter.
Each `owner` must be equal to `SENDER`.

- Since the contract supports only a single token type, all `token_id` values MUST be 0.
  They are passed because FA2 requires that.
  So there are three possible valid values of `operator_tokens` type:
  + `All_tokens`
  + `Some_tokens ({0})` – equivalent to `All_tokens`.
  + `Some_tokens ({})` – no-op.

- Contract must not be paused.

### **is_operator**

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

is_operator =
  ( operator_param                :operator
  , contract is_operator_response :callback
  )
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

- This entrypoint MUST follow the FA2 requirements.

- Since the contract supports only a single token type, all `token_id` values MUST be 0.
  They are passed because FA2 requires that.
  So there are three possible valid values of `operator_tokens` type:
  + `All_tokens`
  + `Some_tokens ({0})` – equivalent to `All_tokens`.
  + `Some_tokens ({})` – for this value the result is always `True`.

## Custom (non-FA2) token functions

Functions for the stablecoin token implementation not present in FA2, but related to token transfers.
They do not have `token_id` argument because it is redundant (always must be 0) and not necessary (since they are not part of FA2).

### Pausing

#### **pause**

Parameter (in Michelson): `unit`.

- Pauses transferring, burning and minting operations so
  that they cannot be performed. All other operations remain
  unaffected.

- Cannot be called multiple times.

- Sender must be a pauser.

#### **unpause**

Parameter (in Michelson): `unit`.

- Unpauses the contract so that transferring, burning and minting
  operations can be performed by users with corresponding roles.

- Cannot be called multiple times.

- Sender must be a pauser.

### Issuing and destroying tokens

#### **configure_minter**

Types
```
configure_minter =
  ( address :minter
  , (option nat) :current_minting_allowance
  , nat :new_minting_allowance
  )
```

Parameter (in Michelson)
```
(pair
  (address %minter)
  (pair ((option nat) %current_minting_allowance) (nat %new_minting_allowance))
)
```

- Adds `minter` to the minter list to allow him to mint tokens (if `minter` is not in the list already).

- Sets the specified minting allowance (`new_minting_allowance`) for this minter.

- `current_minting_allowance` must be explicitly passed.
If it does not match the actual minting allowance, the transaction MUST fail.
It is done to prevent front-running attacks.
It MUST be set to `None` iff the minter is not in the list of minters.

- Sender must be master minter.

- Contract must not be paused.

#### **remove_minter**

Parameter (in Michelson): `address`.

- Removes minter from the minter list and sets its minting allowance to 0.
  Once minter is removed it will no longer be able to mint or burn tokens.

- Sender must be master minter.

#### **mint**

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
(list
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

- Sender must be minter.

- The total amount of minted coins must not exceed the current
  minting allowance specified for each minter individually.

- Minting allowance will decrease by the amount of tokens minted
  and increase the balance of receiver and total minted value.

- If safelist contract is set, this entrypoint MUST call its `assertReceivers` entrypoint (either directly or from the transfer hook) checking the minter and all receivers.

- Contract must not be paused.

#### **burn**

Parameter (in Michelson): `list nat`.

- Decreases balance for sender and the total supply of tokens by the sum of given amounts.

- Each burning operation must happen atomically, so if one of them fails,
  then the whole operation must fail.

- The operation must follow permission policies described above.

- Sender must be a minter and must have a sufficient amount of
  funds to be destroyed.

- A minter with 0 minting allowance is allowed to burn tokens.

- Burning tokens will not increase the mintingAllowance of the address doing the burning.

- If safelist contract is set, this entrypoint MUST call its `assertReceiver` entrypoint (either directly or from the transfer hook) checking the minter (`SENDER`).

- Contract must not be paused.

## Role reassigning functions

### **transfer_ownership**

Parameter (in Michelson): `address`.

- Initiate transfer of contract ownership to a new address.

- Sender must be current contract owner.

- The current contract owner retains his priveleges up until
  `accept_ownership` is called.

- Can be called multiple times, each call replaces pending contract owner with
  the new one. Note, that if proposed contract owner is the same as the current
  one, then the pending contract owner is simply invalidated.

### **accept_ownership**

Parameter: `unit`.

- Accept contract ownership privileges.

- Sender must be a pending contract owner.

### **change_master_minter**

Parameter (in Michelson): `address`.

- Set master minter to a new address.

- Sender must be contract owner.

### **change_pauser**

Parameter (in Michelson): `address`.

- Set pauser to a new address.

- Sender must be contract owner.

## Safelist update

### **set_safelist**

Parameter (in Michelson): `option address`.

- Set the stored (optional) safelist address to the new one.

- If the address does not have any entrypoint listed in the [`Safelist`](#safelist) specification, this call MUST fail.

- Sender must be contract owner.

# Rationale and design considerations

Design decisions outside of FA2 scope:
1. For three central roles (contract owner, master minter and pauser) we require that exactly one address has each of these roles.
One can use a multisig contract if they want some role to be managed by multiple parties.
The only role that can be assigned to multiple parties is minter because different minters can have different minting allowances and it can't be captured by a general multisig contract.
2. One address can have multiple roles because in practice people may not need this granularity and just let one address do all administrative operations.
3. Contract owner role can be transferred only in two steps and requires the new contract owner to accept new privileges.
That's done to avoid accidental loss of control when this role is transferred to a wrong address.
Other roles are transferred in one call because wrong transfer can be fixed by the contract owner.
4. There are no getter entrypoints beside those required by FA2.
At this point it seems unlikely that any other contract may need to call this token contract on chain.
And for off-chain access we require storage to be annotated and well-documented so that users can read data from it directly.

In the following subsection we discuss and present design decisions regarding FA2 specifics.

## FA2 Specifics

FA2 provides a framework for defining permission policies, but does not require any particular policy.
Moreover, it suggests an approach where permission logic is defined in a different contract and the token contract can dynamically change the policy.
For the Stablecoin project we need to answer several questions:

1. FA2 recommends the "transfer hook" design pattern, as opposed to implementing a monolithic contract.
Are there any requirements whether the stablecoin contract should be monolithic or follow this pattern (i. e. have `set_transfer_hook`)?
2. For a monolithic contract: which permission policy should we implement?
Should the whole policy or at least some part of it be hardcoded?
E. g. can we assume that self transfer will always be permitted and there will be no custom policy?
What about other policies?
3. For a modular (non-monolithic) contract: what exactly should be implemented by us and put into the repository?
Should we implement any particular `transfer_hook`?
If we should, the questions from (2) apply here.
4. Should we implement any `owner_hook`?
5. The FA2 standard does not specify who is permitted to update operators on behalf of the token owner.
Who should be allowed to do it in this project?

Overall, the response from TQ was that there are no strict requirements and we should pick the best approaches from the business logic.
At the early development stage we propose the following:
1. We will implement both options (starting from the monolithic one because it's simpler), measure gas costs and then will decide what is better.
The "transfer hook" approach is recommended, but may be infeasible due to gas costs.
2. We will implement a single permission policy (as part of the token contract or as a separate transfer_hook – that depends on the previous point).
 + Self transfer is permitted as well as operator transfer.
 + Owner transfer policy for both sender and receiver is `Optional_owner_hook`.
 + No custom permission policy.
3. We will not implement any `owner_hook`.
The contract is usable without them because we stick to `Optional_owner_hook`.

Regarding `update_operators` we will stick to the following logic:
1. In each `update_operator` item `owner` MUST be equal to `SENDER`.
2. Each address can have arbitrary number of operators.

Note that operator relation is not transitive.

One more uncertainty is related to the `token_metadata` entrypoint: whether metadata is constant or can be changed during contract's lifetime.
We assume that token metadata stays constant forever.

Note: these decisions may change as the project moves forward.

## Differences with CENTRE

The stablecoin contract is based on the CENTRE fiat token design, but diverges from it in several aspects:
1. CENTRE token implements the ERC-20 interface.
Tezos version of it is FA1.2.
The stablecoin contract does not implement the FA1.2 interface, it implements the FA2 interface.
2. Since the names in FA2 are snake\_cased, the names in the stablecoin contract also snake\_cased.
3. FA2 uses the word "owner" to refer to the address that owns some tokens.
At the same time, CENTRE calls the central entity who can manage roles the "owner".
It leads to ambiguity, but the meaning of the word "owner" should be clear from the context.
To eliminate this ambiguity we use two terms:
  * "contract owner" is the central entity that owns the whole contract;
  * "token owner" is the entity that holds some tokens.
4. `mint` and `burn` entrypoints take lists of pairs/amounts respectively.
They follow FA2 style where most entrypoints operate on lists.
In CENTRE they take a single item (a pair or an amount).
5. `configure_minter` takes an additional argument – current minting allowance – to prevent front-running attacks.
6. CENTRE does not have `update_operator`, the closest analog is `approve`, but it does not explicitly say whether `approve` is paused by `pause`.
In this contract `update_operator` IS paused.
7. CENTRE defines the `blacklister` role and blacklisting is part of the CENTRE Fiat Token contract.
Our project is different: whitelisting and blacklisting are offloaded to a separate [`Safelist` contract](#safelist).

[FA2]: https://gitlab.com/tzip/tzip/-/blob/131b46dd89675bf030489ded9b0b3f5834b70eb6/proposals/tzip-12/tzip-12.md
