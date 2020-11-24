<!--
SPDX-FileCopyrightText: 2020 TQ Tezos
SPDX-License-Identifier: MIT
-->

# Overview

These specifications were assembled with the following references:

- [*CENTRE Fiat Token design*](https://github.com/centrehq/centre-tokens/blob/78d964a1a8d481ffd8152772d7a66e47df54b3db/doc/tokendesign.md) which is served as a reference specification to tezos stablecoin smart contract.

- Tezos Token Standard:
  [*FA2*][FA2]

- Contract Permit Interface:
  [*TZIP-17*][TZIP-17]

- Contract Metadata:
  [*TZIP-16*][TZIP-16]

- [*Michelson Contract Interfaces and Conventions*](https://gitlab.com/tzip/tzip/blob/ae2f1e7ebb3454d811a2bea3cd0698b0e64ccea5/proposals/tzip-4/tzip-4.md) TZIP which defines `view` and `void` type synonyms

# General Requirements

- The token contract must be FA2 compatible as to facilitate listing on
  exchanges and interaction with services which support FA2.

- The token contract must be TZIP-17 compliant.

- The token contract must store tokens of a single type.

- The storage of the contract must have annotations for all fields and must be documented to make its interpretation easy for users.

- The token contract should reject transfers with non-zero XTZ AMOUNT (when someone calls the contract with non-zero amount).
The reason is that there is no way to spend XTZ owned by the contract.

# State model

This chapter provides a high-level overview of the contract's state.
Note that the actual storage type is an implementation detail and is not specified here.

* The contract maintains a ledger of addresses and manages some special roles as described below.
* Token operations can be paused, so the contract also knows whether it is paused.
* Token operations can be guarded by the [`Transferlist` contract](#transferlist), so the contract also optionally stores an address of such contract.
If its value is `None`, no transferlist checks are performed.

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

# Permits

A user can pre-sign a parameter, allowing other users to call this contract on the user's behalf.

To do this:
  1. The user creates a permit hash and signs it using the method described [here](#creating-and-signing-a-permit-hash).
  1. The user submits the permit hash and signature by invoking the [`permit`](#permit) entrypoint.
      * Or, alternatively, gives the permit hash and signature to a different user and lets that user submit them.
  1. Now, any user can call the contract with the pre-signed parameter. The permit will be consumed upon use.

Note that not all parameters can be pre-signed.
In the [entrypoints section](#entrypoints) we mention which entrypoints support this.

Permits expire after a certain number of seconds have passed.
A permit's expiry is determined as follows:
  1. If a permit-level expiry has been set via the [`set_expiry`](#set_expiry) entrypoint, the permit will expire after these many seconds.
  1. Otherwise, if a user-level expiry has been set for this permit's issuer via the [`set_expiry`](#set_expiry) entrypoint, the permit will expire after these many seconds.
  1. Otherwise, the contract's default expiry will be used. This is set once, during contract origination, and can be queried via the [`GetDefaultExpiry`](#getdefaultexpiry) off-chain view.

If a user attempts to use an expired permit, the call must fail with `EXPIRED_PERMIT`.

## Creating and signing a permit hash

To create a permit hash, the user should construct a value of the same type as the contract's parameter, and then run `PACK` and `BLAKE2B`.

To sign the hash, the user should:

1. Obtain the contract's [current counter](#getcounter) value.
1. Create a balanced 4-tuple as follows (where `%contract_address` represents the stablecoin contract's address):
    ```
    pair
      (pair
        (address %contract_address)
        (chain_id %chain_id))
      (pair
        (nat %counter)
        (bytes %permit_hash))
    ```
1. `PACK` the tuple.
1. Sign it with their private key.

# Transferlist

Before describing the token contract's entrypoints we describe the `Transferlist` interface.
The syntax is: `entrypointName(argumentType)`.
Required `Transferlist` entrypoints:
* `assertTransfers(list (pair (address %from) (list %tos address)))`
  * Checks whether a transfer is permitted from one address to the other one.
  Fails if transfer is prohibited for any item from this list.
* `assertReceivers(list address)`
  * Fails if any address in the list is not whitelisted or is blacklisted.

The specific interface of transferlist contracts is described in [TZIP-15](https://gitlab.com/tzip/tzip/-/blob/c38f4c2dc9da883d01e85409d34d0015d6ad272a/proposals/tzip-15/tzip-15.md)

# Errors

In error scenarios the stablecoin contract fails with a string.
Here is a summary of all the strings used as error messages.
We start with standard FA2 errors which are part of the FA2 specification.

| Error                      | Description                                                                    |
|----------------------------|--------------------------------------------------------------------------------|
| `FA2_TOKEN_UNDEFINED`      | One of the specified `token_id`s is not defined (i.e. not zero)                |
| `FA2_INSUFFICIENT_BALANCE` | Cannot debit from a wallet because of insufficient amount of tokens            |
| `FA2_NOT_OPERATOR`         | A transfer was initiated by neither the token owner nor a permitted operator   |

The next group consists of the errors that are not part of the FA2 specification.

| Error                        | Description |
|------------------------------|-------------|
| `XTZ_RECEIVED`               | Contract received a non-zero amount of tokens and should not proceed any further                                               |
| `NOT_CONTRACT_OWNER`         | Authorized sender is not contract owner                                                                                        |
| `NOT_PENDING_OWNER`          | Authorized sender is not current contract pending owner                                                                        |
| `NO_PENDING_OWNER_SET`       | Thrown when trying to accept a transfer of ownership, but no transfer was initiated beforehand                                 |
| `NOT_PAUSER`                 | Authorized sender is not contract pauser                                                                                       |
| `NOT_MASTER_MINTER`          | Authorized sender is not master minter                                                                                         |
| `NOT_MINTER`                 | Sender is not registered as minter                                                                                             |
| `CONTRACT_PAUSED`            | Operation cannot be performed during contract pause                                                                            |
| `CONTRACT_NOT_PAUSED`        | Operation cannot be performed if the contract is not paused                                                                    |
| `NOT_TOKEN_OWNER`            | Trying to configure operators for a different wallet which sender does not own                                                 |
| `CURRENT_ALLOWANCE_REQUIRED` | In `configure_minter` the caller wrongly expects the address to be not a minter                                                |
| `ALLOWANCE_MISMATCH`         | In `configure_minter` both allowances are not `None`, but different                                                            |
| `ADDR_NOT_MINTER`            | An attempt is made to modify minter data of an address that's not a minter                                                     |
| `ALLOWANCE_EXCEEDED`         | Thrown when trying to mint tokens more than currently allowed for an address                                                   |
| `BAD_TRANSFERLIST`           | Given address is a not a smart contract complying with the transferlist interface                                              |
| `MINTER_LIMIT_REACHED`       | Cannot add new minter because the number of minters is already at the limit.                                                   |
| `MISSIGNED`                  | The signature used to create a permit was invalid.                                                                             |
| `EXPIRED_PERMIT`             | The sender tried to access an entrypoint for which a permit was found, but it was expired.                                     |
| `NOT_PERMIT_ISSUER`          | The sender tried to set the expiry on a permit that wasn't theirs and no permit was issued to allow this call.                 |
| `DUP_PERMIT`                 | The sender tried to issue a duplicate permit.                                                                                  |
| `EXPIRY_TOO_BIG`             | The `set_expiry` entrypoint was called with an expiry value that is too big.                                                   |

# Entrypoints

Full list:
* [`transfer`](#transfer)
* [`balance_of`](#balance_of)
* [`token_metadata_registry`](#token_metadata_registry)
* [`update_operators`](#update_operators)
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
* [`set_transferlist`](#set_transferlist)
* [`permit`](#permit)
* [`set_expiry`](#set_expiry)

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

transfer_destination =
  ( address :to_
  , token_id :token_id
  , nat :amount
  )

transfer_param =
  ( address :from_
  , list transfer_destination :txs
  )

transfer = list transfer_param
```

Parameter (in Michelson):
```
(list %transfer
  (pair
    (address %from_)
    (list %txs
      (pair
        (address %to_)
        (pair
          (nat %token_id)
          (nat %amount)
        )
      )
    )
  )
)
```

- This entrypoint MUST follow the FA2 requirements.

- Permission logic requirements specific to this contract are described in the ["FA2 Specifics"](#fa2-specifics) chapter.

- If transferlist contract is set, this entrypoint MUST call its `assertTransfers` entrypoint (either directly or from the transfer hook).

- Since the contract supports only a single token type, all `token_id` values MUST be 0.
  They are passed because FA2 requires that.

- Fails with `CONTRACT_PAUSED` if the contract is paused.

- A token owner can issue permits allowing others to transfer tokens from the token owner's account.

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

### **token_metadata_registry**

Types
```
token_metadata_registry is contract (address)
```

Parameter (in Michelson)
```
(contract %token_metadata_registry address)
```

- Return contract address that holds token metadata.

### **update_operators**

Types
```
token_id = nat

operator_param =
  ( address         :owner
  , address         :operator
  , token_id        :token_id
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
        (nat %token_id))
    )
    (pair %remove_operator
      (address %owner)
      (pair
        (address %operator)
        (nat %token_id))
    )
  )
)
```

- This entrypoint MUST follow the FA2 requirements.

- Permission logic requirements specific to this contract are described in the ["FA2 Specifics"](#fa2-specifics) chapter.
Specifically, each `owner` must be equal to `SENDER`, otherwise `NOT_TOKEN_OWNER` error occurs.

- Fails with `CONTRACT_PAUSED` if the contract is paused.

- A token owner can issue permits allowing others to update the token owner's operators.

## Custom (non-FA2) token functions

Functions for the stablecoin token implementation not present in FA2, but related to token transfers.
They do not have `token_id` argument because it is redundant (always must be 0) and not necessary (since they are not part of FA2).

### Pausing

#### **pause**

Parameter (in Michelson): `unit`.

- Pauses transferring, burning and minting operations so
  that they cannot be performed. All other operations remain
  unaffected.

- Fails with `CONTRACT_PAUSED` if the contract is paused.

- Fails with `NOT_PAUSER` if the sender is not a pauser.

- The pauser can issue permits allowing others to pause the contract.

#### **unpause**

Parameter (in Michelson): `unit`.

- Unpauses the contract so that transferring, burning and minting
  operations can be performed by users with corresponding roles.

- Fails with `CONTRACT_NOT_PAUSED` if the contract is not paused.

- Fails with `NOT_PAUSER` if the sender is not a pauser.

- The pauser can issue permits allowing others to unpause the contract.

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
  Currently we allow upto 12 minters.

- Sets the specified minting allowance (`new_minting_allowance`) for this minter.

- `current_minting_allowance` must be explicitly passed.
If it does not match the actual minting allowance, the transaction MUST fail.
It is done to prevent front-running attacks.
It MUST be set to `None` iff the minter is not in the list of minters.
We distinguish three error cases, each has a dedicated error message:
  + `ALLOWANCE_MISMATCH`: both provided and actual minting allowances are not None, but they are different.
  + `CURRENT_ALLOWANCE_REQUIRED`: the caller expects that the `minter` address is not a minter, but this address is already a minter.
  + `ADDR_NOT_MINTER`: the caller expects that the `minter` address is a minter, but it is not.

- Fails with `NOT_MASTER_MINTER` if the sender is not master minter.

- Fails with `CONTRACT_PAUSED` if the contract is paused.

- Fails with `MINTER_LIMIT_REACHED` if the current number of minters is at the limit
  and the request is trying to add a new minter.

- The master minter can issue permits allowing others to call this entrypoint.

#### **remove_minter**

Parameter (in Michelson): `address`.

- Removes minter from the minter list and sets its minting allowance to 0.
  Once minter is removed it will no longer be able to mint or burn tokens.

- Fails with `NOT_MASTER_MINTER` if the sender is not master minter.

- Fails with `ADDR_NOT_MINTER` if the argument is not an address of a minter.

- The master minter can issue permits allowing others to call this entrypoint.

#### **mint**

Types
```
mint_param
  ( address :to_
  , nat     :value
  )

mint = list mint_param
```

Parameter (in Michelson):
```
(list
  (pair
    (address %to_)
    (nat %value)
  )
)
```

- Produces the given amounts of tokens to the wallets associated with the
  given addresses.

- Each minting must happen atomically, so if one of them fails, then the whole
  operation must fail.

- Fails with `NOT_MINTER` if the sender is not a minter.

- The total amount of minted coins must not exceed the current
  minting allowance specified for each minter individually.
  Otherwise the operation fails with `ALLOWANCE_EXCEEDED`.

- Minting allowance will decrease by the amount of tokens minted
  and increase the balance of receiver and total minted value.

- If transferlist contract is set, this entrypoint MUST call its `assertReceivers` entrypoint (either directly or from the transfer hook) checking the minter and all receivers.

- Fails with `CONTRACT_PAUSED` if the contract is paused.

#### **burn**

Parameter (in Michelson): `list nat`.

- Decreases balance for sender.

- Each burning operation must happen atomically, so if one of them fails,
  then the whole operation must fail.

- The operation must follow permission policies described above.

- Fails with `NOT_MINTER` if the sender is not a minter.

- Fails with `FA2_INSUFFICIENT_BALANCE` if the sender does not have enough tokens to burn.

- A minter with 0 minting allowance is allowed to burn tokens.

- Burning tokens will not increase the mintingAllowance of the address doing the burning.

- If transferlist contract is set, this entrypoint MUST call its `assertReceivers` entrypoint (either directly or from the transfer hook) checking the minter (`SENDER`).

- Fails with `CONTRACT_PAUSED` if the contract is paused.

## Role reassigning functions

### **transfer_ownership**

Parameter (in Michelson): `address`.

- Initiate transfer of contract ownership to a new address.

- Fails with `NOT_CONTRACT_OWNER` if the sender is not the current contract owner.

- The current contract owner retains his priveleges up until
  `accept_ownership` is called.

- Can be called multiple times, each call replaces pending contract owner with
  the new one. Note, that if proposed contract owner is the same as the current
  one, then the pending contract owner is simply invalidated.

- The contract owner can issue permits allowing others to call this entrypoint.

### **accept_ownership**

Parameter: `unit`.

- Accept contract ownership privileges.

- Fails with `NOT_PENDING_OWNER` if the sender is not the current pending contract owner or `NO_PENDING_OWNER_SET` if there is no pending contract owner.

- The pending owner can issue permits allowing others to call this entrypoint.

### **change_master_minter**

Parameter (in Michelson): `address`.

- Set master minter to a new address.

- Fails with `NOT_CONTRACT_OWNER` if the sender is not the contract owner.

- The contract owner can issue permits allowing others to call this entrypoint.

### **change_pauser**

Parameter (in Michelson): `address`.

- Set pauser to a new address.

- Fails with `NOT_CONTRACT_OWNER` if the sender is not the contract owner.

- The contract owner can issue permits allowing others to call this entrypoint.

## Transferlist update

### **set_transferlist**

Parameter (in Michelson): `option address`.

- Set the stored (optional) transferlist address to the new one.

- If the address does not have any entrypoint listed in the [`Transferlist`](#transferlist) specification, this call MUST fail with `BAD_TRANSFERLIST`.

- Fails with `NOT_CONTRACT_OWNER` if the sender is not the contract owner.

- The contract owner can issue permits allowing others to call this entrypoint.

## Permits management

### **permit**

Parameter (in Michelson):
```
pair %permit
  key
  (pair
    signature
    (bytes %permit_hash))
```

- Creates a permit, allowing any user to call this contract with a pre-signed parameter on the signer's behalf.

- The parameter's `key` represents the public key of the user who signed the permit.

- The parameter's `bytes` represent the permit's hash (see ["Creating and signing a permit hash"](#creating-and-signing-a-permit-hash)).

- The parameter's `signature` represents the signature of the permit hash (see ["Creating and signing a permit hash"](#creating-and-signing-a-permit-hash)).

- The entrypoint checks that the permit hash was signed with the correct chain ID, the contract's address, and the [contract's current counter](#getcounter).
  + If successful, the contract's counter is incremented by 1. This ensures that a permit's signature is valid for one use only.

- Fails with a `pair string bytes` if the signature is invalid (e.g. the wrong counter was used, or it was signed with a private key that does not correspond to the public key in the entrypoint's parameter).
  + the `string` is `"MISSIGNED"`.
  + the `bytes` is [the packed 4-tuple](#creating-and-signing-a-permit-hash) whose signature did not match the expected signature.

- Fails with `"DUP_PERMIT"` if the same permit hash is re-uploaded before it expires.

- When a permit is issued, the issuer's expired permits are deleted.

### **set_expiry**

Parameter (in Michelson):
```

pair
  (address %issuer)
  (pair
    (nat %expiry)
    (option %permit_hash bytes))
```

- Only the issuer of permit can set expiry.

  + Alternatively, they can issue another permit allowing other users to set expiry on their permits.
  + Otherwise, this entrypoint fails with `NOT_PERMIT_ISSUER`.

- If the pair's second member is a `Some`, then a permit-level expiry will be set for the given permit hash and issuer.

- Otherwise, a user-level expiry will be set for the sender. This will affect all permits signed by the sender, for which no permit-level expiry has been set.

- There is a maximum value of expiry that the contract will accept and is currently set to 31557600000.

- Setting a zero value to expiry revokes the permit.

# Off-chain views

Stablecoin provides the following off-chain views (as defined by [TZIP-16]):

* [`GetDefaultExpiry`](#getdefaultexpiry)
* [`GetCounter`](#getcounter)

### **GetDefaultExpiry**

```
GetDefaultExpiry: unit → nat
```

Retrieves the contract's default expiry (in seconds).
This expiry affects all permits for which no permit-level expiry has been set and for whose issuer no user-level expiry has been set.

### **GetCounter**

```
GetCounter: unit → nat
```

Retrieves the contract's current counter, and is used to [sign and verify permits](#creating-and-signing-a-permit-hash).

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
At this stage of development we make the following decisions:
1. We will implement only monolithic contract for two reasons:
  + We were told that the "transfer hook" approach would likely run out of gas.
  + Communication between contracts in Tezos is currently complicated and is hard to do right without any security vulnerabilities.
2. We will implement a single permission policy (as part of the token contract or as a separate transfer_hook – that depends on the previous point).
 + Self transfer is permitted as well as operator transfer.
 + Owner transfer policy for both sender and receiver is `Owner_no_hook`.
 + No custom permission policy.
3. We will not implement any `owner_hook`.
The contract is usable without them because we stick to `Owner_no_hook`.

Regarding `update_operators` we will stick to the following logic:
1. In each `update_operator` item `owner` MUST be equal to `SENDER`.
2. Each address can have arbitrary number of operators.

Note that operator relation is not transitive.

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
Our project is different: whitelisting and blacklisting are offloaded to a separate [`Transferlist` contract](#transferlist).

[FA2]: https://gitlab.com/tzip/tzip/-/blob/b916f32718234b7c4016f46e00327d66702511a2/proposals/tzip-12/tzip-12.md
[TZIP-17]: https://gitlab.com/tzip/tzip/-/blob/eb1da57684599a266334a73babd7ba82dbbbce66/proposals/tzip-17/tzip-17.md
[TZIP-16]: https://gitlab.com/tzip/tzip/-/blob/eb1da57684599a266334a73babd7ba82dbbbce66/proposals/tzip-16/tzip-16.md
[view]: https://gitlab.com/tzip/tzip/-/blob/master/proposals/tzip-4/tzip-4.md#view-entrypoints
