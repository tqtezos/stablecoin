// SPDX-FileCopyrightText: 2020 TQ Tezos
// SPDX-License-Identifier: MIT

(*
 * Spender allowances library for FA1.2 contracts
 *)

function get_allowance
  ( const owner: address
  ; const spender: address
  ; const spender_allowances: spender_allowances
  ) : nat is
  case Big_map.find_opt((owner, spender), spender_allowances) of
  | None -> 0n
  | Some(allowance) -> allowance
  end

(*
 * Unconditionally set a spender's allowance to the given amount.
 * NOTE: for internal use only.
 *)
function set_allowance
  ( const owner: address
  ; const spender: address
  ; const allowance: nat
  ; const spender_allowances: spender_allowances
  ): spender_allowances is
  // Note: this is an optimization. When an allowance is set to `0`, we can
  // instead remove the entry from the big_map.
  if allowance = 0n then
    Big_map.update((owner, spender), (None: option(nat)), spender_allowances)
  else
    Big_map.update((owner, spender), Some(allowance), spender_allowances)


type deduct_from_allowance_result is
  | Deducted of spender_allowances
  | NotEnoughAllowance of
      ( record
          required: nat;
          present: nat
        end
      )

(*
 * If the owner is trying to spend tokens from their own account, no allowances are deducted.
 * Otherwise, this function tries to deduct the required amount of tokens
 * from the spender's allowance.
 * If the spender does not have enough allowance, this function returns `NotEnoughAllowance`.
 *)
function deduct_from_allowance
  ( const owner: address
  ; const spender: address
  ; const value: nat
  ; const spender_allowances: spender_allowances
  ): deduct_from_allowance_result is
  if spender = owner then
    Deducted(spender_allowances)
  else block {
    const current_allowance: nat = get_allowance(owner, spender, spender_allowances)
  } with
    case is_nat(current_allowance - value) of
    | None ->
        NotEnoughAllowance(record [required = value; present = current_allowance])
    | Some(new_allowance) ->
        Deducted(set_allowance(owner, spender, new_allowance, spender_allowances))
    end

(*
 * Allows `spender` to spend the given amount of tokens from the `owner`'s account.
 *
 * If `spender` already had an allowance, it is overriden by the new allowance.
 *
 * As per the FA1.2 spec, attempts to change a spender's allowance from a
 * non-zero to a non-zero value fail with "UnsafeAllowanceChange".
 *
 *)
function approve_allowance
  ( const owner: address
  ; const spender: address
  ; const new_allowance: nat
  ; const spender_allowances: spender_allowances
  ) : spender_allowances is block
{ const current_allowance: nat = get_allowance(owner, spender, spender_allowances)

  // We're using Embedded Michelson here to get around the limitation that,
  // currently, Ligo's `failwith` only supports strings and numeric types,
  // but not, for example, pairs.
  // https://ligolang.org/docs/advanced/embedded-michelson/
  //
  // Once this MR is merged, we should be able to use Ligo's `failwith`:
  // https://gitlab.com/ligolang/ligo/-/issues/193
; const failwith_ : (string * nat -> spender_allowances) =
  [%Michelson ({| { FAILWITH } |} : string * nat -> spender_allowances)]

} with
  if current_allowance =/= 0n and new_allowance =/= 0n then
    (failwith_(("UnsafeAllowanceChange", current_allowance)) : spender_allowances)
  else
    set_allowance(owner, spender, new_allowance, spender_allowances)
