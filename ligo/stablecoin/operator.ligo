// SPDX-FileCopyrightText: 2020 TQ Tezos
// SPDX-License-Identifier: MIT

(*
 * Operator library for stablecoin smart-contract
 *)

#include "hooks.ligo"
#include "permit.ligo"

(*
 * Returns a callback of `Is_operator` entrypoint call
 *)
function is_operator
  ( const param     : is_operator_params
  ; const operators : operators
  ) : operation is block
{ const operator_param : operator_param_ =
    Layout.convert_from_right_comb ((param.0 : operator_param))
; const operator_key : (owner * operator) =
    (operator_param.owner, operator_param.operator)
; const is_present : bool = Big_map.mem (operator_key, operators)
; const response : is_operator_response = (param.0, is_present)
} with Tezos.transaction (response, 0mutez, param.1)

(*
 * Validates operators for the given transfer batch
 * and operator storage
 *)
function is_approved_operator
  ( const transfer_param         : transfer_param
  ; const operators              : operators
  ) : bool is block
{ const operator : address = Tezos.sender
; const owner : address = transfer_param.0
} with if owner = operator or Big_map.mem ((owner, operator), operators)
  then True else False

(*
 * Add operator from the given parameter and operator storage
 *)
function add_operator
  ( const param     : operator_param
  ; const operators : operators
  ) : operators is block
{ const operator_key : (owner * operator) = (param.0, param.1)
} with Big_map.update (operator_key, Some (unit), operators)

(*
 * Remove operator from the given storage
 *)
function remove_operator
  ( const param     : operator_param
  ; const operators : operators
  ) : operators is block
{ const operator_key : (owner * operator) = (param.0, param.1)
} with Big_map.remove (operator_key, operators)

(*
 * Adds or removes operators from a provided list of instructions
 * accordingly for the given storage
 *)
function update_operators
  ( const params    : update_operator_params
  ; const operators : operators
  ) : operators is List.fold
    ( function
        ( const operators             : operators
        ; const update_operator_param : update_operator_param
        ) : operators is case update_operator_param of
          Add_operator    (param) -> add_operator    (param, operators)
        | Remove_operator (param) -> remove_operator (param, operators)
        end
    , params
    , operators
    )

(*
 * Get the address of the user whose operators will be changed.
 *)
function get_owner
  ( const param : update_operator_param
  ) : address is
  case param of
    Add_operator    (param) -> param.0
  | Remove_operator (param) -> param.0
  end

(*
 * Verify the sender of an `update_operators` action.
 *
 * A user can only modify their own operators.
 *
 * Therefore, we check that the sender is the owner whose operators we're trying to change,
 * or that the owner issued a permit allowing this call to go through.
 *)
function update_operators_sender_check
  ( const parameter : update_operator_params
  ; const store : storage
  ; const full_param : closed_parameter
  ) : storage is
    case parameter of
      nil -> store
    | first_param # rest ->
        block {
          // check the owner of the first operation.
          const owner : address = get_owner(first_param)
        ; const updated_store : storage = sender_check(owner, store, full_param, "NOT_TOKEN_OWNER")
          // check that all operations relate to the same owner.
        ; List.iter
            ( function (const op : update_operator_param): unit is
                if get_owner(op) =/= owner
                  then failwith ("NOT_TOKEN_OWNER")
                  else Unit
            , rest
            )
        } with updated_store
    end
