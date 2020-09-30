// SPDX-FileCopyrightText: 2020 TQ Tezos
// SPDX-License-Identifier: MIT

(*
 * Operator library for stablecoin smart-contract
 *)

#include "hooks.ligo"
#include "permit.ligo"

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
{ validate_token_type(param.1.1)
; const operator_key : (owner * operator) = (param.0, param.1.0)
} with Big_map.update (operator_key, Some (unit), operators)

(*
 * Remove operator from the given storage
 *)
function remove_operator
  ( const param     : operator_param
  ; const operators : operators
  ) : operators is block
{ validate_token_type(param.1.1)
; const operator_key : (owner * operator) = (param.0, param.1.0)
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
