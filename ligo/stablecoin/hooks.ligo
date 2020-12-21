// SPDX-FileCopyrightText: 2020 TQ Tezos
// SPDX-License-Identifier: MIT

(*
 * Generic transfer hook implementation
 *
 * While implementing this framework we have faced some known
 * problems with ligo itself that need to be clarified here,
 * more specifically:
 *
 * 1. All builtins such as `Tezos.*` and `Layout.*` are allowed
 * to be called only on "top-level" and don't work with nested
 * (multiple) calls. From the descussions:
 *
 * ```
 * @sras > Some expressions like Tezos.self_address are only possible
 * to be used in a "top-level". It seemed to me that, according to
 * LIGO, a function is top-level, if it is only called from a single
 * location. So the Tezos.self_address can work from function func1
 * so long as func1 is only called from one place. As soon as you
 * have a second call site for func1, it ceases to be top level, and
 * will trigger the error..I guess it should be same with the various
 * right/left comb functions...
 * ```
 *
 * So, because of that we are forced to pass `self_addr` argument to
 * every function so that overall it can be used in `validate_owner_hook`
 *
 * 2. As was stated earlier, we are forced to pass explicit conversions
 * of parameter structure making such trivial functions as
 * `convert_to_transfer_descriptor` the way they are now.
 *)

#include "spec.ligo"

(* ------------------------------------------------------------- *)

type to_hook is address -> option (contract (transfer_descriptor_param))

(*
 * Helper function that converts `transfer_param` to `transfer_descriptor_param`.
 *)
function convert_to_transfer_descriptor
  ( const self_addr      : address
  ; const param_sender_addr : address
  ; const transfer_destination : transfer_destination
  ): transfer_descriptor_param is block
{ const transfer_destination_descriptor_ : transfer_destination_descriptor_ = record
    [ to_      = Some (transfer_destination.0)
    ; token_id = transfer_destination.1.0
    ; amount   = transfer_destination.1.1
    ]
; const transfer_destination_descriptor : transfer_destination_descriptor =
    Layout.convert_to_right_comb ((transfer_destination_descriptor_ : transfer_destination_descriptor_))
; const transfer_descriptor_ : transfer_descriptor_ = record
    [ from_ = Some (param_sender_addr)
    ; txs   = list [ transfer_destination_descriptor ]
    ]
; const transfer_descriptor_batch : list (transfer_descriptor) = list
    [ Layout.convert_to_right_comb ((transfer_descriptor_ : transfer_descriptor_)) ]
; const transfer_descriptor_param : transfer_descriptor_param_ = record
    [ fa2      = self_addr
    ; batch    = transfer_descriptor_batch
    ; operator = Tezos.sender
    ]
} with Layout.convert_to_right_comb ((transfer_descriptor_param : transfer_descriptor_param_))

(*
 * Helper function that merges two `operation`s `list`s.
 *)
function merge_operations
  ( const fst : list (operation)
  ; const snd : list (operation)
  ) : list (operation) is List.fold
    ( function
      ( const operations : list (operation)
      ; const operation  : operation
      ) : list (operation) is operation # operations
    , fst
    , snd
    )

