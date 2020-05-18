// SPDX-FileCopyrightText: 2020 tqtezos
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
 * so long as func1 is only called from one palace. As soon as you
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

(*
 * Generic transfer hook
 *)

type get_owner is transfer_param -> address
type to_hook is address -> option (contract (transfer_descriptor_param))

(*
 * Helper function that converts `transfer_param` to `transfer_descriptor_param`
 *)
function convert_to_transfer_descriptor
  ( const self_addr      : address
  ; const transfer_param : transfer_param
  ): transfer_descriptor_param is block
{ const transfer_descriptor : transfer_descriptor_ = record
    [ from_    = Some (transfer_param.0)
    ; to_      = Some (transfer_param.1.0)
    ; token_id = transfer_param.1.1.0
    ; amount   = transfer_param.1.1.1
    ]
; const transfer_descriptor_batch : list (transfer_descriptor) = list
    [ Layout.convert_to_right_comb ((transfer_descriptor : transfer_descriptor_)) ]
; const transfer_descriptor_param : transfer_descriptor_param_ = record
    [ fa2      = self_addr
    ; batch    = transfer_descriptor_batch
    ; operator = Tezos.sender
    ]
} with Layout.convert_to_right_comb ((transfer_descriptor_param : transfer_descriptor_param_))

(*
 * An actual validation of owner hook specified by `to_hook` parameter.
 * If such is present, append it to a passed list of transactions.
 *)
function validate_owner_hook
  ( const self_addr      : address
  ; const ops            : list(operation)
  ; const transfer_param : transfer_param
  ; const get_owner      : get_owner
  ; const to_hook        : to_hook
  ; const is_required    : bool
  ) : list(operation) is case to_hook (get_owner(transfer_param)) of
      Some (hook) -> Tezos.transaction (convert_to_transfer_descriptor(self_addr, transfer_param), 0mutez, hook) # ops
    | None ->
      if is_required
      then (failwith ("SENDER_HOOK_UNDEFINED") : list (operation))
      else ops
    end;

(*
 * Validates given list of hooks by appending them to a list of transactions
 * achieved by deconstructing transfer parameter batch parameter
 *)
function validate_owner_hooks
  ( const self_addr   : address
  ; const transfers   : transfer_params
  ; const get_owner   : get_owner
  ; const to_hook     : to_hook
  ; const is_required : bool
  ) : list (operation) is block
  { function folding
      ( const ops: list(operation)
      ; const tp: transfer_param
      ) : list(operation) is
        validate_owner_hook(self_addr, ops, tp, get_owner, to_hook, is_required)
  } with List.fold(folding, transfers, (nil : list(operation)))

(*
 * Produces a list of transactions of transfer hooks depending on the policy provided
 *)
function validate_owner
  ( const self_addr  : address
  ; const descriptor : transfer_params
  ; const policy     : owner_transfer_policy_
  ; const get_owner  : get_owner
  ; const to_hook    : to_hook
  ) : list (operation) is block
  {
  const ops : list (operation) = case policy of
    Owner_no_op -> (nil : list (operation))
  | Optional_owner_hook -> validate_owner_hooks (self_addr, descriptor, get_owner, to_hook, False)
  | Required_owner_hook -> validate_owner_hooks (self_addr, descriptor, get_owner, to_hook, True)
  end
  } with ops

(*
 * Retrieves contract from `tokens_received` entrypoint
 *)
function to_receiver_hook
  ( const receiving_address : address
  ) : option (contract (transfer_descriptor_param)) is
    Tezos.get_entrypoint_opt ("%tokens_received", receiving_address)

(*
 * Make a list of transactions for each validated receiver.
 *)
function validate_receivers
  ( const params : transfer_params
  ; const policy     : owner_transfer_policy_
  ) : list (operation) is block
{ const self_addr : address = Tezos.self_address
; const get_receiver : get_owner = function
    ( const param : transfer_param
    ) : address is param.1.0
} with validate_owner (self_addr, params, policy, get_receiver, to_receiver_hook)

(*
 * Retrieves contract from `tokens_sent` entrypoint
 *)
function to_sender_hook
  ( const sender_address : address
  ) : option (contract (transfer_descriptor_param)) is
    Tezos.get_entrypoint_opt ("%tokens_sent", sender_address)

(*
 * Make a list of transactions for each validated sender
 *)
function validate_senders
  ( const params : transfer_params
  ; const policy : owner_transfer_policy_
  ) : list (operation) is block
{ const self_addr : address = Tezos.self_address
; const get_sender : get_owner = function
    ( const param : transfer_param
    ) : address is param.0
} with validate_owner (self_addr, params, policy, get_sender, to_sender_hook)

(*
 * Constructs a list of transactions of transfer hook items
 * if such are set for associated addresses
 *)
function generic_transfer_hook
  ( const transfer_params        : transfer_params
  ; const permissions_descriptor : permissions_descriptor_
  ) : list (operation) is block
{ const sender_policy : owner_transfer_policy_ =
    Layout.convert_from_right_comb ((permissions_descriptor.sender : owner_transfer_policy))
; const sender_ops : list (operation) =
    validate_senders (transfer_params, sender_policy)
; const receiver_policy : owner_transfer_policy_ =
    Layout.convert_from_right_comb ((permissions_descriptor.receiver : owner_transfer_policy))
; const receiver_ops : list (operation) =
    validate_receivers (transfer_params, receiver_policy)
} with List.fold (* Merge two lists *)
    ( function
      ( const operations : list (operation)
      ; const operation  : operation
      ) : list (operation) is operation # operations
    , receiver_ops
    , sender_ops
    )