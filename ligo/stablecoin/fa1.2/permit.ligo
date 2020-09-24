// SPDX-FileCopyrightText: 2020 tqtezos
// SPDX-License-Identifier: MIT

(*
 * Permit library for stablecoin smart-contract
 *)


(*
 * Alternative version of `sender_check` that takes a lambda to run when
 * 1) the sender is not the expected user
 * 2) the expected user did not issue a permit.
 *
 *)
function sender_check_
  ( const expected_user : address
  ; const store : storage
  ; const full_param : closed_parameter
  ; const on_err : unit -> storage
  ) : storage is
  if Tezos.sender = expected_user then
    store
  else
    block {
      const full_param_hash : blake2b_hash = Crypto.blake2b(Bytes.pack(full_param))
    ; const user_permits : user_permits =
        case Big_map.find_opt(expected_user, store.permits) of
          Some(user_permits) -> user_permits
        | None -> new_user_permits
        end
    }
      with
        case Map.find_opt(full_param_hash, user_permits.permits) of
          None ->
            // The expected user did not call this entrypoint, nor did they issue a permit, so we fail here.
            on_err(Unit)
        | Some(permit_info) ->
            if has_expired(store.default_expiry, user_permits.expiry, permit_info) then
              (failwith("EXPIRED_PERMIT") : storage)
            else
              // A permit exists. Consume it (i.e. delete it from the permits map).
              store with record [
                permits = Big_map.update(
                  expected_user,
                  Some(
                    user_permits with record [
                      permits = Map.remove(full_param_hash, user_permits.permits)
                    ]
                  ),
                  store.permits
                )
              ]
        end
