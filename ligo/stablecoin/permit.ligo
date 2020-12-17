// SPDX-FileCopyrightText: 2020 tqtezos
// SPDX-License-Identifier: MIT

(*
 * Permit library for stablecoin smart-contract
 *)

(*
 * Maximum value that is allowed for expiry in the argument of a
 * `Set_expiry` call.
 *)
const permit_expiry_limit: nat = 31557600000n; // A thousand years

(*
 * Check whether a permit has expired.
 *)
function has_expired
  ( const default_expiry: seconds
  ; const user_expiry_opt: option(seconds)
  ; const permit_info: permit_info
  ) : bool is block {
  const expiry: seconds =
    case permit_info.expiry of
    | Some(permit_expiry) -> permit_expiry
    | None ->
        case user_expiry_opt of
        | Some(user_expiry) -> user_expiry
        | None -> default_expiry
        end
    end
} with permit_info.created_at + int(expiry) < Tezos.now

(*
 * Checks if:
 * 1) the sender is `expected_user`
 * 2) or, failing that, if `expected_user` has issued a permit allowing this call to go through.
 *
 * If both these checks fail, the call fails with `err_message`.
 * If a permit exists, but has expired, the call fails with `"EXPIRED_PERMIT"`.
 *
 * If a valid permit is used, it'll be removed from the storage and the
 * updated storage will be returned.
 *
 * NOTE: Since this function might consume a permit, care must be taken to ensure
 * it is only used ONCE per call.
 *)
function sender_check
  ( const expected_user : address
  ; const store : storage
  ; const full_param : closed_parameter
  ; const err_message : string
  ) : storage is
  if Tezos.sender = expected_user then
    store
  else
    block {
      const full_param_hash : blake2b_hash = Crypto.blake2b(Bytes.pack(full_param))
    ; const user_permits : user_permits =
        case Big_map.find_opt(expected_user, store.permits) of
          Some(user_permits) -> user_permits
        | None ->
            // The expected user did not call this entrypoint, nor did they issue a permit, so we fail here.
            (failwith(err_message) : user_permits)
        end
    }
      with
        case Map.find_opt(full_param_hash, user_permits.permits) of
          None ->
            // The expected user did not call this entrypoint, nor did they issue a permit, so we fail here.
            (failwith(err_message) : storage)
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

(*
 * Deletes all expired permits issued by the given user.
 *)
function delete_expired_permits
  ( const default_expiry: seconds
  ; const user: address
  ; const permits: permits
  ) : permits is
  case Big_map.find_opt(user, permits) of
  | None -> permits
  | Some(user_permits) -> block {
      const updated_map: map (blake2b_hash, permit_info) =
        Map.fold
          ( function
              ( const acc: map (blake2b_hash, permit_info)
              ; const keyValue: (blake2b_hash * permit_info)
              ): map (blake2b_hash, permit_info) is
                if has_expired(default_expiry, user_permits.expiry, keyValue.1)
                  then Map.remove(keyValue.0, acc)
                  else acc
          , user_permits.permits
          , user_permits.permits
          )
    ; const updated_user_permits: user_permits =
        user_permits with record [ permits = updated_map ]
    } with
       Big_map.update(user, Some(updated_user_permits), permits)
  end

(*
 * Initial `user_permits` state for a user attempting to create
 * a permit for the first time.
 *)
const new_user_permits : user_permits =
  record [
    permits = (Map.empty : map (blake2b_hash, permit_info))
  ; expiry = (None : option(seconds))
  ]

(*
 * Fails with `"DUP_PERMIT"` if the given permit hash already exists and hasn't expired.
 *)
function check_duplicates
  ( const default_expiry: seconds
  ; const user_expiry_opt: option(seconds)
  ; const user_permits: user_permits
  ; const permit: blake2b_hash
  ) : unit is
  case Map.find_opt(permit, user_permits.permits) of
  | None -> unit
  | Some(permit_info) ->
      if (has_expired(default_expiry, user_expiry_opt, permit_info))
        then unit
        else failwith("DUP_PERMIT")
  end

(*
 * Inserts an already validated permit in the permits storage.
 *)
function insert_permit
  ( const default_expiry: seconds
  ; const user: address
  ; const permit: blake2b_hash
  ; const permits: permits
  ) : permits is block {
    // Look for the user's permits, or create a new record if one doesn't exist yet.
    const user_permits: user_permits =
      case Big_map.find_opt(user, permits) of
        Some(user_permits) -> user_permits
      | None -> new_user_permits
      end

  ; check_duplicates(default_expiry, user_permits.expiry, user_permits, permit)

  // Add a new entry to this user's permits
  ; const updated_user_permits: user_permits =
      user_permits with record [
        permits = Map.add(
          permit,
          record [ created_at = Tezos.now; expiry = (None : option(seconds)) ],
          user_permits.permits
        )
      ]
  }
  with
   Big_map.update(user, Some(updated_user_permits), permits)

(*
 * Sets the default expiry for a user.
 *
 * If the user already had an expiry set, the old expiry is overriden by the new one.
 *)
function set_user_default_expiry
  ( const user : address
  ; const new_expiry : seconds
  ; const permits : permits
  ) : permits is block
{ const user_permits: user_permits =
    case Big_map.find_opt(user, permits) of
    | Some(user_permits) -> user_permits
    | None -> new_user_permits
    end
; const updated_user_permits: user_permits =
    user_permits with record [
      expiry = Some(new_expiry)
    ]
} with Big_map.update(user, Some(updated_user_permits), permits)

(*
 * Checks if the permit has expired, sets a new expiry otherwise.
 *)
function set_permit_expiry_with_check
  ( const permit_info : permit_info
  ; const new_expiry : seconds
  ) : option(permit_info) is block {
    const permit_age: int = Tezos.now - permit_info.created_at;
  } with if permit_age >= int(new_expiry)
      then (None : option(permit_info))
      else Some(permit_info with record [
        expiry = Some(new_expiry)
      ])

(*
 * Sets the expiry for a permit.
 *
 * If the permit already had an expiry set, the old expiry is overriden by the new one.
 * If the permit does not exist, nothing happens.
 *)
function set_permit_expiry
  ( const user : address
  ; const permit : blake2b_hash
  ; const new_expiry : seconds
  ; const permits : permits
  ; const default_expiry : seconds
  ) : permits is
  if new_expiry < permit_expiry_limit then
    case Big_map.find_opt(user, permits) of
    | None -> permits
    | Some(user_permits) ->
        case Map.find_opt(permit, user_permits.permits) of
        | None -> permits
        | Some(permit_info) ->
            block {
              const updated_user_permits : user_permits =
                if has_expired(default_expiry, user_permits.expiry, permit_info)
                  then user_permits
                  else user_permits with record [
                    permits = Map.update(
                      permit,
                      set_permit_expiry_with_check(permit_info, new_expiry),
                      user_permits.permits
                    )
                  ]
            } with Big_map.update(user, Some(updated_user_permits), permits)
        end
    end
  else
    (failwith("EXPIRY_TOO_BIG") : permits)
