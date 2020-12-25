// SPDX-FileCopyrightText: 2020 TQ Tezos
// SPDX-License-Identifier: MIT

type contract_metadata_storage is record
  metadata : big_map (string, bytes)
; dummy_field : unit // We need this field for the metadata field to be annotated with the %metadata%.
end

(*
 * Token Metadata storage contract.
 * We use this contract to store the metadata of the Stablecoin contract.
 * The Stablecoin contract only needs to store the address of this contract in its storage.
 *)
function metadata
  ( const action : unit
  ; const store  : contract_metadata_storage
  ) : list (operation) * contract_metadata_storage is
      ( (nil : list (operation))
        , store
      )
