// SPDX-FileCopyrightText: 2020 TQ Tezos
// SPDX-License-Identifier: MIT

#include "spec.ligo"


type metadata_storage is token_metadata

type metadata_storage is record
   token_metadata : big_map (nat, token_metadata)
; dummy_field : unit
end

(*
 * Token Metadata storage contract. We use this contract to store the metadata of the
 * Stablecoin contract, and just store address of this contract in its storage.
 *)
function metadata
  ( const action : unit
  ; const store  : metadata_storage
  ) : list (operation) * metadata_storage is
      ( (nil : list (operation))
      , store
      )

