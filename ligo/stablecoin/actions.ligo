#include "operator.ligo"

// function credit_to
//   ( const parameter : mint_params
//   ; const store     : storage
//   ) : storage is block
// { var updated_ledger : ledger := store.ledger
// ; case store.ledger[parameter.to_] of
//     Some (ledger_balance) -> block
//     { const updated_balance : nat = ledger_balance + parameter.amount
//     ; updated_ledger := Big_map.update(parameter.to_, Some (updated_balance), updated_ledger)
//     }
//   // TODO
//   | None -> failwith ("Given address is not present in ledger")
//   end
// ; const ups : storage = store with record [ ledger = updated_ledger ]
// } with ups

function credit_to
  ( const parameter : mint_params
  ; const store     : storage
  ) : storage is block
{ var updated_balance : nat := 0n
; case store.ledger[parameter.to_] of
    Some (ledger_balance) -> updated_balance := ledger_balance
  | None -> skip
  end
; updated_balance := updated_balance + parameter.amount
; const updated_ledger : ledger =
    Big_map.update(parameter.to_, Some (updated_balance), store.ledger)
; const ups : storage = store with record [ ledger = updated_ledger ]
} with ups

function debit_from
  ( const parameter : burn_params
  ; const store     : storage
  ) : storage is block
{ var updated_ledger : ledger := store.ledger
; case store.ledger[parameter.from_] of
    Some (ledger_balance) -> if parameter.amount > ledger_balance
      then failwith ("Given address has not enough balance to debit from")
      else
      { const updated_balance : nat = abs (ledger_balance - parameter.amount)
      ; updated_ledger := Big_map.update(parameter.from_, Some (updated_balance), updated_ledger)
      }
  | None -> failwith ("Given address is not present in ledger")
  end
; const ups : storage = store with record [ ledger = updated_ledger ]
} with ups

//   var ups : storage := store;
//   case ups.ledger[parameter.from_] of
//     Some (ledger_balance) -> block
//     {
//       if ledger_balance < parameter.amount
//       then failwith ("Not enough balance to debit from the given wallet")
//       else {
//         ledger_balance := abs (ledger_balance - parameter.amount);
//         Big_map.update (parameter.from_, Some (ledger_balance), ups.ledger);
//         // ups.total_supply := abs (ups.total_supply - parameter.amount);
//       }
//     }
//   | None -> failwith ("Given address is not present in ledger")
//   end
// } with ups

function validate_token_type
  ( const caller   : string
  ; const token_id : token_id
  ) : unit is
    if token_id =/= default_token_id
    then failwith (caller ^ ": Call does not support multiple token types")
    else unit

function validate_token_types
  ( const caller    : string
  ; const token_ids : list (token_id)
  ) : unit is block
  { const u : list (unit) = List.map
      ( function
        ( const parameter : token_id
        ) : unit is
          validate_token_type (caller, parameter)
      , token_ids
      )
  } with unit

function transfer
  ( const parameters : transfer_params
  ; const store      : storage
  ) : entrypoint is block
{

 case store.permissions.operator of
    M_left (u) -> failwith ("permitted")
  | M_right (u) -> failwith ("denied")
  end
// validate_operator (store.permissions, parameters, store.operators)
; function transfer_tokens
   ( const accumulator : storage
   ; const parameter   : transfer_param
   ) : storage is block
  { const debit_params : burn_params = record
     [ from_  = parameter.0
     ; amount = parameter.1.1.1
     ]

  ; const credit_params : mint_params = record
     [ to_    = parameter.1.0
     ; amount = parameter.1.1.1
     ]

  ; const debited_storage  : storage = debit_from (debit_params, accumulator)
  ; const credited_storage : storage = credit_to (credit_params, debited_storage)
  } with credited_storage

; const upd : list (operation) = nil
; const ups : storage = List.fold (transfer_tokens, parameters, store)
} with (upd, ups)

function balance_of
  ( const parameter : balance_of_params
  ; const store      : storage
  ) : entrypoint is block
{
  function retreive_balance
    ( const request : balance_of_request
    ) : balance_of_response is block
  {
    validate_token_type ("balance_of", request.token_id);

    var retreived_balance : nat := abs (0);

    case Big_map.find_opt (request.owner, store.ledger) of
      Some (ledger_balance) -> retreived_balance := ledger_balance
    | None -> skip
    end;

  } with (request, retreived_balance);

  const responses : list (balance_of_response) =
    List.map (retreive_balance, parameter.0);

  const transfer_operation : operation =
    Tezos.transaction (responses, 0mutez, parameter.1);

  const upd : list (operation) = list [transfer_operation];
  const ups : storage = store;
} with (upd, ups)

function total_supply
  ( const parameter : total_supply_params
  ; const store      : storage
  ) : entrypoint is block
{
  validate_token_types ("total_supply", parameter.0);

  const response : total_supply_response = record
    [ token_id     = default_token_id
    ; total_supply = store.total_supply
    ];

  const op : operation =
    Tezos.transaction (list [response], 0mutez, parameter.1);

  const upd : list (operation) = list [op];
  const ups : storage = store;
} with (upd, ups)

function token_metadata
  ( const parameter : token_metadata_params
  ; const store      : storage
  ) : entrypoint is block
{
  validate_token_types ("token_metadata", parameter.0);
  const mdata_ : token_metadata_ = store.token_metadata;
  const mdata : token_metadata =
    ( mdata_.token_id
    , ( mdata_.symbol
      , ( mdata_.name
        , ( mdata_.decimals , mdata_.extras))))
} with
  ( list [Tezos.transaction
    ( list [mdata]
    , 0mutez
    , parameter.1
    )]
  , store
  )

function permission_descriptor
  ( const parameter : permissions_descriptor_params
  ; const store     : storage
  ) : entrypoint is block
  {
  const permissions_ : permissions_descriptor_ = store.permissions;

  const permissions : permissions_descriptor =
    ( permissions_.self
    , ( permissions_.operator
      , ( permissions_.receiver
        , ( permissions_.sender, permissions_.custom))));

  } with
  ( list [Tezos.transaction
    ( permissions
    , 0mutez
    , parameter
    )]
  , store
  )

function update_operators_action
  ( const parameter : update_operator_params
  ; const store     : storage
  ) : entrypoint is block
{ const upd : list (operation) = nil
; const updated_operators : operators =
    update_operators (parameter, store.operators)
; const ups : storage =
    store with record [ operators = updated_operators ]
} with (upd, ups)

function is_operator_action
  ( const parameter : is_operator_params
  ; const store      : storage
  ) : entrypoint is block
  { case store.permissions.operator of
      M_right (u) -> failwith ("is_operator_action: Operator transfer is prohibited")
    | M_left (u) -> skip
    end
  } with
  ( list [is_operator (parameter, store.operators)]
  , store
  )
