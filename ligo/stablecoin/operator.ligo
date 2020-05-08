#include "hooks.ligo"

type owner_tokens is map (address, set (token_id))

// TODO: we should probably ommit `owner` in `operator_param_` as @gromak suggested
function validate_operator_owner_is_sender
  ( const param : operator_param
  ) : unit is if param.0 = Tezos.sender
  then unit
  else failwith ("valdiate_oeprator_param: Sender is not token owner")

function is_operator_impl
  ( const param     : operator_param
  ; const operators : operators
  ) : bool is block
  { const operator_key : (owner * operator) = (param.0, param.1.0)
  } with case Big_map.find_opt (operator_key, operators) of
      None -> False
    | Some (u) -> True
    end

function is_operator
  ( const param     : is_operator_params
  ; const operators : operators
  ) : operation is block
{ const response : is_operator_response =
    (param.0, is_operator_impl (param.0, operators))
} with Tezos.transaction (response, 0mutez, param.1)

function validate_operator
  ( const permissions_descriptor : permissions_descriptor_
  ; const transfer_parameters    : transfer_params
  ; const operators              : operators
  ) : unit is block
{
  const can_transfer_to_self : bool =
    case permissions_descriptor.self of
      M_left (u)     -> True
    | M_right (u) -> False
    end;

  const can_transfer_operator : bool =
    case permissions_descriptor.operator of
      M_left (u)     -> True
    | M_right (u) -> False
    end;

  const operator : address = Tezos.sender;

  function owner_folding
    ( const owner_tokens_accumulator : owner_tokens
    ; const transfer_operation       : transfer_param
    ) : owner_tokens is block
    {
      const tokens : option (set (token_id)) =
        Map.find_opt (transfer_operation.0, owner_tokens_accumulator);
      const updated_tokens : set (token_id) =
        case tokens of
          None -> set [transfer_operation.1.1.0]
        | Some (tokens) -> Set.add (transfer_operation.1.1.0, tokens)
        end;
    } with Map.update (transfer_operation.0, Some (updated_tokens), owner_tokens_accumulator);

  const owner_tokens : owner_tokens = List.fold
    ( owner_folding
    , transfer_parameters
    , (map [] : owner_tokens)
    );


  function validate_owner
    ( const owner  : address
    ; const tokens : set (token_id)
    ) : unit is block
  {
    var is_operator : bool := False;

    if can_transfer_to_self and owner = operator
    then skip
    else if (not can_transfer_operator)
      then failwith ("validate_operator: Operator transfer is prohibited")
      else
        {
        //   const tokens_ : operator_tokens =
        //     Layout.convert_to_right_comb ((Some_tokens (tokens) : operator_tokens_))
        // ; const operator_param_ : operator_param_ = record
        //     [ owner = owner
        //     ; operator = operator
        //     ; tokens = tokens_
        //     ]
        // ; const operator_param : operator_param =
        //     Layout.convert_to_right_comb ((operator_param_ : operator_param_))
          const operator_param : operator_param = (owner, (operator, (M_right (tokens) : operator_tokens)))
        ; is_operator := is_operator_impl (operator_param, operators)
        }
  } with
      if is_operator
      then unit
      else (failwith ("validate_operator: not permitted operator given") : unit)

} with Map.iter
    ( validate_owner
    , owner_tokens
    )


function add_operator
  ( const param     : operator_param
  ; const operators : operators
  ) : operators is block
{ validate_operator_owner_is_sender (param)
; const operator_key : (owner * operator) = (param.0, param.1.0)
} with Big_map.add (operator_key, unit, operators)

function remove_operator
  ( const param     : operator_param
  ; const operators : operators
  ) : operators is block
{ validate_operator_owner_is_sender (param)
; const operator_key : (owner * operator) = (param.0, param.1.0)
} with Big_map.remove (operator_key, operators)

function update_operators
  ( const params    : list (update_operator_param)
  ; const operators : operators
  ) : operators is List.fold
    ( function
        ( const operators             : operators
        ; const update_operator_param : update_operator_param
        ) : operators is case update_operator_param of
          Add_operator    (operator) -> add_operator    (operator, operators)
        | Remove_operator (operator) -> remove_operator (operator, operators)
        end
    , params
    , operators
    )
