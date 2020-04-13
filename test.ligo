// SPDX-FileCopyrightText: 2020 tqtezos
// SPDX-License-Identifier: MIT

function execute (const u: unit) : unit is unit

/* ------------------------------------------------------------- */

type state is int

type entrypoint is list (operation) * state

type sum_arg is (int * int)
type sum_lambda is (sum_arg * state) -> entrypoint
type sum_map is big_map (bool, sum_lambda)

type put_arg is int 
type put_lambda is (put_arg * state) -> entrypoint 
type put_map is big_map (bool, put_lambda)

type storage is record 
[ sum_entry : sum_map
; put_entry : put_map 
; state     : state
]

type root_entrypoint is list (operation) * storage

type parameter is
  Sum of sum_arg
| Put of put_arg

/* ------------------------------------------------------------- */

function put
  ( const param : put_arg
  ; const store : storage
  ) : entrypoint is block
{ const updated_storage : state = param;
} with ((nil : list (operation)), updated_storage)

function execute_put
  ( const s : put_map
  ) : put_lambda is block
{
  const f : option (put_lambda) = Big_map.find_opt (True, s);
} with case f of
    Some (r) -> r
  | None -> (failwith ("memems") : put_lambda)
  end

function put_action
  ( const param : put_arg
  ; const store : storage
  ) : root_entrypoint is block
{ const f : put_lambda = execute_put (store.put_entry)
; const r : entrypoint = f (param, store.state)
; const s : storage = store with record [ state = r.1 ]
} with (r.0, s)

/* ------------------------------------------------------------- */

function sum
  ( const param : sum_arg
  ; const store : state
  ) : entrypoint is block
{ const updated_storage : state = param.0 + param.1;
} with ((nil : list (operation)), updated_storage)

function execute_sum
  ( const s : sum_map
  ) : sum_lambda is block
{
  const f : option (sum_lambda) = Big_map.find_opt (True, s);
} with case f of
    Some (r) -> r
  | None -> (failwith ("memems") : sum_lambda)
  end

function sum_action
  ( const param : sum_arg
  ; const store : storage
  ) : root_entrypoint is block
{ const f : sum_lambda = execute_sum (store.sum_entry)
; const r : entrypoint = f (param, store.state)
; const s : storage = store with record [ state = r.1 ]
} with (r.0, s)

/* ------------------------------------------------------------- */

function main
  ( const action : parameter
  ; const store  : storage
  ) : root_entrypoint is
  case action of
    Sum (param) -> sum_action (param, store)
  | Put (param) -> put_action (param, store)
  end
