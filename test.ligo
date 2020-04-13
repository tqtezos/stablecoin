// SPDX-FileCopyrightText: 2020 tqtezos
// SPDX-License-Identifier: MIT

function execute (const u: unit) : unit is unit

type storage is int

type entrypoint is list (operation) * storage

type sum_arg is (int * int)
type put_arg is int

type sum_lambda is (sum_arg * storage) -> entrypoint
type put_lambda is (put_arg * storage) -> entrypoint

type sum_param is big_map (unit, (sum_lambda * sum_arg))
type put_param is big_map (unit, (put_lambda * put_arg))

type parameter is
  Sum of sum_param
// | Put of put_param

function put
  ( const param : int
  ; const store : storage
  ) : entrypoint is block
{
  skip
} with ((nil : list (operation)), store)

/*
 * I've separated it from `sum_action` to be able to
 * pass this function from cli
 */
function sum
  ( const param : sum_arg
  ; const store : storage
  ) : entrypoint is block
{
  const updated_storage : storage = param.0 + param.1;
} with ((nil : list (operation)), updated_storage)

function execute_sum
  ( const s : sum_param
  ) : (sum_lambda * sum_arg) is block
{
  const f : option (sum_lambda * sum_arg) = Big_map.find_opt (unit, s);
} with case f of
    Some (r) -> r
  | None -> (failwith ("memems") : sum_lambda * sum_arg)
  end

function sum_action
  ( const param : sum_param
  ; const store : storage
  ) : entrypoint is block
{
  const er : sum_lambda * sum_arg = execute_sum (param);
  const f  : sum_lambda = er.0;
} with f (er.1, store)

function main
  ( const action : parameter
  ; const store  : storage
  ) : entrypoint is
  case action of
    Sum (param) -> sum_action (param, store)
  // | Put (param) -> put (param, store)
  end
