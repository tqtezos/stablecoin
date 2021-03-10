-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: MIT

module Main
  ( main
  ) where

import Cleveland.Ingredients (ourIngredients)
import Morley.Nettest.Tasty (nettestMainWithIngredients)

import Tree (tests)

main :: IO ()
main = tests >>= nettestMainWithIngredients ourIngredients
