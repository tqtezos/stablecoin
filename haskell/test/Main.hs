-- SPDX-FileCopyrightText: 2021 Oxhead Alpha
-- SPDX-License-Identifier: MIT

module Main
  ( main
  ) where

import Test.Cleveland.Ingredients (ourIngredients)
import Test.Cleveland.Tasty (clevelandMainWithIngredients)

import Tree (tests)

main :: IO ()
main = tests >>= clevelandMainWithIngredients ourIngredients
