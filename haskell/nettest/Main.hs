-- SPDX-FileCopyrightText: 2021 Oxhead Alpha
-- SPDX-License-Identifier: MIT
{-# OPTIONS_GHC -Wno-deprecations #-}

module Main
  ( main
  ) where

import Test.Cleveland.Ingredients (ourIngredients)
import Test.Cleveland.Tasty (clevelandMainWithIngredients, setAliasPrefix)

import Tree (tests)

main :: IO ()
main = tests >>= clevelandMainWithIngredients ourIngredients . setAliasPrefix "nettest.Stablecoin"
