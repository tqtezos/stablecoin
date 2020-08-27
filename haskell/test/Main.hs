-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: MIT

module Main
  ( main
  ) where

import Test.Tasty (defaultMain)
import Tree (tests)

main :: IO ()
main = tests >>= defaultMain
