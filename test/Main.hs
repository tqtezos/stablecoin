module Main
  ( main
  ) where

import           Test.Tasty (defaultMain)
import           Tree       (tests)

main :: IO ()
main = tests >>= defaultMain
