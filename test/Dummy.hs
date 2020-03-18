module Dummy
  ( test_dummy
  ) where

import Test.HUnit ((@?=))
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertBool, testCase)

test_dummy :: [TestTree]
test_dummy = [ testCase "always true" $ do
    (2 :: Integer) + (2 :: Integer) @?= (4 :: Integer)
    assertBool "the list is empty" $ null []
    ("foo" :: String) @?= ("foo" :: String)]
