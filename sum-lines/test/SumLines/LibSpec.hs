module SumLines.LibSpec
  ( spec
  ) where

import Data.Conduit ((.|), await, runConduitPure, yield)
import Test.Hspec (Spec, describe, it, shouldBe)

import Lib (doubleFromLineC)

spec :: Spec
spec = describe "doubleFromLineC" $ do
  it "should yield a (Just Double) from a line containing a double" $
    let x = runConduitPure $ yield "45.18" .| doubleFromLineC .| await
      in x `shouldBe` Just 45.18

  it "should yield a (Just 0) from a line that does not start with a number" $
    let x = runConduitPure $ yield "awk does this too" .| doubleFromLineC .| await
     in x `shouldBe` Just 0
