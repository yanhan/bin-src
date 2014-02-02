module Main (
  main
) where

import Test.Framework (defaultMain)
import BinSrc.GitUtils.Tests (tests)

main :: IO ()
main = defaultMain
    [ BinSrc.GitUtils.Tests.tests
    ]
