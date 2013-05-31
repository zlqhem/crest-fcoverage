module Main (main) where

import Properties (properties)
import Test.Framework (defaultMain, testGroup)

main :: IO ()
main = defaultMain [
        testGroup "properties" properties
        ]
