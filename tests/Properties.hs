module Properties (properties) where

import Test.QuickCheck
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Fcoverage
import Data.Char

pDumb :: String -> Bool
pDumb s = case maybeInt s of
  Just numS -> and [isDigit x | x <- s]
  Nothing -> not $ and [isDigit x | x <- s] 

--properties :: [Test]
properties =
  [ testProperty "dumb" pDumb
  ]
