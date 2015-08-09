{-# OPTIONS_GHC -fno-warn-orphans #-}

import Data.List
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Linear.V2
import Linear.V3

import Types
import LCG
import Field

instance Arbitrary a => Arbitrary (V2 a) where
  arbitrary = V2 <$> arbitrary <*> arbitrary
  shrink (V2 x y) = map (\(x', y') -> V2 x' y') $ shrink (x, y)

instance Arbitrary a => Arbitrary (V3 a) where
  arbitrary = V3 <$> arbitrary <*> arbitrary <*> arbitrary
  shrink (V3 x y z) = map (\(x', y', z') -> V3 x' y' z') $ shrink (x, y, z)

hcellCellConv :: Cell -> Bool
hcellCellConv c = hcellToCell (cellToHCell c) == c

rngExampleTest :: Assertion
rngExampleTest = do
  let lcg = defLCG 17
      vals g = let (x, g') = nextLCG g in x : vals g'
      example = [0, 24107, 16552, 12125, 9427, 13152, 21440, 3383, 6873, 16117]
  assertBool "LCG doesn't satisfy example" (example `isPrefixOf` vals lcg)

tests :: TestTree
tests = testGroup "tests"
        [ testProperty "hcell-cell conversion" hcellCellConv
        , testCase "rng example seq" rngExampleTest
        ]

main :: IO ()
main = defaultMain tests
