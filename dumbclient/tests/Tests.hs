{-# OPTIONS_GHC -fno-warn-orphans #-}

import Linear.V2
import Linear.V3
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

import Types
import Field

instance Arbitrary a => Arbitrary (V2 a) where
  arbitrary = V2 <$> arbitrary <*> arbitrary
  shrink (V2 x y) = map (\(x', y') -> V2 x' y') $ shrink (x, y)

instance Arbitrary a => Arbitrary (V3 a) where
  arbitrary = V3 <$> arbitrary <*> arbitrary <*> arbitrary
  shrink (V3 x y z) = map (\(x', y', z') -> V3 x' y' z') $ shrink (x, y, z)

hcellCellConv :: Cell -> Bool
hcellCellConv c = hcellToCell (cellToHCell c) == c

tests :: TestTree
tests = testGroup "field"
        [ testProperty "hcell-cell conversion" hcellCellConv
        ]

main :: IO ()
main = defaultMain tests
