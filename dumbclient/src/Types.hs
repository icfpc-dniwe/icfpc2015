{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Control.Monad
import Data.Map.Strict (Map)
import qualified Data.Vector as V
import Data.Aeson
import Linear.V2
import Linear.V3
import Control.Lens

instance (FromJSON a) => FromJSON (V2 a) where
  parseJSON (Object c) = V2 <$> c .: "x" <*> c .: "y"
  parseJSON (Array vs) = do
    unless (V.length vs == 2) $ fail "parseJSON (V2): invalid length"
    x <- parseJSON (vs V.! 0)
    y <- parseJSON (vs V.! 1)
    return $ V2 x y
  parseJSON _ = fail "parseJSON (V2): invalid type"

instance (FromJSON a) => FromJSON (V3 a) where
  parseJSON (Object c) = V3 <$> c .: "x" <*> c .: "y" <*> c .: "z"
  parseJSON (Array vs) = do
    unless (V.length vs == 3) $ fail "parseJSON (V3): invalid length"
    x <- parseJSON (vs V.! 0)
    y <- parseJSON (vs V.! 1)
    z <- parseJSON (vs V.! 2)
    return $ V3 x y z
  parseJSON _ = fail "parseJSON (V3): invalid type"

type Cell = V2 Int

data Direction = E | W | SE | SW
               deriving (Show, Eq, Ord, Enum, Bounded)

data TDirection = CW | CCW
                deriving (Show, Eq, Ord, Enum, Bounded)

data Command = Move !Direction
             | Turn !TDirection
             deriving (Show, Eq, Ord)

instance Bounded Command where
  minBound = Move minBound
  maxBound = Turn maxBound

instance Enum Command where
  fromEnum (Move m) = fromEnum m
  fromEnum (Turn t) = fromEnum (maxBound :: Direction) + 1 + fromEnum t
  toEnum n
    | n <= fromEnum (maxBound :: Direction) = Move $ toEnum n
    | otherwise = Turn $ toEnum $ n - fromEnum (maxBound :: Direction) - 1

type CColor = V3 Float

data Visualized = Visualized { visFilled :: !(Map Cell CColor)
                             , visWidth :: !Int
                             , visHeight :: !Int
                             }
                deriving (Show, Eq)

cell :: Iso (V2 a) (V2 b) (a, a) (b, b)
cell = iso (\(V2 x y) -> (x, y)) (\(x, y) -> V2 x y)

type Solution = [Command]
