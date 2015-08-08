{-# LANGUAGE OverloadedStrings #-}

module Types where

import qualified Data.Vector as V
import Data.Aeson
import Linear
import Control.Monad
import qualified Data.Array as A

instance (FromJSON a) => FromJSON (V2 a) where
  parseJSON (Object c) = V2 <$> c .: "x" <*> c .: "y"
  parseJSON (Array vs) = do
    unless (V.length vs == 2) $ fail "V2: invalid length"
    x <- parseJSON (vs V.! 0)
    y <- parseJSON (vs V.! 1)
    return $ V2 x y

instance (FromJSON a) => FromJSON (V3 a) where
  parseJSON (Object c) = V3 <$> c .: "x" <*> c .: "y" <*> c .: "z"
  parseJSON (Array vs) = do
    unless (V.length vs == 3) $ fail "V3: invalid length"
    x <- parseJSON (vs V.! 0)
    y <- parseJSON (vs V.! 1)
    z <- parseJSON (vs V.! 2)
    return $ V3 x y z

type Cell = V2 Int

data Direction = E | W | SE | SW
               deriving (Show, Eq, Ord)

data TDirection = CW | CCW
                deriving (Show, Eq, Ord)

data Command = Move Direction
             | Turn TDirection
             deriving (Show, Eq, Ord)

type CColor = V3 Float

data Filled = NotFilled
            | Built
            | CellPart CColor
            deriving (Eq, Show)

type ResultField = A.Array (Int, Int) Filled
