{-# LANGUAGE OverloadedStrings #-}

module Types where

import qualified Data.Vector as V
import Data.Aeson hiding (Array)
import Linear.V2
import Control.Monad
import Data.Array

type Cell = V2 Int

instance FromJSON a => FromJSON (V2 a) where
  parseJSON = withObject "Cell" $ \c -> V2 <$> c .: "x" <*> c .: "y"

data Direction = E | W | SE | SW
               deriving (Show, Eq, Ord)

data TDirection = CW | CCW
                deriving (Show, Eq, Ord)

data Command = Move Direction
             | Turn TDirection
             deriving (Show, Eq, Ord)

data CColor = CColor Float Float Float
            deriving (Show, Eq)

instance FromJSON CColor where
  parseJSON =
    withArray "CColor" $ \vs -> do
    unless (V.length vs == 3) $ fail "CColor: invalid length"
    r <- withScientific "CColor.r" (return . realToFrac) (vs V.! 0)
    g <- withScientific "CColor.g" (return . realToFrac) (vs V.! 1)
    b <- withScientific "CColor.b" (return . realToFrac) (vs V.! 2)
    return $ CColor r g b

data Filled = NotFilled
            | Built
            | CellPart CColor
            deriving (Eq, Show)

type ResultField = Array (Int, Int) Filled
