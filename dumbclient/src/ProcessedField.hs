{-# LANGUAGE TupleSections, DeriveGeneric, DeriveAnyClass #-}

module ProcessedField where

import qualified Data.Map as M
import Data.Aeson
import Linear.V2
import GHC.Generics (Generic)
import Data.Array

import Types

data PField = PField { units :: [(Cell, PUnit)] }
            deriving (Show, Eq, Generic, FromJSON)

data PUnit = PUnit { color :: CColor
                   , pivot :: Cell
                   , members :: [Cell]
                   }
           deriving (Show, Eq, Generic, FromJSON)

processedField :: PField -> ResultField
processedField field = array size pts
  where size = ((0, 0), (20, 20))
        cells = M.fromList $ concatMap (\(V2 rx ry, u) -> map (\(V2 cx cy) -> ((cx + rx, cy + ry), CellPart $ color u)) $ members u) $ units field
        pts = map (\p -> maybe (p, NotFilled) (p, ) $ M.lookup p cells) $ range size
