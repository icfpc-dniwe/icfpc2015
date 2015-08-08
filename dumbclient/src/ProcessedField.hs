{-# LANGUAGE TupleSections, DeriveGeneric, DeriveAnyClass #-}

module ProcessedField where

import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Aeson
import Linear.V2
import GHC.Generics (Generic)
import Data.Array

import Types

data PField = PField { units :: Set PUnit }
            deriving (Show, Eq, Generic, FromJSON)

data PUnit = PUnit { color :: CColor
                   , pivot :: Cell
                   , members :: Set Cell
                   }
           deriving (Show, Eq, Ord, Generic, FromJSON)

processedField :: PField -> ResultField
processedField field = array size pts
  where size = ((0, 0), (30, 30))
        cells = M.fromListWith merge $ concatMap doUnit $ S.toList $ units field
        conv (V2 x y) = (x, y)
        doUnit u = (conv $ pivot u, CellPart $ pure 0.1) : map (\c -> (conv c, CellPart $ color u)) (S.toList $ members u)
        pts = map (\p -> maybe (p, NotFilled) (p, ) $ M.lookup p cells) $ range size
        merge (CellPart a) (CellPart b) = CellPart ((a + b) / 2)
