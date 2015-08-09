{-# LANGUAGE TupleSections, DeriveGeneric, DeriveAnyClass #-}

module ProcessedField where

import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Aeson
import GHC.Generics (Generic)

import Types

data PField = PField { units :: Set PUnit }
            deriving (Show, Eq, Generic, FromJSON)

data PUnit = PUnit { color :: CColor
                   , pivot :: Cell
                   , members :: Set Cell
                   }
           deriving (Show, Eq, Ord, Generic, FromJSON)

processedField :: PField -> Visualized
processedField field = Visualized { visFilled = cells
                                  , visWidth = 30
                                  , visHeight = 30
                                  }
  where cells = M.fromListWith merge $ concatMap doUnit $ S.toList $ units field
        doUnit u = (pivot u, 0.3) : map (\c -> (c, color u)) (S.toList $ members u)
        merge a b = (a + b) / 2
