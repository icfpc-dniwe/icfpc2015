{-# LANGUAGE TupleSections #-}

module Field where

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import Data.Array
import Data.Ix

import qualified Data as D
import Data (RawInput, TDirection(..), Cell(..), Unit(..))

data Field = Field { filled :: [Cell]
                   , width :: Integer
                   , height :: Integer
                   , units :: [(Cell, Unit)]
                   }

rotate :: TDirection -> Unit -> Unit
rotate CW unit = unit { members = map (\c -> Cell { x = y c, y = x c }) $ members unit }
rotate CCW unit = unit { members = map (\c -> Cell { x = y c, y = negate $ x c }) $ members unit }

toField :: RawInput -> Field
toField input = Field { filled = D.filled input
                      , width = D.width input
                      , height = D.height input
                      , units = []
                      }

data Filled = NotFilled
            | Built
            | CellPart
            deriving (Eq, Show)

type ResultField = Array (Integer, Integer) Filled

result :: Field -> ResultField
result field = array size pts
  where size = ((0, 0), (width field - 1, height field - 1))
        built = M.fromList $ map (\c -> ((x c, y c), Built)) $ filled field
        cells = M.fromList $ concatMap (\(ctr, u) -> map (\c -> ((x c + x ctr, y c + y ctr), CellPart)) $ members u) $ units field
        mpts = built `M.union` cells
        pts = map (\p -> maybe (p, NotFilled) (p, ) $ M.lookup p mpts) $ range size
