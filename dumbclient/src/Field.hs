{-# LANGUAGE TupleSections #-}

module Field where

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad.State
import Data.Array
import Data.Ix
import Linear.V2

import ReadWrite (Unit(..))
import qualified ReadWrite as D
import LCG
import Types

data Field = Field { filled :: [Cell]
                   , width :: Int
                   , height :: Int
                   , unitCenter :: Cell
                   , unit :: Unit
                   , availableUnits :: [Unit]
                   , sourceLength :: Int
                   , randGen :: LCG
                   }
           deriving (Show, Eq)

toField :: D.RawInput -> Int -> Field
toField input nseed =
  Field { filled = D.filled input
        , width = D.width input
        , height = D.height input
        , availableUnits = D.units input
        , unitCenter = V2 0 0
        , unit = Unit { pivot = V2 0 0, members = [] }
        , randGen = gen
        }
  where gen = LCG { seed = undefined --sourceSeeds input !! nseed
                  , modulo = 2 ^ 32
                  , mult = 1103515245
                  , incr = 12345
                  }

resultField :: Field -> ResultField
resultField field = array size pts
  where size = ((0, 0), (width field - 1, height field - 1))
        built = M.fromList $ map (\(V2 x y) -> ((x, y), Built)) $ filled field
        cells = M.fromList $ map convUnit $ members $ unit field
        convUnit p =
          let (V2 x y) = unitCenter field + p
          in ((x, y), CellPart $ CColor 1 0 0)
        mpts = built `M.union` cells
        pts = map (\p -> maybe (p, NotFilled) (p, ) $ M.lookup p mpts) $ range size

-- newtype FieldAction a = FieldAction { runFieldAction' :: State (ResultField, Field) a }
--                       deriving (Functor, Applicative, Monad)
-- 
-- runFieldAction :: FieldAction a -> Field -> (ResultField, Field, a)
-- runFieldAction comp f = (rf', f', a)
--   where ((rf', f'), a) = runState (runFieldAction' comp) (resultField f, f)
-- 
-- move 
