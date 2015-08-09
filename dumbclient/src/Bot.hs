module Bot where

import Data.Map (Map)
import qualified Data.Map as M

import Types
import Field

data SolTree = DeadEnd Cell
             | Crossroad (Map Command SolTree)
             deriving (Show, Eq)

validSolutions :: Field -> SolTree
validSolutions = path []
  where path olds (Field { unit = Nothing }) = DeadEnd undefined
        path olds f@(Field { unit = Just u }) = undefined
