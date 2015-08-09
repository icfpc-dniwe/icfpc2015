module Bot where

import Data.Map (Map)
import qualified Data.Map as M

import Types
import Field

data SolTree = DeadEnd
             | Crossroad (Map Command SolTree)
             deriving (Show, Eq)

validSolutions :: Field -> SolTree
validSolutions = path []
  where path olds (Field { unit = Nothing }) = DeadEnd
        path olds f@(Field { unit = Just u }) = undefined
